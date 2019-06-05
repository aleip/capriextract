# Load rows
#            - For emission fields
#
dims <- c("rall", "cols", "rows", "y", "ssp", "run", "n")
ocols <- function(x) {
  x <- x[, .SD, .SDcols = c(dims[dims %in% names(x)], names(x)[!names(x) %in% dims])]
}

dims <- function(x){
  
  # Determine dims that belong to the scenario description
  dims <- c("rall", "cols", "rows", "y", "ssp", "run", "n")
  a <- names(x)
  dims <- dims[dims %in% a]
  
}

getSpringmannresults <- function(subfld = NULL, flag = NULL) {
  currows <-
    c(
      "GNH3",
      "GCH4",
      "NOXMAN",
      "NOXAPP",
      "NOXGRA",
      "NOXSYN",
      #            - For various purposes
      "LEVL",
      #           - For calculation of
      "N_CAL",
      "N_PRO",
      "INCE",
       s$xx, "OILS",
      #           - For calculation of energy needs
      "ELEC",
      "EGAS",
      "EFUL"
    )
  curempty <- c(
    "",
    "N2OAMM",
    "NH3MAN",
    "NH3APP",
    "NH3GRA",
    "NH3SYN",
    "NOXMAN",
    "NOXAPP",
    "NOXGRA",
    "NOXSYN",
    "CH4ENT",
    "CH4MAN",
    "CH4RIC"
  )
  resfld <- paste0(cenv$capri, cenv$resdir, "/capmod/", subfld, "/")
  fls <-
    getfilesfromfolder(
      curfol = resfld,
      flag = flag,
      pattern = "res_2_08.*.gdx$",
      reference = NULL
    )
  capri <- filtermultiple(
    scope = "capmod_run-tarShft,5",
    cols = NULL,
    rows = currows,
    ydim = NULL,
    curdim5 = curempty,
    regi = NULL,
    curcountries = "",
    curyears = NULL,
    baseyear = '08',
    curscens = fls,
    curscensshort = flag,
    resultfile = paste0(resfld, flag, format(Sys.time(), "%Y%m%d"))
  )
  return(capri)
  
}


# Get energy densities
getenergydensities <- function() {
  #ncnc <- data.table(rgdx.param(gdxName = 'X:/dev/leipadr/results201905/capmod/chk_kcalMAgPIEfix.gdx',
  ncnc <-
    data.table(rgdx.param(
      gdxName = paste0(cenv$capri, cenv$resdir, '/capmod/chk_kcalMAgPIEfix.gdx'),
      symName = "p_NCNC_CONT",
      names = c('rows', 'nutrient')
    ))
  ncnc <- dcast.data.table(ncnc, rows ~ nutrient, value.var = "value")
  ncal <- ncnc[, .(rows, N_CAL)]
}

eatFoodGrp <- function(x = p_diet) {
  eatFood2O <-
    data.table(rgdx.set(
      gdxName = paste0(cenv$capri, cenv$resdir, '/capmod/chk_kcalEATvar.gdx'),
      symName = "eatFood2O",
      names = c('eatFoodGrp', 'rows')
    ))
  p_eatdiet <- merge(x, eatFood2O, by = "rows")
  eatFoodGrp <- as.character(unique(p_eatdiet$eatFoodGrp))
  p_eatdietintk <-
    dcast.data.table(p_eatdiet, rall + y + ssp + run + n ~ eatFoodGrp, value.var = "INTK_gPcapday", sum)
  p_eatdietintk <-
    melt.data.table(p_eatdietintk,
                    measure.vars = eatFoodGrp,
                    variable.name = "rows")
  p_eatdietintk$cols <- "INTK_gPcapday"
  
  p_eatdietkcal <-
    dcast.data.table(p_eatdiet, rall + y + ssp + run + n ~ eatFoodGrp, value.var = "ENNE_kcalPcapday", sum)
  p_eatdietkcal <-
    melt.data.table(p_eatdietkcal,
                    measure.vars = eatFoodGrp,
                    variable.name = "rows")
  p_eatdietkcal$cols <- "ENNE_kcalPcapday"
  p_eatdiet <- rbind(p_eatdietintk, p_eatdietkcal)
  p_eatdiet <- ocols(p_eatdiet)
  
}
eatRefDiet <- function(x = p_eatintk) {
  resfld <-paste0(cenv$capri, cenv$resdir, "/capmod/", subfld, "/", subfld)
  p_refeat <- Reduce(rbind, lapply(unique(x$n), function (x) {
    dt <-
      data.table(rgdx.param(
        gdxName = paste0(resfld, '_chk_kcalEATvar_', x, '.gdx'),
        symName = "p_eatTargets",
        names = c('rall', 'rows', "empty")
      ))
    dt$n <- x
    setnames(dt, "value", "ref")
    dt <- dt[empty == "gtarget", .(rall, rows, n, ref)]
  }))
  # eatFood2O <- data.table(rgdx.set(gdxName = paste0(cenv$capri, cenv$resdir, '/capmod/chk_kcalEATvar.gdx'),
  #                                  symName = "eatFood2O",
  #                                  names = c('eatFoodGrp', 'rows')))
  #
  # eatFoodGrp<- as.character(unique(eatFood2O$eatFoodGrp))
  y <- merge(x, p_refeat, by = c("rall", "rows", "n"), all=TRUE)
  y <- y[, ratio := ref / value]
  setnames(y, "value", "INTK_gPcapday")
  y <- y[, -"cols", with=FALSE]
  y <- ocols(y)
  return(y)
}

# Get intake per kcal and g
# N_CAL in kcal/cap/day
getintake <- function(x = caprid) {
  p_diet <- x[cols == "N_CAL"]
  ncal <- getenergydensities()
  p_diet <- merge(p_diet, ncal, by = "rows", all.x = TRUE)
  p_diet <- p_diet[, INTK_gPcapday := value / N_CAL * 1000]
  setnames(p_diet, "value", "ENNE_kcalPcapday")
  p_diet <- ocols(p_diet)
  return(p_diet)
}
# Get population
getinha <- function(x = caprid) {
  p_inha <- x[rows == "LEVL" & cols == "INHA" & y != 2070]
  #p_inha <- dcast.data.table(p_inha, rall + ssp ~ y, value.var="value", mean)
  p_inha <-
    unique(p_inha[, INHA := mean(value), by = .(rall, ssp, y)][, .(rall, ssp, y, INHA)])
}

getlca <- function(x = caprid, what="YILD") {
  # Get global LCA data for 'producction' (thus total emissions)
  curempty <- as.character(unique(x$empty))
  lca <- x[empty %in% setdiff(curempty, "") & cols %in% c("PROD", "YILD")]
  lca <- dcast.data.table(lca, rall + cols + rows + y + ssp + run + n~ empty, value.var = "value")
  lca[is.na(lca)] <- 0
  # Assumption: N2OAMM in Gg N2O --> convert to Gg N-N2O and --> Gg N-NH3
  # Data under 'YILD' assumed to be kg CH4/t and kg N2O/t
  # Data under 'PROD' assumed to be 1000t CH4 and 1000t N2O (or Gg CH4 and Gg N2O)
  # We use YILD here as changes in production in a country (and hence total
  # emissions) is not necessarily related to consumption pattern in the country
  # it is therefore better to use emission factors
  if(what=="YILD"){
    cat("\nExtracting LCA factors in kg per ton of product")
    lca <- lca[, NH3_kgPt := N2OAMM * 28 / 44 * 100]
    #NH3 emissions from EU supply models are not available in global LCA
    #lca <- lca[, NH3 := NH3APP + NH3GRA + NH3SYN + NH3MAN]
    lca <- lca[, NOX_kgPt := NOXAPP + NOXGRA + NOXSYN + NOXMAN]
    lca <- lca[, CH4_kgPt := CH4ENT + CH4MAN + CH4RIC]
    lca <- lca[, .(rall, cols, rows, y, ssp, run, n, NH3_kgPt, NOX_kgPt, CH4_kgPt)]
    colsPtime <- names(lca)[grepl("Pt", names(lca))]
    
  }else if(what =="PROD"){
    lca <- lca[, NH3_GgPy := N2OAMM * 28 / 44 * 100]
    #NH3 emissions from EU supply models are not available in global LCA
    #lca <- lca[, NH3 := NH3APP + NH3GRA + NH3SYN + NH3MAN]
    lca <- lca[, NOX_GgPy := NOXAPP + NOXGRA + NOXSYN + NOXMAN]
    lca <- lca[, CH4_GgPy := CH4ENT + CH4MAN + CH4RIC]
    lca <- lca[, .(rall, cols, rows, y, ssp, run, n, NH3_GgPy, NOX_GgPy, CH4_GgPy)]
    colsPtime <- names(lca)[grepl("GgPy", names(lca))]
    
  }
  
  p_diet <- getintake(x)
  p_inha <- getinha(x)
  
  
  p_diet <- merge(p_diet,
                  p_inha[, .(rall, ssp, y, INHA)],
                  by = c("rall", "y", "ssp"),
                  all.x = TRUE)
  p_diet <- p_diet[, INTK_GgPyear := INTK_gPcapday * 365 * INHA / 1000000]
  p_diet <- p_diet[, .(rall, rows, y, ssp, run, n, INTK_GgPyear)]
  
  # Merge diets and LCA emissions and
  # convert emissions to values per total calories consumed per year in the country
  p_dietemis <- merge(p_diet, lca[cols == what], by = c("rall", "rows", "y", "ssp", "run", "n"))
  p_dietemis <- p_dietemis[, -"cols", with=FALSE]
  
  idvars <- c("rows", "rall", "y", "ssp", "run", "n")
  p_dietemis <- melt.data.table(
    p_dietemis,
    id.vars = idvars,
    measure.vars = c(colsPtime, "INTK_GgPyear"),
    variable.name = "cols"
  )
  p_dietemis <- ocols(p_dietemis)
  
  return(p_dietemis)
}

getLosses <- function(x=caprid){
  
  losses <- caprid[cols %in% c("GROF", "IMPT", "HCON", "LOSMsh", "LOSCsh", "INDMsh")]
  losses <- dcast.data.table(losses, rall + rows + y + ssp + run + n ~ cols, value.var = "value")
  losses[is.na(losses)] <- 0
  
  #INTKsh: share of INTK on total consumer demand HCON
  losses <- losses[, INTKsh := 1 - LOSMsh - INDMsh - LOSCsh]
  
  # HCONsh: Share on consumer demand (HCON) on total supply (GROF + IMPT)
  losses <- losses[, HCONsh := HCON / (GROF + IMPT)]
  
  # HSHLsh: Share of households on consumer demand = intake + food waste
  losses <- losses[, HSHLsh := LOSCsh + INTKsh]
  
  # INTK2HSHL: Share of intake of total household demand (INTK + LOSC)
  losses <- losses[, INTK2HSHL := INTKsh/(INTKsh + LOSCsh)]
  losses <- losses[, INTK := HCON * INTKsh]
  losses <- losses[, LOSC := HCON * LOSCsh]
  
  eatFood2O <- geteatFood2O()
  losses <- merge(losses, eatFood2O, by="rows")
  losses <- losses[HCONsh != 0]
  
  
  householdshare <- losses[, .(rall, rows, y, ssp, run, n, HCON, HSHLsh)]
  householdshare <- householdshare[, HSHL := HCON * HSHLsh]
  householdshare <- merge(householdshare, eatFood2O, by="rows")
  hcontot <- dcast.data.table(householdshare, rall+y+ssp+run+n ~ eatFoodGrp, value.var = "HCON", sum)
  hshltot <- dcast.data.table(householdshare, rall+y+ssp+run+n ~ eatFoodGrp, value.var = "HSHL", sum)
  hcontot <- melt.data.table(hcontot, id.vars = c("rall", "y", "ssp", "run", "n"), variable.name = "rows")
  hcontot <- hcontot[, cols := "HCON"]
  hshltot <- melt.data.table(hshltot, id.vars = c("rall", "y", "ssp", "run", "n"), variable.name = "rows")
  hshltot <- hshltot[, cols := "HSHL"]
  householdshare <- dcast.data.table(rbind(hcontot, hshltot), rall+y+rows+ssp+run+n~ cols, value.var = 'value')
  householdshare <- householdshare[, HSHLsh := HSHL / HCON]
  householdshare <- householdshare[, .(rall, y, rows, ssp, run, n, HSHLsh)]
  return(losses)
}

geteatFood2O <- function(){
  eatFood2O <-  data.table(rgdx.set(
    gdxName = paste0(cenv$capri, cenv$resdir, '/capmod/chk_kcalEATvar.gdx'),
    symName = "eatFood2O",
    names = c('eatFoodGrp', 'rows')
  ))
  eatFood2O <- eatFood2O[eatFoodGrp=="MILC", rows := "MILK"]
  eatFood2O <- eatFood2O[eatFoodGrp=="OILS", rows := "OILS"]
  eatFood2O <- unique(eatFood2O)
}

avByEat <- function(x = p_dietemis, y = p_eatintk) {
  
  
  eatFood2O <- geteatFood2O()
  dt <- merge(x, eatFood2O, by = "rows")
  eatFoodGrp <- as.character(unique(eatFood2O$eatFoodGrp))
  
  #keepcols <- c("INTK_GgPyear", "NH3_kgPt", "NOX_kgPt", "CH4_kgPt")
  #dt <- dt[cols %in% keepcols]
  st <- dcast.data.table(dt, rall + y + ssp + run + n + eatFoodGrp ~ cols, 
                         value.var = "value", sum)
  setnames(st, "eatFoodGrp", "rows")
  #st$cols <- 'emis'
  st <- ocols(st)
  
  st <- merge(st, y, by=dims[!dims=="cols"], all=TRUE)
  # Food products have not been aggregated
  st <- st[!rows %in% c("ALLP", "ANIM", "CROPIN", "CROPDE")]
  # No emissions calculated for fish products
  st <- st[!rows %in% c("ACQU")]
  
  # delete country aggregates if countries exist
  st <- st[!rall %in% c("BUR", "WBA", "EU", "EU013000", "EU028000", 
                        "MED", "URUPAR","MER_OTH","NONEU_EU",
                        "ASIA","AFRICA","N_AM","MS_AM","MER",
                        "HI_INC","MID_INC","LDCACP","LDC","ACP",
                        "NONEU","World","EU_WEST","EU_EAST","EU028000")]
  
  # Check missing values
  st_na <- st[is.na(ref)]
  cat("\nCheck if there are NAs in column ref")
  print(st_na)
  return(st)
}
scale4losses <- function(x=caprid, st=p_eatemis) {
  # Scale total emissions according to the share of Intake on total household demand
  # If intake changes by factor 'ratio' then so does also total household demand
  losses <- getLosses(x=caprid)
  eatFood2O <- geteatFood2O()
  a <- dcast.data.table(losses[, .(rows, rall, y, ssp, run, n, INTK, eatFoodGrp)],
                        rall + y + ssp + run + n ~ eatFoodGrp,
                        value.var = c("INTK"), sum)
  a <- a[, cols:='INTK']
  a <- melt.data.table(a, id.vars = c("rall", "cols","y", "ssp", "run", "n"), variable.name = 'rows')
  b <- dcast.data.table(losses[, .(rows, rall, y, ssp, run, n, LOSC, eatFoodGrp)],
                        rall + y + ssp + run + n ~ eatFoodGrp,
                        value.var = c("LOSC"), sum)
  b <- b[, cols:='LOSC']
  b <- melt.data.table(b, id.vars = c("rall", "cols","y", "ssp", "run", "n"), variable.name = 'rows')
  c <- dcast.data.table(rbind(a, b), rall + rows + y + ssp + run + n ~ cols, value.var = 'value')
  # Share of Intake on total houshold demand (intake + food waste)
  eastIntk2Hshl <- c[, INTK2HSHL := INTK / (INTK + LOSC)]

  # as we assume that food waste remains stable.
  # HSHLsh increases therefore from x to x*ratio, but the total changes as well
  # from 100% to (100 + ratio-1)%
  tt <- merge(st, eastIntk2Hshl[,.(rall, rows, y, ssp, run, n, INTK2HSHL)], by=c("rall", "rows", "y", "ssp", "run", "n"))
  
  #Emissions linked to household demand are calculated from
  #Emissions per t of product * intake divided (scaled) to total household demand
  # Units: kg N/t food * Gg food/y = Mg N/y --> divide by 1000 to obtain Gg N/y
  tt <- tt[, `:=` (NH3hshl_GgPy = 0.001 * NH3_kgPt * INTK_GgPyear / INTK2HSHL,
                   NOXhshl_GgPy = 0.001 * NOX_kgPt * INTK_GgPyear / INTK2HSHL,
                   CH4hshl_GgPy = 0.001 * CH4_kgPt * INTK_GgPyear / INTK2HSHL)]
  
  # Scale only shocked scenarios to the target diets from EAT
  # - the un-shocked keep their emissions
  tt <- tt[run=='1', `:=` (NH3hshlfin_GgPy = NH3hshl_GgPy * ratio, # Note that IS scaled to target
                           NOXhshlfin_GgPy = NOXhshl_GgPy * ratio,
                           CH4hshlfin_GgPy = CH4hshl_GgPy * ratio)]
  tt <- tt[run=='0', `:=` (NH3hshlfin_GgPy = NH3hshl_GgPy, # Note that this is NOT scaled to target
                           NOXhshlfin_GgPy = NOXhshl_GgPy,
                           CH4hshlfin_GgPy = CH4hshl_GgPy)]
  
  
  p_eatemis <- tt[, `:=` (NH3diff_GgPy = NH3hshlfin_GgPy - NH3hshl_GgPy,
                          NOXdiff_GgPy = NOXhshlfin_GgPy - NOXhshl_GgPy,
                          CH4diff_GgPy = CH4hshlfin_GgPy - CH4hshl_GgPy)]
  
  return(p_eatemis)
}
