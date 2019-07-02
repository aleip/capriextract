# Load rows
#            - For emission fields
#
dims <- c("rall", "cols", "rows", "y", "ssp", "run", "n")
ocols <- function(x) {
  c <- dims(x)
  x <- x[, .SD, .SDcols = c(c, names(x)[!names(x) %in% c])]
}

dims <- function(x){
  
  # Determine dims that belong to the scenario description
  dims <- c("rall", "cols", "rows", "y", "ssp", "run", "n")
  a <- names(x)
  dims <- dims[dims %in% a]
  return(dims)
  
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
  pp <- p_eatdiet[, .(INTK_gPcapday = sum(INTK_gPcapday, na.rm=TRUE),
                      ENNE_kcalPcapday = sum(ENNE_kcalPcapday, na.rm=TRUE)),
                  by=c("rall", "y", "ssp", "run", "n", "eatFoodGrp")]
  setnames(pp, "eatFoodGrp", "rows")
  pp <- ocols(pp)
  
}
eatRefDiet <- function(pp=p_eatdiet) {
  
  flstart <- subfld
  if(subfld == "20190602_Springmann18_used4sendingdata") flstart <- "20190602_Springmann18"
  resfld <-paste0(cenv$capri, cenv$resdir, "/capmod/", subfld, "/", flstart)
  
  # Attention!! the first four are from baseline
  ns <- sort(unique(pp$n))
  bn <- 0
  if(length(ns) > 8){
    bn <- max(ns-8)
    ns <- 1:8
  }
  
  p_refeat <- Reduce(rbind, lapply(ns, function (x) {
    
    dt <-
      data.table(rgdx.param(
        gdxName = paste0(resfld, '_chk_kcalEATvar_', x, '.gdx'),
        symName = "p_eatTargets",
        names = c('rall', 'rows', "empty")
      ))
    dt$n <- x + bn
    setnames(dt, "value", "ref")
    dt <- dt[empty == "gtarget", .(rall, rows, n, ref)]
  }))
  # eatFood2O <- data.table(rgdx.set(gdxName = paste0(cenv$capri, cenv$resdir, '/capmod/chk_kcalEATvar.gdx'),
  #                                  symName = "eatFood2O",
  #                                  names = c('eatFoodGrp', 'rows')))
  #
  # eatFoodGrp<- as.character(unique(eatFood2O$eatFoodGrp))
  y <- merge(pp, p_refeat, by = c("rall", "rows", "n"), all=TRUE)
  y <- y[, ratio := ref / INTK_gPcapday]
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

getlca <- function(x = caprid) {
  # Get global LCA data for 'producction' (thus total emissions)
  curempty <- as.character(unique(x$empty))
  #lca <- x[empty != "" & cols %in% c("PROD")]
  # Assumption: N2OAMM in Gg N2O --> convert to Gg N-N2O and --> Gg N-NH3
  # Data under 'YILD' assumed to be kg CH4/t and kg N2O/t
  # Data under 'PROD' assumed to be 1000t CH4 and 1000t N2O (or Gg CH4 and Gg N2O)
  # We use YILD here as changes in production in a country (and hence total
  # emissions) is not necessarily related to consumption pattern in the country
  # it is therefore better to use emission factors
  #if(what=="YILD"){
    cat("\nExtracting LCA factors in kg per ton of product")
    lcayild <- x[empty != "" & cols %in% c("YILD")]
    lcayild <- dcast.data.table(lcayild, rall + cols + rows + y + ssp + run + n~ empty, value.var = "value")
    #grof <- x[empty == "" & cols == "GROF"]
    #grof <- grof[,.(rall, rows, y, ssp, run, n, value)]
    #setnames(grof, "value", "GROF")
    lcayild[is.na(lcayild)] <- 0
    lcayild <- lcayild[, NH3_kgPt := N2OAMM * 28 / 44 * 100]
    #NH3 emissions from EU supply models are not available in global LCA
    #lca <- lca[, NH3 := NH3APP + NH3GRA + NH3SYN + NH3MAN]
    lcayild <- lcayild[, NOX_kgPt := NOXAPP + NOXGRA + NOXSYN + NOXMAN]
    lcayild <- lcayild[, CH4_kgPt := CH4ENT + CH4MAN + CH4RIC]
    lcayild <- lcayild[, .(rall, cols, rows, y, ssp, run, n, NH3_kgPt, NOX_kgPt, CH4_kgPt)]
    colsPtime <- names(lcayild)[grepl("Pt", names(lcayild))]
    
  #}else if(what =="PROD"){
    lcaprod <- x[empty != "" & cols %in% c("PROD")]
    lcaprod <- dcast.data.table(lcaprod, rall + cols + rows + y + ssp + run + n~ empty, value.var = "value")
    #grof <- x[empty == "" & cols == "GROF"]
    #grof <- grof[,.(rall, rows, y, ssp, run, n, value)]
    #setnames(grof, "value", "GROF")
    lcaprod[is.na(lcaprod)] <- 0
    lcaprod <- lcaprod[, NH3_GgPy := N2OAMM * 28 / 44 * 100]
    #NH3 emissions from EU supply models are not available in global lcaprod
    #lcaprod <- lcaprod[, NH3 := NH3APP + NH3GRA + NH3SYN + NH3MAN]
    lcaprod <- lcaprod[, NOX_GgPy := NOXAPP + NOXGRA + NOXSYN + NOXMAN]
    lcaprod <- lcaprod[, CH4_GgPy := CH4ENT + CH4MAN + CH4RIC]
    lcaprod <- lcaprod[, .(rall, cols, rows, y, ssp, run, n, NH3_GgPy, NOX_GgPy, CH4_GgPy)]
    colsPtime <- names(lcaprod)[grepl("GgPy", names(lcaprod))]
    
  #}
  lca <- merge(lcaprod[,-'cols', with=FALSE], lcayild[,-'cols', with=FALSE], by=dims(lcaprod[,-'cols', with=FALSE]))
  lca <- lca[, GROF := 1000*NH3_GgPy/NH3_kgPt]
  return(lca)
}
  
mergeLCAwithDiets <- function(x=caprid, lca=p_eatemis, y=p_eatintk){
  p_inha <- getinha(x)
  
  lcax <- merge(lca, y, by=dims(lca))
  lcax <- merge(lcax, p_inha, by=dims(p_inha))
  
  lcax <- lcax[, `:=` (INTK_GgPyear = INTK_gPcapday * 365 * INHA / 1000000,
                       ref_GgPyear = ref * 365 * INHA / 1000000)]
  
  st <- lcax[, .(rall, y, ssp, rows, run, n, GROF, INHA, 
               NH3_GgPy, NH3_kgPt, NOX_GgPy, NOX_kgPt, CH4_GgPy, CH4_kgPt,
               ENNE_kcalPcapday, INTK_gPcapday, ref, ratio,
               INTK_GgPyear, ref_GgPyear)]
  
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
  losses <- losses[, HCOM := HCON * (1 - LOSCsh - INDMsh)]
  
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
  #eatFood2O <- eatFood2O[eatFoodGrp=="MILC", rows := "MILK"]
  #eatFood2O <- eatFood2O[eatFoodGrp=="OILS", rows := "OILS"]
  oils <- eatFood2O[rows=="OTHO"]
  oils <- oils[, rows := "OILS"]
  eatFood2O <- rbind(eatFood2O, oils)
  return(eatFood2O)
  #eatFood2O <- unique(eatFood2O)
}

avByEat <- function(x=caprid, z = p_dietemis) {
  
  
  eatFood2O <- geteatFood2O()
  dt <- merge(z, eatFood2O, by = "rows")
  eatFoodGrp <- as.character(unique(eatFood2O$eatFoodGrp))
  
  #keepcols <- c("INTK_GgPyear", "NH3_kgPt", "NOX_kgPt", "CH4_kgPt")
  #dt <- dt[cols %in% keepcols]
  # st <- dcast.data.table(dt, rall + y + ssp + run + n ~ eatFoodGrp + GROF, 
  #                        value.var = "value", sum)
  sumcols <- c("NH3_GgPy", "NOX_GgPy", "CH4_GgPy", "NH3_kgPt", "NOX_kgPt", "CH4_kgPt", "GROF")
  st <- dt[, lapply(.SD, sum, na.rm=TRUE), by=.(rall, y, ssp, run, n, eatFoodGrp), .SDcols = sumcols]
  setnames(st, "eatFoodGrp", "rows")
  #st$cols <- 'emis'
  st <- ocols(st)
  st <- st[, `:=`(
    NH3_kgPt = 1000 * NH3_GgPy/GROF,
    NOX_kgPt = 1000 * NOX_GgPy/GROF,
    CH4_kgPt = 1000 * CH4_GgPy/GROF
  )]
  
  
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
  return(tt)
  
  ##STOP here this function as the adjustment is not proceeded here. 
  ##    Instead we use the emissions fields from the leakage module direclty
  
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
  
  
  xx <- tt[, `:=` (NH3diff_GgPy = NH3hshlfin_GgPy - NH3hshl_GgPy,
                          NOXdiff_GgPy = NOXhshlfin_GgPy - NOXhshl_GgPy,
                          CH4diff_GgPy = CH4hshlfin_GgPy - CH4hshl_GgPy)]
  
  xx <- xx[, `:=` (NH3adj_GgPy = ( NH3_GgPy + NH3diff_GgPy),
                                 NOXadj_GgPy = ( NOX_GgPy + NOXdiff_GgPy),
                                 CH4adj_GgPy = ( CH4_GgPy + CH4diff_GgPy))]
  
  return(xx)
}

GlobalPoolMarket <- function(x=caprid, xx = p_eatemisscaled){
  
  # Get global market balance.
  # In checks theses seem to be the positions. 
  # include BIOF
  smktbal <- c("IMPORTS", "EXPORTS", "HCON", "PROD", "PROC", "FEED", "YILD", "BIOF")
  mktbal <- dcast.data.table(caprid[cols %in%  smktbal & empty == ""],
                             rall + rows + y + ssp + run ~ cols, value.var="value")
  
  
  eatFood2O <- geteatFood2O()
  mktbal <- merge(mktbal, eatFood2O, by="rows")
  mktbal <- mktbal[, lapply(.SD, sum, na.rm=TRUE), by=.(rall, y, ssp, run, eatFoodGrp), .SDcols=smktbal]
  setnames(mktbal, "eatFoodGrp", "rows")
  mktbal[is.na(mktbal)] <- 0
  mktbal <- mktbal[, totIN := IMPORTS + PROD]
  mktbal <- mktbal[, totEX := BIOF+HCON+PROC+FEED+EXPORTS]
  
  # a) we have the global market balance. 
  # for each food group we can calculate the share of import vs total supply in region r
  # IMPTshr := IMPORTS / (IMPORTS + PROD)
  mktbal <- mktbal[, IMPTsh := IMPORTS / (IMPORTS + PROD)]
  
  mm1 <- merge(xx, mktbal, by=c("rall", "y", "ssp", "run", "rows"))
  
  # b) we know domestic emissions from a food NH3_GgPy and the by 
  # how much the consumption must be changed: 
  # x (to go from INTK_capri -> INTK_target; xr = (INTK_target-INTK_capri)/INTK_capri)
  
  mm1 <- mm1[, scale_INTK := (ref - INTK_gPcapday)/INTK_gPcapday]
  
   
  # c) crop products are used for hcom, feed, exports and proc. we calculate the share of HCOM and FEED
  # HCONshr := HCON / (HCON + FEED + EXPORT + PROC)
  # FEEDshr := FEED / (HCON + FEED + EXPORT + PROC)
  losses <- getLosses(x)
  losses <- losses[, c(dims(losses), "HCOM", "eatFoodGrp"), with=FALSE]
  losses <- losses[, .(HCOM = sum(HCOM, na.rm = TRUE)), by=c("rall", "y", "ssp", "run", "eatFoodGrp", "n")]
  setnames(losses, "eatFoodGrp", "rows")  
   
  mm1 <- merge(mm1, losses, by = dims(losses))
  #mm1 <- mm1[, HCOM := HCON * (1 - LOSMsh - INDMsh)]
  mm1 <- mm1[, HCOMsh := HCOM / totEX]
  mm1 <- mm1[, FEEDsh := FEED / totEX]
  
  # c) we assume that import shares remain constant, thus we can adjust the domestic emissions
  # so we assume that only  (1-IMPTshr) * x comes from domestic production and has an effect on domestic emissions
  # the share of emissions to be scaled is the one relating to HCOM
  # DELTAhcom_NH3r_GgPy = NH3_GgPy * (1-IMPTshr) * xr * HCONsh
  
  mm2 <- mm1[, DELTAhcom := (1-IMPTsh) * scale_INTK * HCOMsh]
  mm2 <- mm2[scale_INTK>100, DELTAhcom := (1-IMPTsh) * ref_GgPyear/totEX ]
  
  # d) we sum all 'imported intake changes' globally over all regions r
  # HCOMimpt_globalchange = SUMr (IMPTsh * scale_INTK * HCOM)
  # HCOMtot = SUMr(HCOM)
  
  mm2 <- mm2[, HCOMimpt_change := IMPTsh * scale_INTK * HCOM]
  
  # Special treatment of cases where scale_INTK == inf 
  # because no CAPRI intake is defined. 
  #   scale_INTK := (ref - INTK_gPcapday)/INTK_gPcapday
  # This is because INTK_gPcapday is zero
  # IMPTsh can have values between [0-1] or NaN (if no production or import)
  #   ---> All additional intake must be imported
  #        Take relation  INTK_GgPyear/INTK_gPcapday
  mm2 <- mm2[scale_INTK>100, HCOMimpt_change := ref_GgPyear]
  
  
  # Sum over all countries. The result should give the total 
  # of additional (or less) of a product that is taken
  # from the global market pool
  HCOMimpt_changeglob <- mm2[, .(HCOMimpt_changeglob=sum(HCOMimpt_change, na.rm=TRUE)),
                by=c("y", "ssp", "run", "rows")]
  
  # Sum total production 
  PRODtot <- mm2[, .(PRODtot = sum(PROD, na.rm = TRUE)), by=c("y", "ssp", "run", "rows")]

  # ... and calculate the relative change that this will cause in total production
  # that the global market must deliver
  # DELTA_hcomimpt = INTK_globalchange / HCOMtot
  
  # DELTAhcomimpt give the relative changes on total production that the
  # changing demand of imported foods imply
  mm3 <- merge(HCOMimpt_changeglob, PRODtot, by=c("y", "ssp", "run", "rows"))[,
               DELTAhcomimpt := HCOMimpt_changeglob / PRODtot]
  mm3 <- merge(mm2, mm3, by=dims(mm3))
    
  # d) we know how much feed is used, but we don't know (globally) which feed is used for which animal product.
  # we assume that the same IMPTsh applies also to feed, but we don't have a direct link to consumption.
  # therefore we scale feed only globally. 
  # to do so, we first need to have an idea of the change of demand of ALL animal products
  # changeINTK_lvst = SUM_{r,y} (INTK_target_ry-INTK_capri_ry) / SUM_{r,y} (INTK_capri_ry)
  # y in {red meat, poultry+eggs, milc}
  # 
  # Note that in particular MILC intake in CAPRI stays below the reference diets
  # Therefore this gives overall the need for INCREAS

  sum_lvst <- mm3[rows %in% c("rmea", "POUL", "MILC"), 
                     .(sumRef_lvst = sum(ref_GgPyear, na.rm=TRUE),
                       sumCAPRI_lvst = sum(INTK_GgPyear, na.rm=TRUE)),
                     by=c("rows","ssp", "run", "y")]
  
  sum_lvstall <- sum_lvst[, .(sumRef_lvst = sum(sumRef_lvst),
                             sumCAPRI_lvst = sum(sumCAPRI_lvst)),
                         by=c("ssp", "run", "y")]
  
  # scale_lvst gives the factor with which marketable feed production must
  # change to meet the demand for the EAT reference diets.
  # It gives only one factor for all livestock products and feed items
  # thus ignoring that shifts between livestock products might require
  # different feed items. This is because we do not have (at this point)
  # the information which feed is used for which product. 
  # Even with this knowlege the this link would change with changing 
  # demands through economic effects - which we ignore anyway in this
  # post-processing.
  sum_lvstall <- sum_lvstall[, scale_lvst := sumRef_lvst / sumCAPRI_lvst]

  # e) changes in feed emissions are allocated globally assuming that 
  # the relative contribution of countries to global feed production remains the same
  # DELTAfeed_NH3r_GgPy = NH3r_GgPy * FEEDsh * changeINTK_lvs
  
  # If feed demand changes by scale_lvst, then total production must change 
  mm4 <- merge(mm3, sum_lvstall[,.(ssp, run, y, scale_lvst)], by=c("ssp", "run", "y"))
  mm4 <- mm4[, DELTAfeed := FEEDsh * (scale_lvst-1)]
  # 
  # 
  # Adding up all DELTAS required for production
  mm4 <- mm4[, DELTAtot := DELTAhcom + DELTAhcomimpt + DELTAfeed]
   
  
  return(mm4)
}
adjustEmissions <- function(x = caprid, mm4 = p_eatglobm){
  
  # f) we adjust the emissions
  # NH3_adjusted = NH3r_GgPy  + DELTAhcom_NH3r_GgPy  + DELTA_hcomimpt_NH3r_GgPy  + DELTAfeed_NH3r_GgPy 
  
  nh3cols <- names(mm4)[grepl("NH3", names(mm4))]
  ch4cols <- names(mm4)[grepl("CH4", names(mm4))]
  noxcols <- names(mm4)[grepl("NOX", names(mm4))]
  emicols <- c(nh3cols, ch4cols, noxcols)
  deltacl <- names(mm4)[grepl("DELTA", names(mm4))]
  
  ## Apply changes on production in the countries 
  ## to the emissions 
  mm4[, `:=` (NH3adj_GgPy = NH3_GgPy * ( 1 + DELTAtot),
              NOXadj_GgPy = NOX_GgPy * ( 1 + DELTAtot),
              CH4adj_GgPy = CH4_GgPy * ( 1 + DELTAtot))]
  
  nh3cols <- names(mm4)[grepl("NH3", names(mm4))]
  ch4cols <- names(mm4)[grepl("CH4", names(mm4))]
  noxcols <- names(mm4)[grepl("NOX", names(mm4))]
  emicols <- c(nh3cols, ch4cols, noxcols)
  smktbal <- c("IMPORTS", "EXPORTS", "HCON", "PROD", "PROC", "FEED", "YILD", "BIOF", "HCOM")
  colorder <- c(dims(mm4), smktbal, "totIN", "totEX", "GROF", "INHA",  
                names(mm4)[grepl("sh", names(mm4))], 
                "ENNE_kcalPcapday", "INTK_gPcapday", "ref", "ratio" ,
                "INTK_GgPyear", "ref_GgPyear",
                "INTK2HSHL", "scale_INTK", "HCOMimpt_change", "HCOMimpt_changeglob", "scale_lvst",
                deltacl, emicols)
  mm4 <- mm4[, .SD, .SDcols = colorder]  
  return(mm4)
}
  

writeresults <- function(p_emis4fasst = p_emis4fasst,
                         p_eatintk = p_eatintk
                         ){
  manuscript <- "x:/adrian/google/literature/manuscripts/springmann_costunhealthydiet/"
 
  p_emis4fasstglobal <- p_eatemisfin[, .SD, .SDcols = c("rall", "rows", "y", "ssp", "run", cols2sum)]
  p_emis4fasstglobal <- p_emis4fasstglobal[, lapply(.SD, sum, na.rm=TRUE), by=.(y, ssp, rows, run), 
                                           .SDcols = names(p_emis4fasstglobal)[!names(p_emis4fasstglobal) %in% c("rall", "rows", "y", "ssp", "run")]]
  write.csv(p_emis4fasstglobal, file=paste0(manuscript, "p_emis4fasstglobal", format(Sys.time(), "%Y%m%d"), ".csv"))
  
  p_diet4healthmodel <- dcast.data.table(p_eatintk[!is.na(run), -"ratio", with=FALSE], rall +rows + y + ssp ~ run,
                                         value.var=c("INTK_gPcapday", "ref"))
  p_diet4healthmodel[is.na(p_diet4healthmodel)] <- 0
  save(p_emis4fasst, p_diet4healthmodel, p_eatintk, p_eatemisscaled, 
       file=paste0(manuscript, "capriresults", format(Sys.time(), "%Y%m%d"), ".rdata"))
  method <- paste0("\n#        Diets are 'shocked' by calculating intake values (g/cap/day) based on Springmann et al.(2018).",
                   "\n#        Shocks are applied to each of 12 'EAT food groups' ",
                   "\n#                Vegetable foods: cereals - starch crops - fruits - nuts and seeds - dry pulses - vegetables - sugar - oils",
                   "\n#                Animal foods:    dairy - read meat - poultry meat and eggs - fish and shellfish",
                   "\n#        The 'resulting' diets in CAPRI are not exactly matching the target diets. ",
                   "\n#        Therefore footprints are calculated from the unshocked and shocked world",
                   "\n#        and total emissions are assigned to the country of production",
                   "\n#")
  con <- file(paste0(manuscript, "capripollutants4fasst", format(Sys.time(), "%Y%m%d"), ".csv"), open="wt")
  writeLines(paste0("#Extract of NH3 NOx and CH4 total emissions calculated on the basis of CAPRI runs with different diets",
                    "\n#Method: Footprints are calculated with CAPRI by constraining total calories consumption to about 2100 kcal.",
                    method,
                    "\n# Emissions are calculated using the CAPRI 'leakage' module. Emissions remain in the country where they occur",
                    "\n# Emissions changes are split into those that are assumed to occur domestically ",
                    "\n# and those that are assumed to come from the 'global market' using import shares.",
                    "\n# The 'global market' is assumed to deliver food to one 'pool market' from which all imports",
                    "\n# are derived (approach followed in the CAPRI LCA).",
                    "\n# Thus all productions are scaled acc to the ratio of the global trade changes to total global production.",
                    "\n# ",
                    "\n# Emissions from marketable feed is not included in the factors for livestock products ",
                    "\n# but in the total emissions of the crops. The total global changes in feed demand is calculated",
                    "\n# assuming constant share of countries to produce feed and ignoring a possible shift in share of different feeds.",
                    "\n# Emissions are adjusted accordingly.",
                    "\n# ",
                    "\n# NH3_GgPy: Emissions of NH3-N [Gg NH3-N y-1]. ",
                    "\n# NOX_GgPy: Emissions of NOx-N [Gg NOx-N y-1]. Available only for CAPRI supply models (Europe). ",
                    "\n# CH4_GgPy: Emissions of CH4 [Gg CH4 y-1]. ",
                    "\n#",
                    "\n# *adj: Emissions as defined above but changes due to scaled to target diets are included.",
                    "\n#       To this purpose the footprints from the 'shocked' runs are scaled to match the target intake values.",
                    "\n#       The 'unshocked' data are not scaled.",
                    "\n#",
                    "\n# *_0: Results from un-shocked CAPRI runs only constraining total calories intake to 2100 kcal/cap/dat",
                    "\n# *_1: Results from shocked CAPRI runs - footprints are scaled.",
                    "\n#"
  ), con)
  write.csv(p_emis4fasst, con)
  close(con)
  con <- file(paste0(manuscript, "capridiet4healthmodel", format(Sys.time(), "%Y%m%d"), ".csv"), open="wt")
  writeLines(paste0("#Extract of average diets calculated on the basis of CAPRI and EAT target diets",
                    "\n#Method: Total calories consumption are constrained in all simulations to about 2100 kcal.",
                    method,
                    "\n# INTK_gPcapday: Intake of food group [g / cap / day] as simulated with CAPRI",
                    "\n# ref: Intake of food group [g / cap / day] in the target diet",
                    "\n#",
                    "\n# *_0: Results from un-shocked CAPRI runs only constraining total calories intake to 2100 kcal/cap/dat",
                    "\n# *_1: Results from shocked CAPRI runs - footprints are scaled.",
                    "\n#"
  ), con)
  write.csv(p_diet4healthmodel, con)
  close(con)
  write.csv(p_eatemisfin[, .SD, .SDcols = c(dims(p_eatemisfin), 
                                            names(p_eatemisfin)[grepl("DELTA", names(p_eatemisfin))])], 
            file=paste0(manuscript, "p_eatemisfin_delta", format(Sys.time(), "%Y%m%d"), ".csv"))
  
  return(p_diet4healthmodel)
}


checkresults <- function(p=p_eatemisfin){
  require(ggplot2)
  library(RColorBrewer)
  
  manuscript <- "x:/adrian/google/literature/manuscripts/springmann_costunhealthydiet/"
  p <- p[, NH3_kgPcap := NH3_GgPy/INHA * 1000 ]
  p <- p[, NH3adj_kgPcap := NH3adj_GgPy/INHA * 1000]
  
  jpeg(filename=paste0(manuscript, "NH3_adjusted.vs.non-adjusted.jpg"), 
      width = 1000, height = 1000, quality = 600)
  plot(y=p$NH3adj_GgPinha, x=p$NH3_GgPinha)  
  plot(y=p[run==0]$NH3adj_kgPcap, x=p[run==0]$NH3_kgPcap, pch=21, bg = "red", cex=3)  
  points(y=p[run==1]$NH3adj_kgPcap, x=p[run==1]$NH3_kgPcap, pch=21, bg = "blue", cex=3)  
  dev.off()
  p <- p[, ssp_y := paste0(ssp, "-", y)]
  p <- p[, refdiet_g_per_cap_day := ref]
  p <- p[, CH4_kgPcap := CH4_GgPy/INHA * 1000 ]
  p <- p[, CH4adj_kgPcap := CH4adj_GgPy/INHA * 1000]
  jpeg(filename=paste0(manuscript, "CH4_adjusted.vs.non-adjusted.jpg"), 
      width = 1000, height = 1000, quality = 600)
  plot(y=p[run==0]$CH4adj_kgPcap, x=p[run==0]$CH4_kgPcap, pch=21, bg = "red", cex=3)  
  points(y=p[run==1]$CH4adj_kgPcap, x=p[run==1]$CH4_kgPcap, pch=21, bg = "blue", cex=3)  
  dev.off()
  
  
  pdf(file=paste0(manuscript, "NH3_adjusted.vs.non-adjusted.pdf"))
  a <- ggplot(p, aes_string(x=p$NH3_kgPcap, y="NH3adj_kgPcap"))
  a <- a + 
    scale_colour_manual(name="CAPRI simulation \n(non-shocked / shocked", 
                        values = c("0"="red", "1"="blue")) +
    scale_shape_manual(values=c(15:18))+ 
    geom_point(alpha = 0.6,
               aes(colour = factor(run), 
                   fill = factor(run),
                   size=ref, 
                   shape = interaction(ssp,y))) 
  print(a)
  dev.off()
  
  pdf(file=paste0(manuscript, "NH3_adjusted.vs.non-adjusted_food.pdf"))
  a <- ggplot(p, aes_string(x=p$NH3_kgPcap, y="NH3adj_kgPcap"))
  a <- a + 
    geom_point(alpha = 0.6,
               aes(colour = factor(rows), 
                   fill = factor(rows),
                   size=ref, 
                   shape = interaction(ssp,y)))+ 
    #scale_fill_brewer(aesthetics = c("colour", "fill"), type = "qual") +
    scale_shape_manual(values=c(15:18)) 
  a
  print(a)
  dev.off()
  
  h <- p[ratio < 100]
  a <- ggplot(h, aes_string(x=rows, y=ratio))
  hist(p[ratio<100]$ratio, breaks = seq(0, 100, 0.5))
  
  e <- p[, .SD, .SDcols = c(dims(p), "NH3_GgPy", "NH3adj_GgPy")]
  e <- e[run==1, NH3_GgPy := NH3adj_GgPy]
  e <- dcast.data.table(e, rall + rows + y + ssp ~ run, value.var = "NH3_GgPy")  
  e <- e[, reduction := `1` / `0`]
  e <- e[reduction < 100]
  e <- e[, diff := `1` - `0`]
    
  pe <- ggplot(e, aes_string(x=e$`0`, y="diff"))
  pe <- pe + 
    geom_point(alpha = 0.3,
               aes(colour = factor(rows), 
                   fill = factor(rows),
                   shape = interaction(ssp,y))) 
  pe <- pe + scale_x_log10() 
  pe

}

