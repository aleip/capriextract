# Load rows
#            - For emission fields
#
dims <- c("rall", "cols", "rows", "y", "ssp", "run", "n")
ocols <- function(x) {
  x <- x[, .SD, .SDcols = c(dims[dims %in% names(x)], names(x)[!names(x) %in% dims])]
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
  lca <-
    x[empty %in% setdiff(curempty, "") & cols %in% c("PROD", "YILD")]
  lca <-
    dcast.data.table(lca, rall + cols + rows + y + ssp + run ~ empty, value.var = "value")
  lca[is.na(lca)] <- 0
  # Assumption: N2OAMM in Gg N2O --> convert to Gg N-N2O and --> Gg N-NH3
  # Data under 'YILD' assumed to be kg CH4/t and kg N2O/t
  # Data under 'PROD' assumed to be 1000t CH4 and 1000t N2O (or Gg CH4 and Gg N2O)
  lca <- lca[, NH3tot := N2OAMM * 28 / 44 * 100]
  lca <- lca[, NH3 := NH3APP + NH3GRA + NH3SYN + NH3MAN]
  lca <- lca[, NOX := NOXAPP + NOXGRA + NOXSYN + NOXMAN]
  lca <- lca[, CH4 := CH4ENT + CH4MAN + CH4RIC]
  lca <-
    lca[, .(rall, cols, rows, y, ssp, run, NH3tot, NH3, NOX, CH4)]
  
  
  p_diet <- getintake(x)
  p_inha <- getinha(x)
  
  
  p_diet <-
    merge(p_diet,
          p_inha[, .(rall, ssp, y, INHA)],
          by = c("rall", "y", "ssp"),
          all.x = TRUE)
  p_diet <-
    p_diet[, ENNE_GcalPyear := ENNE_kcalPcapday * 365 * INHA / 1000000]
  
  # Merge diets and LCA emissions and
  # convert emissions to values per total calories consumed per year in the country
  p_dietemis <-
    merge(p_diet, lca[cols == "PROD"], by = c("rall", "rows", "y", "ssp", "run"))
  
  # We assume emissions in GgPyear
  p_dietemis <-
    p_dietemis[, NH3totkgperkcalyear := NH3tot / ENNE_GcalPyear]
  p_dietemis <-
    p_dietemis[, NH3kgperkcalyear := NH3 / ENNE_GcalPyear]
  p_dietemis <-
    p_dietemis[, NOXkgperkcalyear := NOX / ENNE_GcalPyear]
  p_dietemis <- p_dietemis[, CH4perkcalyear := CH4 / ENNE_GcalPyear]
  
  colsPtime <-
    names(p_dietemis)[grepl("Pcap|per|Pyear", names(p_dietemis))]
  colsAbs <- c("NH3tot", "NH3", "NOX", "CH4")
  idvars <- c("rows", "rall", "y", "ssp", "run", "n")
  p_dietemis <- melt.data.table(
    p_dietemis,
    id.vars = idvars,
    measure.vars = c(colsPtime, colsAbs),
    variable.name = "cols"
  )
  p_dietemis <- ocols(p_dietemis)
  return(p_dietemis)
}

avByEat <- function(x = p_dietemis) {
  eatFood2O <-
    data.table(rgdx.set(
      gdxName = paste0(cenv$capri, cenv$resdir, '/capmod/chk_kcalEATvar.gdx'),
      symName = "eatFood2O",
      names = c('eatFoodGrp', 'rows')
    ))
  eatFood2O <- eatFood2O[eatFoodGrp=="MILC", rows := "MILK"]
  eatFood2O <- eatFood2O[eatFoodGrp=="OILS", rows := "OILS"]
  eatFood2O <- unique(eatFood2O)
  
  dt <- merge(x, eatFood2O, by = "rows", all=TRUE)

  eatFoodGrp <- as.character(unique(eatFood2O$eatFoodGrp))
  
  keepcols <- c("INTK_gPcapday", "NH3tot", "NH3", "NOX", "CH4")
  dt <- dt[cols %in% keepcols]
  st <-
    dcast.data.table(dt, rall + y + ssp + run + n + eatFoodGrp ~ cols, value.var =
                       "value", sum)
  setnames(st, "eatFoodGrp", "rows")
  st$cols <- 'emis'
  st <- ocols(st)
  return(st)
}
