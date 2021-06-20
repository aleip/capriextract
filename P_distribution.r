include_P <- function(){  
  
  # Adding P to distribution
  # 1) Loading database and determining P/N ratios for manure (PMAN/NMAN), mineral fertilizer (PMIN/NMIN),
  #    crops (PRET/NRET), and crop residues (PCRD/NCRD)
  #
  #
  # 2) Loading spatial data and applying ratios to various manure, mineral fertilizer and crop positions.
  #
  # This is based on the assumption that P/N ratios are linked to the matrix (manure, crops, crop residues).
  # Only for mineral fertlizer independent application could be done by farmers, in function of soil P
  # contents, but information on this is (yet) lacking.
  
  rm(list=objects())
  source("R/initializecapri.R")
  
  impact <- c("CRESID", "SURSOI", "SURTOT", "EXCRET", "RUNANI", "IMPORT", "Impact", "APLOSS")
  fnut <- c("NITF", "PHOF", "POTF")
  # AT110000	2.87	4.81	4.81	1.94	0.06	4.81	4.81	0.03
  # AT120000	13.55	38.37	38.37	24.82	0.73	38.37	38.37	0.37
  # AT210000	4.00	12.94	12.94	8.94	0.35	12.94	12.94	0.21
  # AT220000	8.12	28.96	28.96	20.85	0.61	28.96	28.96	0.31
  # AT310000	10.83	42.50	42.50	31.68	1.79	42.50	42.50	1.35
  # AT320000	3.50	10.81	10.81	7.31	0.32	10.81	10.81	0.20
  # AT330000	3.80	11.97	11.97	8.16	0.26	11.97	11.97	0.12
  # AT340000	1.26	4.33	4.33	3.07	0.09	4.33	4.33	0.04
  
  # Extracting Nuts2 data
  
  currun <- "cur_ru_1_20210122.gms"
  curdate <- format(Sys.time(), "%Y%m%d")
  folderdate <- '20210620'
  project <- 'res_time_series_GHG'
  baseyear <- '12'
  InitCapriEnv(capri.runfile = currun, addcomment = "Extraction of base year for bruna")
  source("capdistseries_function.r")
  startextractcapdis(folderdate = '20210304')
  
  curcountrieslist <- list.files(path = savepath, pattern = paste0("res_time_series_GHG.*gdx"), full.names = FALSE)
  curcountrieslist <- unique(substr(curcountrieslist, 21, 22))
  latecountries <- c("AL", "BA", "CS", "HR", "KO", "MK", "MO")
  curcountries <- curcountrieslist[! curcountrieslist %in% c(latecountries, "TU")]
  curyears <- c(1990:2014, 2016, 2018)
  
  Pdata <- c("PMIN", "PMAN", "PCRD", "PRET", "MANP")
  Ndata <- c("NMIN", "NMAN", "NCRD", "NRET", "MANN")
  Kdata <- c("KMIN", "KMAN", "KCRD", "KRET", "MANK")
  
  caprix1 <- filtermultiple(scope="tseriesGHG", 
                           cols=c(s$maact, s$mcact, "UAAR"), 
                           rows=c(Pdata, Ndata, Kdata),
                           ydim=NULL, #curdim5=NULL, 
                           #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                           regi=s$srnuts2, 
                           curcountries=curcountries, 
                           #curcountries="BL", 
                           curyears=curyears, 
                           #curyears=curyear,
                           baseyear=baseyear, 
                           curscens="", 
                           curscensshort="") 
  save(caprix1, file = paste0("caprix1.rdata"))
  caprix1 <- caprix1[[1]]
  caprix2 <- dcast.data.table(caprix1, rall + cols + y ~ rows, value.var = 'value')
  
  matrix <- c("MIN", "MAN", "CRD", "RET", "EXC")
  pnratios <- paste0("PN_", matrix) 
  
  caprix2 <- caprix2[, `:=` (PN_MAN=PMAN/NMAN, PN_MIN=PMIN/NMIN, PN_RET=PRET/NRET, PN_CRD=PCRD/NCRD, PN_OUT=MANP/MANN)]
  capri_nutratios <- caprix2[, `:=` (KN_MAN=KMAN/NMAN, KN_MIN=KMIN/NMIN, KN_RET=KRET/NRET, KN_CRD=KCRD/NCRD, KN_OUT=MANK/MANN)]
  save(caprix2, file = paste0(savepath, "/capri_nutratios..rdata"))

  capriy1 <- filtermultiple(scope="tseriesGHG", 
                           rows=fnut, 
                           cols=impact,
                           ydim=NULL, #curdim5=NULL, 
                           #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                           regi=s$srnuts2, 
                           curcountries=curcountries, 
                           #curcountries="BL", 
                           curyears=curyears, 
                           #curyears=curyear,
                           baseyear=baseyear, 
                           curscens="", 
                           curscensshort="") 
  save(capriy1, file = paste0("capriy1.rdata"))
  
  # xlsfile <- paste0(savepath, "/res_time_series_GHG", "_4verify~", format(Sys.time(), "%Y%m%d"), ".xlsx")
  # xlswb <- createWorkbook(creator = "Adrian Leip", title = "Extraction of national GHG emissions for Verify", subject = "CAPRI Timeseries")
  # xlsxws <- addWorksheet(xlswb, sheetName = "readme")
  # writeData(xlswb, sheet="readme", startCol = 1, x = capriversion)
  # writeData(xlswb, sheet="readme", startRow = 2 + length(capriversion), x = "Units: kt of gas")
  # ghgs <- sdesc[element %in% c(ghg1, ghg2) & set=="All CAPRI rows"][, 2:3][, description:=paste0("'", description, "'")]
  # writeData(xlswb, sheet="readme", startRow = 4 + length(capriversion), x = ghgs)
  # 
  # for(p in unique(caprip$rows)){
  #   xlsws <- addWorksheet(xlswb, sheetName = p)
  #   xlsws <- writeData(xlswb, sheet = p, x = caprip[rows==p])
  # }
  # xlswf <- saveWorkbook(xlswb, file = xlsfile, overwrite = TRUE)
  
  load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20210423_kipinca/xobs_EU_2000-2018_GHGNtot_land.rdata")
  pbudgetland <- nbudgetland[, .(rall, cols, y, MANLOSSES, MINLOSSES, MMSLOSSES, NMANAP, NMANGR, NMINSL, NRET)]
  rm(nbudgetland)
  
  # Merge FSU with nuts
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  verify <- merge(verifydata, fsu_delimdata[, .(rall=fsuID, CAPRINUTS2, CNTR_NAME)], by=c("rall"))
  
  pland <- merge(pbudgetland, capri_nutratios[, .(rall, cols, y, PN_MAN, PN_MIN, PN_RET, PN_CRD, PN_OUT), by=c("rall", "cols", "y")])
  
}

