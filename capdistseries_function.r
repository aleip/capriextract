startextractcapdis <- function(){
  
  #' Extraction of Nitrogen surplus data at HSU level for the KIP-INCA project
  #'
  #' Extracts relevant elements for the Gross Nitrogen Surplus by crop at the 
  #' spatial levle of Spatial Homogeneous Units (HSU) and writes them (i) into 
  #' rdata-files and (ii) into csv files for distribution.
  #' 
  #' @author Adrian Leip \email{adrian.leip@ec.europa.eu}
  #' @references Leip, A., Koeble, R., 2018 The CAPRI disaggregation. Report in preparation
  #' @referemces Leip, A., Koeble, R., Reuter, H.I., Lamboni, M., Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284, Unpublished data archive
  #' @references Lamboni, M., Koeble, R., Leip, A., 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82, 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028
  #' @references Leip, A., Marchi, G., Koeble, R., Kempen, M., Britz, W., Li, C., 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5, 73-94. https://doi.org/10.5194/bg-5-73-2008
  #' 
  #' @return saves rdata and csv files with relevant data
  #' 
  
  # Hard-coded elements embedded as a function of (different) scopes
  scope<<-"capdistimes"                 
  #rm(list=setdiff(objects(), "capri")); 
  setwd(gsub("logfiles", "capriextract", getwd())); 
  
  #CAPRI already initialized - startextract called from InitCapriEnv
  #source("R/initializecapri.R"); 
  #InitCapriEnv(scope = scope)
  
  #source("capri_packages.r")           # Data libararies require
  require(openxlsx)
  #' CAPRI-EPNF branch used on 
  svnpath <<- "https://svn1.agp.uni-bonn.de/svn/capri/branches/epnf"
  svnrespath <<- "https://svn.jrc.es/repos/GHG/ECAMPA4Results"
  
  #' @source 
  capriversion <- as.data.frame(matrix(nrow=1, ncol=6))
  colnames(capriversion) = c("CAPRI_task", "Date", "Revision", "FilesOutput","Branch", "Note")
  capriversion[1,] <- c("CAPREG-12", "20200706", "351", "res_%BAS%%MS%.gdx", "ECAMPA4", "BAS=Base year, MS=Member State") 
  capriversion[2,] <- c("Inventories-12", "20200706", "352", "res_time_series_GHG_%MS%.gdx'", "ECAMPA4Results", "MS=Member State") 
  capriversion[3,] <- c("CAPDIS-LAPM", "20210107", "local", "capdis_%regionname%_10GRID.gdx", "epnf", "regionname: first four characters of NUTS2") 
  capriversion[4,] <- c("CAPDIS-CAPREG", "202101xx", "local", "xobs_2_%MS%_%BAS%%BAS%", "epnf", "BAS=Base year, MS=Member State") 
  capriversion[5,] <- c("CAPDIS-12-2xxx", "202101xx", "local", "xobs_2_%MS%_%BAS%%Y%", "epnf", "BAS=Base year 2 digits, Y=Simulation year 4 digits") 
  capriversion <<- capriversion
  
  #source("capri_dirs.r")               # Defines paths to data depending machine
  #source("capri_sets.r")               # Loads all relevant sets from a CAPRI dump-file
  source("capriextract_functions.r")
  source("xobsfunctions.r")
  source("openfiltermultiple.r")
  #source("R/initializecapri.R"); 
  #source(".init_leipadrD01RI1600881")
  d5space<<-"\\\\ies-ud01.jrc.it/D5_agrienv/Data/"
  folderdate <<- '20210107'
  savepath<<-paste0(d5space, "/capdis_results/20181122_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20190116_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20190125_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20190919_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20191010_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20191016_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/", folderdate, "_kipinca")
  if(! dir.exists(savepath)){dir.create(savepath)}
  
  n2o<<-c("N2OAPP", "N2OGRA", "N2OSYN", "N2OHOU")
  
  curcountries <<- substr(s$nuts0, 1, 2)
  cntnodisagg <<- c("AL", "MK", "CH", "CS", "MO", "BA", "KO", "NO", "TU", "HR")
  eu28<<-setdiff(curcountries, cntnodisagg)
  misregOK <<- c("AT130000", # Wien
                 "BL100000", # Region de Bruxelles-Capitale
                 "DE300000", # Berlin
                 "DE500000", # Bremen
                 "DE600000", # Hamburg
                 "ES630000", # Ciudad Autonoma de Ceuta (ES)
                 "ES640000", # Ciudad Autonoma de Melilla (ES)
                 "ES700000", # Canarias
                 "PT200000", # Acores
                 "PT300000" # Madeira
  )
  
  mcactuaar <<- c(s$mcact, "UAAR")
  #datapath <<- paste0(cenv$capri, "epnfresults/")
  
}

cleanup <- function(){
  #all <- objects()
  kp <- c("scope", "reginame", "cols", "ydim", "regi", "curcountries", "rows",
          "curyears", "baseyear", "curscens", "curscenshort", 
          "savepath", "datapath", "svnpath", "capriversion", "mcactuaar",
          "s", "cenv", "sursoi", "soilemissions", "lvstemissions", 
          "levlyild", "xobsname", "reginame")
  #torem <- as.vector(setdiff(all, toleav))
  torem <<- setdiff(ls(), kp)
  #print(setdiff(ls(), kp))
  rm(list=setdiff(ls(), kp), inherits=TRUE)
}

loadcurfile<-function(
  extractdate=NULL,
  scope=scope, xobsname="xobs", reginame="EU27",
  cols=mcactuaar,
  rows="LEVL", ydim=NULL, regi=NULL,
  curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  curdate <- paste0("_",format(Sys.time(), "%Y%m%d"))
  yearrange <- paste0(min(curyears), "-", max(curyears))
  if(! is.null(extractdate)){curdate <- paste0("_", extractdate)}
  curfile<-paste0(savepath, "/xobs_", reginame, "_", yearrange, "_", toupper(xobsname), curdate, ".rdata")
  cat("\n", curfile)
  if(file.exists(curfile)){
    cat("\nFile exists")
    #load(curfile)
  }else{
    xobshsu<-filtermultiple(scope=scope, 
                            cols=cols, 
                            rows=rows, 
                            ydim=ydim, #curdim5=NULL, 
                            #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                            regi=regi, 
                            curcountries=curcountries, 
                            #curcountries="BL", 
                            curyears=curyears, 
                            #curyears=curyear,
                            baseyear=baseyear, 
                            curscens=curscens, 
                            curscensshort=curscensshort
    )
    #capriinfo <- capridat[[2]]
    #capridata <- capridat[[1]]
    # xobsnuts2<-capridat[!grepl("^U[1-9]", capridat$RALL),]
    # xobshsu<-capridat[grepl("^U[1-9]", capridat$RALL),]
    # assign(paste0(xobsname,"hsu"), xobshsu)
    # assign(paste0(xobsname,"nuts2"), xobshsu)
    # save(xobsnuts2, xobshsu, file=paste0(curfile))
    xobs <- xobshsu[[1]]
    xobsinfo<-xobshsu[[2]]
    xobsnuts2<-xobs[!grepl("^F[1-9]", xobs$rall),]
    xobshsu<-xobs[grepl("^F[1-9]", xobs$rall),]
    
    assign(paste0(xobsname,"fsu"), xobshsu)
    assign(paste0(xobsname,"nuts2"), xobsnuts2)
    
    save(list=c(paste0(xobsname,"fsu"), paste0(xobsname,"nuts2")), file=paste0(curfile))
    return(xobshsu)
  }
}

extractall <- function(extractdate=NULL, scope=scope, reginame="EU27",
                       cols=s$mcact,
                       ydim=NULL, regi=NULL,
                       curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  if(reginame=="EU27") { curcountries <- eu28 }
  # Note: N2OCRO and N2OHIS not included here
  
  
  sursoi<-c("SURSOI","NinSOI","NMANAP","NMINSL","ATMOSD","CRESID","NRET","YILD","LEVL","BIOFIX","NMANGR")
  rows <- sursoi
  xobsname <-"sursoi"
  reginame <- paste(reginame, collapse = "")
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  xobshsu<-loadcurfile(extractdate=extractdate, scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL,
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  if(exists("xobshsu")){rm(xobshsu)}
  
  soilemissions<-c("N2OAPP", "N2OGRA", "N2OSYN", "NH3APP", "NH3GRA", "NH3SYN", "NOXAPP", "NOXGRA", "NOXSYN", "RUNMIN", "RUNSUR", "CH4RIC")
  rows <- soilemissions
  xobsname="soilemissions"
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  xobshsu<-loadcurfile(extractdate=extractdate, scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL,
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  if(exists("xobshsu")){rm(xobshsu)}
  
  lvstemissions<-c("LU", "EXCRET", "N2OHOU", "N2OSTO", "NH3HOU", "NH3STO", "NOXHOU", "NOXSTO", "N2STO", "RUNHOU", "CH4ENT", "CH4MAN")
  rows <- lvstemissions
  xobsname="lvstemissions"
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  xobshsu<-loadcurfile(extractdate=extractdate, scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL,
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  if(exists("xobshsu")){rm(xobshsu)}
  
  levlyild <- c("LEVL", "YILD")
  rows <- levlyild
  xobsname="levlyild"
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  xobshsu<-loadcurfile(extractdate=extractdate, scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL,
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  if(exists("xobshsu")){rm(xobshsu)}
  
  xobsname="livestock"
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  xobshsu<-loadcurfile(extractdate=extractdate, scope="capdistimesLU", xobsname=xobsname, reginame=reginame,
                       cols=NULL, rows=c("1000Ha", "HeadperHa", "1000Head"), ydim=NULL, #curdim5=NULL, 
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  if(exists("xobshsu")){rm(xobshsu)}
  
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  xobshsu<-loadcurfile(extractdate=extractdate, scope="capdistimes", xobsname="maactlevl", reginame=reginame,
                       cols=s$maact, rows="LEVL", ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  if(exists("xobshsu")){rm(xobshsu)}
  
  
}

combinedata4kipinca <- function(date2load = "", reginame="EU27", yearrange){
  
  date2save <- paste0("_", format(Sys.time(), "%Y%m%d"))
  curfile<-paste0(savepath, "/xobs_", reginame, "_", yearrange, "_")
  
  # Calculate total for syn and manemissions
  cat("\nLoad",paste0(curfile, "SOILEMISSIONS", date2load, ".rdata"))
  load(paste0(curfile, "SOILEMISSIONS", date2load, ".rdata"))
  
  synemissions<-soilemissionsfsu[grepl("SYN|MIN", rows)]
  synemissionstot<-synemissions[, sum(value),by=list(rall, cols, y)]
  synemissionstot$rows <- 'MINLOSSES'
  manemissions<-soilemissionsfsu[grepl("APP|GRA", rows)]
  manemissionstot<-manemissions[, sum(value),by=list(rall, cols, y)]
  manemissionstot$rows <- 'MANLOSSES'
  setnames(manemissionstot, "V1", "value")
  setnames(synemissionstot, "V1", "value")
  manemissionstot <- manemissionstot[,.(rall, cols, rows, y, value)]
  synemissionstot <- synemissionstot[,.(rall, cols, rows, y, value)]
  symmanemissionstot <- rbind(synemissionstot, manemissionstot)
  rm(synemissionstot, manemissionstot, manemissions, synemissions, soilemissionsfsu)
  
  
  # Calculate total from manure management systems
  cat("\nLoad",paste0(curfile, "lvstemissions", date2load, ".rdata"))
  load(paste0(curfile, "lvstemissions", date2load, ".rdata"))
  mmsrows <- unique(lvstemissionsfsu$rows)
  mmsrows <- mmsrows[grepl("HOU|STO", mmsrows)]
  mmsfsutot <-lvstemissionsfsu[rows %in% mmsrows, sum(value),by=list(rall, cols, y)]
  mmsfsutot$rows <- 'MMSLOSSES'
  setnames(mmsfsutot, "V1", "value")
  mmsfsutot <- mmsfsutot[,.(rall, cols, rows, y, value)]
  synmanmmstot <- rbind(symmanemissionstot, mmsfsutot)
  rm(symmanemissionstot, mmsfsutot, lvstemissionsfsu)
  save(synmanmmstot, file=paste0(curfile, "SYNMANMMSLOSTOT", date2load, ".rdata"))
  
  # Add to sursoi
  cat("\nLoad",paste0(curfile, "SURSOI", date2load, ".rdata"))
  load(paste0(curfile, "SURSOI", date2load, ".rdata"))
  sursoifsu <- rbind(sursoifsu[,.(rall, cols, rows, y, value)], synmanmmstot)
  rm(synmanmmstot)
  save(sursoifsu, file=paste0(curfile, "NBUDGET", date2load, ".rdata"))
  
  # Save also by country so that it can be loaded individually
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  sursoifsu <- merge(sursoifsu, fsu_delimdata[, .(rall=fsuID, CNTR_CODE)])
  for (c in unique(sursoifsu$CNTR_CODE)){
    assign(paste0("sursoifsu", c), sursoifsu[CNTR_CODE==c])
    save(list=paste0("sursoifsu", c), file=paste0(curfile, "NBUDGET_", c, date2load, ".rdata"))
  }
  
}


cleandate4kipinca <- function(folderdate = "", extractiondate = NULL, reginame="EU27", yearrange){
  
  # Calculates statistics to check distributions
  # Creates a readable data table
  # Cleans very small values adn checks on outliers
  # Puts an upper cap on very high N input
  
  
  if(is.null(extractiondate)){extractiondate <- paste0("_", format(Sys.time(), "%Y%m%d"))}
  
  nbpath <- paste0("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/", folderdate, "_kipinca/")
  nbfile <- paste0(nbpath, "xobs_", reginame, "_", yearrange, "_NBUDGET", extractiondate)
  
  xlsfile <- paste0(nbpath, "xobs_", reginame, "_", yearrange, "_NBUDGET", extractiondate, ".xlsx")
  xlswb <- createWorkbook(creator = "Adrian Leip", title = "Tests for KI-INCA data", subject = "CAPDIS N-BUDGET")
  
  cat("\n", nbfile)
  load(paste0(nbfile, ".rdata"))
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  y <- sursoifsu
  y <- merge(y, fsu_delimdata[, .(rall=fsuID, CAPRINUTS2, CNTR_CODE)])
  save(list=objects(), file = "y.rdata")
  
  cat("\nCalculating statistics for UAAR data")
  stats <- y[cols=="UAAR", .( min = min(value),
                              max = max(value),
                              mean = mean(value),
                              median = median(value)), by=c("CAPRINUTS2", "rows", "y")]
  xlsws <- addWorksheet(xlswb, sheetName = "UAAR_stats")
  xlsws <- writeData(xlswb, sheet = "UAAR_stats", x = stats)
  #write.csv(stats, file=paste0(nbfile, "_UAAR_stats.csv"))
  cat("\nCalculating statistics for non-UAAR data")
  stats <- y[cols!="UAAR", .( min = min(value),
                              max = max(value),
                              mean = mean(value),
                              median = median(value)), by=c("CAPRINUTS2", "rows", "y")]
  xlsws <- addWorksheet(xlswb, sheetName = "nonUAAR_stats")
  xlsws <- writeData(xlswb, sheet = "nonUAAR_stats", x = stats)
  cat("\nDcast data by rows, save data")
  nbudget <- dcast.data.table(y, rall + cols + y + CAPRINUTS2 + CNTR_CODE ~ rows, value.var = "value", fill = 0)
  
  cat("\nDelete rows with LEVL below 0.01")
  minLEVL <- 0.005
  xSmallLevl <- nbudget[LEVL < minLEVL]
  nbudget <- nbudget[LEVL >= minLEVL]
  
  
  cat("\nHigh NinSOI (exclude UAAR) that is not due to CRESID (allow for 400 higher NinSOI)")
  #                         and that is not due to NMANGR/NMANAP
  xlimit <- 2000
  xHigh_NinSOI <- nbudget[NinSOI > (xlimit+200) & !cols %in% c("UAAR") & CRESID < (xlimit-200) 
                          & NMANGR < (xlimit-200) & NMANAP < (xlimit-200)]
  nbudget[NinSOI > (xlimit)]
  nbudget[NinSOI > (xlimit) & NMANGR > 1000]
  xl <- nbudget[NinSOI > (xlimit)]
  xf <- paste0("NinSOI.gt.", as.character(xlimit))
  xlsws <- addWorksheet(xlswb, sheetName = xf)
  xlsws <- writeData(xlswb, sheet = xf, x = xl)
  
  xlsws <- addWorksheet(xlswb, sheetName = "ATMOSD.gt.80")
  xlsws <- writeData(xlswb, sheet = "ATMOSD.gt.80", x = nbudget[ATMOSD>80 & cols=="UAAR"])
  
  cat("\nHigh SURSOI. If due to VERY high NMANAP - reduce all down to 400")
  cat("\nFirst correct those that depend on one input term")
  
  sLimit <- 400
  nbudget[SURSOI > sLimit]
  
  xl <- nbudget[SURSOI > sLimit]
  xf <- paste0(nbfile, "_SURSOI.gt.", sLimit)
  xlsws <- addWorksheet(xlswb, sheetName = paste0("SURSOI.gt.", sLimit))
  xlsws <- writeData(xlswb, sheet = paste0("SURSOI.gt.", sLimit), x = xl)
  
  xHigh_SURSOI <- nbudget[SURSOI > sLimit & NMANAP > 400]
  XHigh_Sursoi_correct <- xHigh_SURSOI[SURSOI > sLimit & NMANAP > sLimit, 
                                       `:=` ( SURSOI = 400, NMANAP = NMANAP+400-SURSOI, NinSOI = NinSOI+400-SURSOI)]
  # NMANAP > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP > sLimit & CRESID < sLimit & NMINSL < sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, NMANAP = NMANAP+400-SURSOI, NinSOI = NinSOI+400-SURSOI)]
  # CRESID > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID > sLimit & NMINSL < sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, CRESID = CRESID+400-SURSOI, NinSOI = NinSOI+400-SURSOI)]
  #NMINSL > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID < sLimit & NMINSL > sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, NMINSL = NMINSL+400-SURSOI, NinSOI = NinSOI+400-SURSOI)]
  #NMANGR > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID < sLimit & NMINSL < sLimit & NMANGR > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, NMANGR = NMANGR+400-SURSOI, NinSOI = NinSOI+400-SURSOI)]
  nbudget[SURSOI > sLimit]
  max(nbudget$SURSOI, na.rm=TRUE)
  
  cat("\nThen correct in pairs")
  #NMANAP and NMANGR > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP > sLimit & CRESID < sLimit & NMINSL < sLimit & NMANGR > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR), 
                                  NMANGR = NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR), 
                                  NinSOI = NinSOI+(400-SURSOI))]
  #CRESID and NMINSL > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID > sLimit & NMINSL > sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  CRESID = CRESID+(400-SURSOI)*CRESID/(CRESID+NMINSL), 
                                  NMINSL = NMINSL+(400-SURSOI)*NMINSL/(CRESID+NMINSL), 
                                  NinSOI = NinSOI+(400-SURSOI))]
  #CRESID, NMANAP and NMANGR > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP > sLimit & CRESID > sLimit & NMINSL < sLimit & NMANGR > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID), 
                                  NMANGR = NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID), 
                                  CRESID = CRESID+(400-SURSOI)*CRESID/(NMANAP+NMANGR+CRESID), 
                                  NinSOI = NinSOI+(400-SURSOI))]
  nbudget[SURSOI > sLimit]
  max(nbudget$SURSOI, na.rm=TRUE)
  
  cat("\nFinally scale")
  xsel <- nbudget[SURSOI > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  NMANGR = NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  CRESID = CRESID+(400-SURSOI)*CRESID/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  NMINSL = NMINSL+(400-SURSOI)*NMINSL/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  NinSOI = NinSOI+(400-SURSOI))]
  nbudget[SURSOI > sLimit]
  max(nbudget$SURSOI, na.rm=TRUE)
  
  
  cat("\nDelete values close to zero")
  nbudget <- nbudget[NMANGR > -0.1 & NMANGR < 0.001 , NMANGR := 0]
  nbudget <- nbudget[NMANAP > -0.1 & NMANAP < 0.001, NMANAP := 0]
  nbudget <- nbudget[NMINSL > -0.1 & NMINSL < 0.001, NMINSL := 0]
  nbudget <- nbudget[MINLOSSES > -0.1 & MINLOSSES < 0.001, MINLOSSES := 0]
  nbudget <- nbudget[MANLOSSES > -0.1 & MANLOSSES < 0.001, MANLOSSES := 0]
  nbudget <- nbudget[MMSLOSSES > -0.1 & MMSLOSSES < 0.001, MMSLOSSES := 0]
  nbudget <- nbudget[CRESID > -0.1 & CRESID < 0.001, CRESID := 0]
  
  cat("\nCheck for negative values where there shouldn't be")
  xneg <- nbudget[NinSOI < 0]
  xlsws <- addWorksheet(xlswb, sheetName = "NinSOInegative")
  xlsws <- writeData(xlswb, sheet = "NinSOInegative", x = xneg)
  nbudget <- nbudget[NinSOI >= 0]
  
  xneg <- nbudget[NMANGR < 0]
  xneg <- nbudget[NMANAP < 0]
  
  cat("\nSave cleaned data")
  
  save(nbudget, file=paste0(nbfile, "_dcastclean", ".rdata"))
  for (c in unique(nbudget$CNTR_CODE)){
    assign(paste0("nbudget", c), nbudget[CNTR_CODE==c])
    save(list=paste0("nbudget", c), file=paste0(nbfile, c, "_dcastclean", ".rdata"))
  }
  
  # NRET timeseries
  nrettimeseries <- dcast.data.table(y[rows=="NRET" | rows=="LEVL"], rall + cols + rows ~ y, value.var="value")
  
  cat("\nCalculating statistics for cleaned data UAAR data")
  nbudget <- melt.data.table(nbudget, id.vars = c("rall", "cols", "y", "CAPRINUTS2", "CNTR_CODE"))
  nbudget <- nbudget[! is.na(value)]
  #save(nbudget, file=paste0(nbfile, "_tempbeforestatistics.rdata"))
  stats <- nbudget[cols=="UAAR", .( min = min(value),
                                    max = max(value),
                                    mean = mean(value),
                                    median = median(value)), by=c("CAPRINUTS2", "variable", "y")]
  xlsws <- addWorksheet(xlswb, sheetName = "UAAR_statscleaned")
  xlsws <- writeData(xlswb, sheet = "UAAR_statscleaned", x = stats)
  cat("\nCalculating statistics for non-UAAR data")
  stats <- nbudget[cols!="UAAR", .( min = min(value),
                                    max = max(value),
                                    mean = mean(value),
                                    median = median(value)), by=c("CAPRINUTS2", "variable", "y")]
  xlsws <- addWorksheet(xlswb, sheetName = "nonUAAR_statscleaned")
  xlsws <- writeData(xlswb, sheet = "nonUAAR_statscleaned", x = stats)
  
  stats <- nbudget[, .( min = min(value, na.rm=TRUE),
                        max = max(value, na.rm=TRUE),
                        mean = mean(value, na.rm=TRUE),
                        median = median(value, na.rm=TRUE)), by=c("cols", "variable")]
  statsd <- dcast.data.table(stats, cols ~ variable, value.var = c("min", "max", "mean"))
  xlsws <- addWorksheet(xlswb, sheetName = "all_bycolsonly_statscleaned")
  xlsws <- writeData(xlswb, sheet = "all_bycolsonly_statscleaned", x = statsd)
  
  cat("\nSaving Excel workbook with test restults: ", xlsfile)
  xlswf <- saveWorkbook(xlswb, file = xlsfile, overwrite = TRUE)
  
  
}


replaceEUfilesbynewcountry <- function(reginame="FI", lastdate=""){
  
  load("//ies/d5/agrienv/Data/FSU/fsu_delimdata.rdata")
  fsudelim <- fsu_delimdata[, .(rall=fsuID, CNTR_CODE)]
  fsudelim <- fsudelim[, fsuID_nr := as.numeric(gsub("F", "", rall))]
  nuts2del <- fsu_delimdata[, .(CAPRINUTS2, FSUADM2_ID)]
  replacePar <- function(reginame, xobsname, lastdate){
    
    if(lastdate !=""){lastdate <- paste0("_", lastdate)}
    
    toload <- paste0(savepath, "/xobs_", "EU27", "_", yearrange, "_", toupper(xobsname), lastdate, ".rdata")
    cat("\nLoading", toload)
    load(toload)
    assign("eu", get(paste0(xobsname, "fsu")))
    assign("eu2", get(paste0(xobsname, "nuts2")))
    rm(list=c(paste0(xobsname, "fsu"), paste0(xobsname, "nuts2")))
    
    toload <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_", toupper(xobsname), ".rdata")
    cat("\nLoading", toload)
    load(toload)
    assign("reg", get(paste0(xobsname, "fsu")))
    assign("reg2", get(paste0(xobsname, "nuts2")))
    rm(list=c(paste0(xobsname, "fsu"), paste0(xobsname, "nuts2")))
    
    #fsuinreg <- unique(reg$rall)
    #eu <- eu[! rall %in% fsuinreg]
    eu <- merge(eu, fsudelim, by="rall")
    reg <- merge(reg, fsudelim, by="rall")
    eu <- eu[!grepl(reginame, CNTR_CODE)]
    eu2 <- eu2[! grepl(reginame, rall)]
    
    eufsu <- rbind(eu, reg)
    rm(list=c("eu", "reg"))
    eufsu <- eufsu[order(fsuID_nr)]
    eufsu <- eufsu[, .(rall, cols, rows, y, value, n)]
    assign(paste0(xobsname, "fsu"), eufsu)
    rm(eufsu)
    
    eunuts2 <- rbind(eu2, reg2)
    eunuts2 <- eunuts2[order(rall)]
    assign(paste0(xobsname, "nuts2"), eunuts2)
    rm(eunuts2, eu2, reg2)
    
    
    save(list=c(paste0(xobsname,"fsu"), paste0(xobsname,"nuts2")), 
         file=paste0(savepath, "/xobs_", "EU27", "_, ", yearrange, "_", toupper(xobsname), "_",format(Sys.time(), "%Y%m%d"), ".rdata"))
    rm(list=c(paste0(xobsname, "fsu"), paste0(xobsname, "nuts2")))
    
  }
  
  replacePar(reginame, xobsname="lvstemissions", lastdate=lastdate)
  replacePar(reginame, xobsname="levlyild", lastdate=lastdate)
  replacePar(reginame, xobsname="soilemissions", lastdate=lastdate)
  replacePar(reginame, xobsname="sursoi", lastdate=lastdate)
  #replacePar(reginame, xobsname="maactlevl", lastdate=lastdate)
  #replacePar(reginame, xobsname="livestock", lastdate=lastdate)
  
}

writemeta<-function(){
  con<-file(paste0(savepath, "/README_capridisagg_",format(Sys.time(), "%Y%m%d"),".txt"),open = "wt")
  fsets <- paste0(savepath, "/README_caprisets_",format(Sys.time(), "%Y%m%d"),".txt")
  
  cat("\n Write meta information ", format(Sys.time(), "%Y%M%d %H:%M"))
  writeLines(paste0("# README file for the disaggregatgion of Nitrogen surplus data at FSU level for the KIP-INCA project",
                    "\n#",
                    "\n# Disaggregated data from CAPRI time series: relevant elements for the Gross Nitrogen Surplus by crop at the",
                    "\n# spatial levle of Farmstructure Soil Units (FSU).",
                    "\n\n# Content of folders:",
                    "\n# Data are organized in 1 folder per parameter. Each row correspond to 1 FSU. Columns are fsuID, fsuID_nr, activities.",
                    "\n#     - For a shapefile with the geometry of the FSU see @RENATE pleaseX.",
                    "\n#     - For a description of crop and livestock activities see file ", fsets,
                    "\n# Variables are as follows:",
                    "\n#        Nitrogen flows relevant for SURSOI",
                    "\n#        Soil budgets terms [kg N / ha / yr] with ha referring to crop activities",
                    "\n#             - NinSOI N inputs to soil as the farmer applies emissions from application have yet to occur",
                    "\n#                      NinSOI = BIOFIX + NMINSL + NMANAP + NMANGR + ATMOSD + CRESID - ",
                    "\n#             - BIOFIX Biological fixation",
                    "\n#             - NMINSL Mineral fertilizer N input net of gaseous losses and run-off",
                    "\n#             - NMANAP Manure input net of all surface losses. Part applied intentionally to agricultural land",
                    "\n#             - NMANGR Manure input net of all surface losses. Part deposited by grazing animals",
                    "\n#             - ATMOSD Atmospheric deposition",
                    "\n#             - CRESID Crop residues",
                    "\n#             - SURSOI Soil surface surplus: all gaseous emissions from manure and mineral fertilizer as well as runoff already subtracted",
                    "\n#                      SURSOI = NinSOI - NRET",
                    "\n#                      SURSOI = Leaching plus denitrification (N2)",
                    "\n#             - NRET   Crop retention",
                    "\n#             - MINLOSSES N losses from mineral fertilizer (NH3, N2O, NOX, run-off)",
                    "\n#             - MANLOSSES N losses from manure after application (NH3, N2O, NOX, run-off)",
                    "\n#             - MMSLOSSES N losses from manure in manure management system",
                    "\n#                      Total excretion of N in manure is obtained from:",
                    "\n#                      EXCRET = NMANAP + NMANGR + MANLOSSES + MMSLOSSES",
                    "\n#",
                    "\n#        Activities",
                    "\n#             - LEVL Cultivation of crops [1000 ha]",
                    "\n#             - LEVLLIVESTOCK Number of animals [1000 head] or [1000000 head for poultry]",
                    "\n#             - 1000Head Number of heads of livestock groups differentiated for grazing and non-grazing animals",
                    "\n#                    Attention that the individual categories for Dairy and Nondairy Cattle are the sum of animals with different LU per head!",
                    "\n#                    The same shares is applied to all livestock activities - see LEVLLIVESTOCK and LU converstion factors:",
                    "\n#                    Livestock activities 'LU' [100 Livestock Units, see https://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Livestock_unit_(LSU)]",
                    "\n#             - 1000Ha Area to which the livestock density refers. This area for grazing animals is based on shares of variour Corine Land Cover Classes. The area for non-grazing animals is UAAR.",
                    "\n#             - HeadperHa Livestock density - calculated from 1000Head and 1000Ha",
                    "\n# ",
                    "\n# "
  ), con)
  
  #                      "\n# rdata-files and (ii) into csv files for distribution.",
  
  
  writeLines(paste0("# @Data source: CAPRI results",svnpath),con)
  writeLines(paste0("# @Repository: ",svnpath),con)
  writeLines(paste(c("#", names(capriversion)), collapse=","), con)
  write.table(capriversion, quote=FALSE, col.names=FALSE, row.names=rep("#", nrow(capriversion)), sep=",", con)
  
  writeLines(paste0("#\n# @Data processing: https://github.com/aleip/capriextract"),con)
  writeLines(paste0("#    file: capdistseries_functions.r, functions extractall and reload_and_write and other functions called therein."),con)
  writeLines("#\n# @author Adrian Leip adrian.leip@ec.europa.eu", con)
  
  writeLines(paste0("#\n# @references Leip A. Koeble R. 2018 The CAPRI disaggregation. Report in preparation"), con)
  writeLines(paste0("# @references Leip A. Koeble R. Reuter H.I. Lamboni M. Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284 Unpublished data archive"), con)
  writeLines(paste0("# @references Lamboni M. Koeble R. Leip A. 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028"), con)
  writeLines(paste0("# @references Leip A. Marchi G. Koeble R. Kempen M. Britz W. Li C. 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5 73-94. https://doi.org/10.5194/bg-5-73-2008"), con)
  writeLines(paste0("#\n# @documentation see XZXXX"), con)
  writeLines("#\n", con)
  
  close(con)
  
  con<-file(fsets,open = "wt")
  curset <- sdesc[grepl("activities", set)]
  write.table(curset, quote=FALSE, row.names=FALSE, sep=",", con)
  close(con)
}

write1para <- function(datap, p, reginame, yearrange){
  #save(list=objects(), file="w1.rdata")
  datap$rows <- p
  datap <- datap[, .(fsuID=rall, fsuID_nr = as.numeric(gsub("F", "", rall)), cols, rows, y, value)]
  datap <- datap[abs(value) < 1e-6, value := 0]
  yrs <- unique(datap$y)
  cat("dcast for crops ")
  for(yr in yrs){
    cat("\n Extract", as.character(p), "for year", yr, "from data ", format(Sys.time(), "%Y%M%d %H:%M"))
    wdata <- dcast.data.table(datap[y==yr], fsuID + fsuID_nr ~ cols, drop=TRUE, value.var="value", fun=sum)
    
    #Add first line with zeros to indicate to ArcGIS that these are numeric data
    firsline <- wdata[1]
    firsline <- firsline[, `:=` (fsuID = "F0", fsuID_nr = 0)]
    r <- setdiff(names(firsline), c("fsuID", "fsuID_nr"))
    v <- rep(0.000001, length(r))
    firsline <- firsline[, as.vector(r) := as.list(v)]
    wdata <- rbind(firsline, wdata)
    
    #Create subfolder if it does not exist
    if(! dir.exists(paste0(savepath, "/", p))){
      dir.create(paste0(savepath, "/", p))
    }
    
    # Ensure correct numeric sorting of the data by fsuID
    wdata <- wdata[order(fsuID_nr)]
    
    con<-file(paste0(savepath, "/", p, "/capridisagg_", reginame, "_", p, "_", yr, "_",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
    cat("\n Write data ", format(Sys.time(), "%Y%M%d %H:%M"))
    write.csv(wdata, quote=FALSE, con,row.names=FALSE, na="")
    close(con)
    
  }
}
wrapoverwrite <- function(x, reginame, yearrange){
  
  # Write out data
  params<-setdiff(names(x), c("rall", "cols", "y"))
  xcopy <- copy(x)
  for(p in params){
    datap <- xcopy[, c("rall", "cols", "y", p), with=FALSE]
    setnames(datap, p, "value")
    write1para(datap = datap, p = p, reginame = reginame, yearrange = yearrange)
  }
}

writelivestock <- function(x2wf, reginame){
  
  cat("\nLoad ", x2wf)
  load(x2wf)
  x2w <- livestockfsu
  
  #CAPRI daten sind auf 'head' basis - delete die LU variablen
  x2w <- x2w[! grepl("LU", rows)]
  
  # Combine cols and type (type is grazing, non-grazing (acronyms))
  x2w <- dcast.data.table(x2w, rall + rows + y ~ cols + type, value.var="value")
  x2w <- melt.data.table(x2w, id.vars=1:3, measure.vars=4:length(x2w), value.name='value', variable.name='cols')
  
  # Make sure that there are no rows witout data
  x2w <- x2w[!is.na(value), .(rall, cols, rows, y, value)]
  # Bring the 'rows' (variables combined with grazing/nongrazing) into column
  x2w <- dcast.data.table(x2w, rall + cols + y ~ rows, value.var="value", fill=0)
  # Delete rows with very small livestock numbers
  x2w <- x2w[HeadperHa > 0]
  livestockfsu <- x2w
  save(livestockfsu, file = gsub(".rdata", "_dcast.rdata", x2wf))   
  wrapoverwrite(livestockfsu, reginame = reginame)
  
  load(file=gsub("LIVESTOCK", "MAACTLEVL", x2wf))
  maactlvl <- write1para(maactlevlfsu, "LEVLLIVESTOCK", reginame = reginame, yearrange = yearrange)

    
  
  return(livestockfsu)
  
  
  
}


dohistos <- function(forhist){
  
  params<-setdiff(names(forhist), c("rall", "cols", "y", "CAPRINUTS2", "CNTR_CODE"))
  if(! dir.exists(paste0(savepath, "/histograms"))) { dir.create(paste0(savepath, "/histograms")) }
  
  for (p in params){
    dohist <- forhist[cols != "UAAR", p, with=FALSE]
    if(p=="MMSLOSSES"){dohist <- forhist[cols == "UAAR", p, with=FALSE]}
    setnames(dohist, p, "value")
    
    dohist <- dohist[value>quantile(dohist$value, probs = 0.001) & value<quantile(dohist$value, probs = 0.999)]
    png(paste0(savepath, "/histograms/", p, "999.png"))
    hist(dohist$value, breaks=100,
         main = paste0("0.1-99.9%ile for ", p),
         xlab = paste0("mean=", round(mean(dohist$value), 2),
                       ", med=", round(median(dohist$value), 2), 
                       ", min=",round(min(dohist$value), 2),
                       ", max=",round(max(dohist$value), 2)))
    dev.off()
    
    dohist <- dohist[value>quantile(dohist$value, probs = 0.01) & value<quantile(dohist$value, probs = 0.99)]
    
    png(paste0(savepath, "/histograms/", p, "99.png"))
    hist(dohist$value, breaks=100,
         main = paste0("1-99%ile for ", p),
         xlab = paste0("mean=", round(mean(dohist$value), 2),
                       ", med=", round(median(dohist$value), 2), 
                       ", min=",round(min(dohist$value), 2),
                       ", max=",round(max(dohist$value), 2)))
    dev.off()
  }
  
}

checkregions <- function(folderdate='20191010', savedate='20191011'){
  
  if(is.null(savedate)){savedate <- folderdate}
  kipf <- paste0("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/", folderdate, "_kipinca/xobs_EU27_", yearrange, "_")
  
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  
  misnamesall <- data.table()
  nfiles <- c("SURSOI", "SOILEMISSIONS", "LVSTEMISSIONS", "LEVLYILD", "LIVESTOCK", "MAACTLEVL")
  for ( NbudgetFile in nfiles){
    
    e <- new.env(parent = emptyenv())
    cat("\nLoad ", paste0(kipf, NbudgetFile, "_", savedate, ".rdata"))
    load(paste0(kipf, NbudgetFile, savedate, ".rdata"), envir=e)
    cat(" ", ls(envir=e)[grepl("nuts2", ls(envir=e))])
    xnuts <- e[[ls(envir=e)[grepl("nuts2", ls(envir=e))]]]
    
    if("UAAR" %in% xnuts$cols) {x
      nuts <- dcast.data.table(xnuts[cols=="UAAR"], rall + cols + y ~ rows, value.var="value")
    }else{
      nuts <- dcast.data.table(xnuts, rall + cols + y ~ rows, value.var="value")
    }
    setnames(xnuts, "rall", "CAPRINUTS2")
    allnuts <- merge(unique(fsu_delimdata[, .(CAPRINUTS2)]), xnuts, by="CAPRINUTS2", all.x=TRUE)
    
    miscntOK <- c("AL", "BA", "CH", "CS", "HR", "MK", "NO", "TR")
    miscntOK <- paste0("^", miscntOK)
    miscntOK <- paste(miscntOK, collapse="|")
    misnuts <- allnuts[is.na(cols)]
    misnuts <- misnuts[! grepl(miscntOK, CAPRINUTS2)]
    
    
    misnames <- unique(merge(misnuts[! CAPRINUTS2 %in% misregOK, .(CAPRINUTS2)], sdesc[, .(CAPRINUTS2=element, description)], by="CAPRINUTS2"))
    xnuts[CAPRINUTS2 %in% misnames$CAPRINUTS2]
    misnamesall <- rbind(misnamesall, cbind(NbudgetFile, misnames))
  }
  write.csv(misnamesall, file=paste0(kipf, savedate, "_missingNuts.csv"))
}

regionaverages <- function(){
  
  nbudgabs <- copy(nbudget)
  params <- setdiff(names(nbudgabs), c("rall", "cols", "y", "CAPRINUTS2", "CNTR_CODE", "LEVL"))
  newparams <- paste0(params, "tot")
  nbudgabs <- nbudgabs[,  c(newparams) := (LEVL * .SD), .SDcols = params]
  nbudnuts <- nbudgabs[, lapply(.SD, sum), .SDcols = c(newparams, "LEVL"), by=.(CAPRINUTS2, cols, y)]
  nbudnuts <- nbudnuts[, c(params) := .SD/LEVL, .SDcols = newparams]
  nbudcntr <- nbudgabs[, lapply(.SD, sum), .SDcols = c(newparams, "LEVL"), by=.(CNTR_CODE, cols, y)]
  nbudcntr <- nbudcntr[, c(params) := .SD/LEVL, .SDcols = newparams]
  nbudcntr <- dcast.data.table(melt.data.table(nbudcntr, id.vars = c("CNTR_CODE", "cols", "y"), measure.vars = c(params, "LEVL"), variable.name = "rows"), 
                               CNTR_CODE + cols + rows ~ y, value.var = "value", drop = TRUE)
  
  
  folderdate <- "20191016"
  date2load <- "20191016"
  nbpath <- paste0("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/", folderdate, "_kipinca/")
  nbfile <- paste0(nbpath, "xobs_EU27_", yearrange, "_NBUDGET_", date2load)
  write.csv(nbudcntr[cols=="UAAR"], paste0(nbfile, "_UAAR-NUTS0.csv"), na = "", row.names = FALSE)
  
  
}

