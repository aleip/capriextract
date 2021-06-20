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

getsimuregions <- function(){
  
}

loadcurfile<-function(
  extractdate=NULL,
  scope=scope, xobsname="xobs", reginame="EU27",
  cols=mcactuaar,
  rows="LEVL", ydim=NULL, regi=NULL,
  curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  yearrange <- paste0(min(curyears), "-", max(curyears))
  curfile<-paste0(savepath, "/xobs_", reginame, "_", yearrange, "_", toupper(xobsname), extractdate, ".rdata")
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
    xobs <- xobshsu[[1]]
    xobsinfo<-xobshsu[[2]]
    xobsnuts2<-xobs[!grepl("^F[1-9]", xobs$rall),]
    xobshsu<-xobs[grepl("^F[1-9]", xobs$rall),]
    
    assign(paste0(xobsname,"fsu"), xobshsu)
    assign(paste0(xobsname,"nuts2"), xobsnuts2)
    
    save(list=c(paste0(xobsname,"fsu"), paste0(xobsname,"nuts2")), file=paste0(curfile))
  }
  return(curfile)
}
extractP <- function(extractdate=NULL, scope=scope, reginame="EU27", project=project,
                     cols=s$mcact,
                     ydim=NULL, regi=NULL,
                     curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  if(reginame=="EU27") { curcountries <- eu28 }
  # Note: N2OCRO and N2OHIS not included here
  
  curdate <- extractdate
  yearrange <- paste0(min(curyears), "-", max(curyears))
  
  sursoi<-c("PMAN","PMIN","PRET")

  rows <- c(sursoi)
  cols <- c(mcactuaar, "OLND", s$maact)
  xobsname <-"ghgp"
  reginame <- paste(reginame, collapse = "")
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  load(loadcurfile(extractdate=curdate, scope="capdistimes", xobsname=xobsname, reginame=reginame,
                   cols=cols, rows=rows, ydim=NULL, #curdim5=NULL,
                   #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                   regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort=''))
  
  
  curfile<-paste0(savepath, "/xobs_", reginame, "_", yearrange, "_", toupper(xobsname), "tot", curdate, ".rdata")
  save(list=c(paste0(xobsname,"fsu")), file=paste0(curfile))
  save(list=c(paste0(xobsname,"nuts2")), file=gsub("tot", "nuts2", paste0(curfile)))
  # Save also by country so that it can be loaded individually
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  ghgpfsu <- merge(ghgpfsu, fsu_delimdata[, .(rall=fsuID, CNTR_CODE)])
  saveeachcountry <- FALSE
  if(saveeachcountry){
  for (c in unique(ghgnfsu$CNTR_CODE)){
    assign(paste0(xobsname, "fsu", c), ghgnfsu[CNTR_CODE==c])
    save(list=paste0(xobsname, "fsu", c), file=gsub(".rdata", paste0("_", c, ".rdata"), curfile))
  }}

  return(ghgpfsu)
  
  }
extractall <- function(extractdate=NULL, scope=scope, reginame="EU27", project=project,
                        cols=s$mcact,
                        ydim=NULL, regi=NULL,
                        curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  if(reginame=="EU27") { curcountries <- eu28 }
  # Note: N2OCRO and N2OHIS not included here

  curdate <- extractdate
  yearrange <- paste0(min(curyears), "-", max(curyears))
  
  sursoi<-c("SURSOI","NinSOI","NMANAP","NMINSL","ATMOSD","CRESID","NRET","YILD","LEVL","BIOFIX","NMANGR")
  soilemissions<-c("N2OAPP", "N2OGRA", "N2OSYN", "NH3APP", "NH3GRA", "NH3SYN", "NOXAPP", "NOXGRA", "NOXSYN", "RUNMIN", "RUNSUR", "CH4RIC")
  lvstemissions<-c("LU", "EXCRET", "N2OHOU", "N2OSTO", "NH3HOU", "NH3STO", "NOXHOU", "NOXSTO", "N2STO", "RUNHOU", "CH4ENT", "CH4MAN")
  rows <- c(sursoi, soilemissions, lvstemissions)
  cols <- c(mcactuaar, "OLND", s$maact)
  xobsname <-"ghgn"
  reginame <- paste(reginame, collapse = "")
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  load(loadcurfile(extractdate=curdate, scope="capdistimes", xobsname=xobsname, reginame=reginame,
                   cols=cols, rows=rows, ydim=NULL, #curdim5=NULL,
                   #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                   regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort=''))
  
  ghgnfsu <- rbind(ghgnfsu[, .(rall, cols, rows, y, value)],
                   ghgnfsu[grepl("SYN|MIN", rows), .( value=sum(value), rows="MINLOSSES"), by=.(rall, cols, y)])
  ghgnfsu <- rbind(ghgnfsu[, .(rall, cols, rows, y, value)],
                   ghgnfsu[grepl("APP|GRA", rows), .( value=sum(value), rows="MANLOSSES"), by=.(rall, cols, y)])
  ghgnfsu <- rbind(ghgnfsu[, .(rall, cols, rows, y, value)],
                   ghgnfsu[grepl("HOU|STO", rows), .( value=sum(value), rows="MMSLOSSES"), by=.(rall, cols, y)])
  ghgnnuts2 <- rbind(ghgnnuts2[, .(rall, cols, rows, y, value)],
                   ghgnnuts2[grepl("SYN|MIN", rows), .( value=sum(value), rows="MINLOSSES"), by=.(rall, cols, y)])
  ghgnnuts2 <- rbind(ghgnnuts2[, .(rall, cols, rows, y, value)],
                   ghgnnuts2[grepl("APP|GRA", rows), .( value=sum(value), rows="MANLOSSES"), by=.(rall, cols, y)])
  ghgnnuts2 <- rbind(ghgnnuts2[, .(rall, cols, rows, y, value)],
                   ghgnnuts2[grepl("HOU|STO", rows), .( value=sum(value), rows="MMSLOSSES"), by=.(rall, cols, y)])
  
  curfile<-paste0(savepath, "/xobs_", reginame, "_", yearrange, "_", toupper(xobsname), "tot", curdate, ".rdata")
  save(list=c(paste0(xobsname,"fsu")), file=paste0(curfile))
  save(list=c(paste0(xobsname,"nuts2")), file=gsub("tot", "nuts2", paste0(curfile)))
  # Save also by country so that it can be loaded individually
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  ghgnfsu <- merge(ghgnfsu, fsu_delimdata[, .(rall=fsuID, CNTR_CODE)])
  saveeachcountry <- FALSE
  if(saveeachcountry){
  for (c in unique(ghgnfsu$CNTR_CODE)){
    assign(paste0(xobsname, "fsu", c), ghgnfsu[CNTR_CODE==c])
    save(list=paste0(xobsname, "fsu", c), file=gsub(".rdata", paste0("_", c, ".rdata"), curfile))
  }}
  
  
  xobsname="livestock"
  cleanup()
  cat("\nExtracting for ", toupper(xobsname), "in", reginame)
  load(loadcurfile(extractdate=curdate, scope="capdistimesLU", xobsname=xobsname, reginame=reginame,
                   cols=NULL, rows=c("1000Ha", "HeadperHa", "1000Head"), ydim=NULL, #curdim5=NULL,
                   regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort=''))
  
  return(ghgnfsu)
  
  }

calcstats2excel <- function(filename, objectname, suffix=""){
  
  load(filename)
  yy <- get(objectname)
  save(list=objects(), file = "y.rdata")
  
  xlsfile <- gsub(".rdata", paste0(suffix, ".xlsx"), filename)
  xlswb <- createWorkbook(creator = "Adrian Leip", title = "Tests for KIP-INCA data", subject = "CAPDIS N-BUDGET and GHG emissions")
  
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  fsu_nuts <- unique(fsu_delimdata[, .(rall=fsuID, CAPRINUTS2, CNTR_CODE)])
  
  rh <- "CAPRINUTS2"; if(rh %in% names(yy)) {yy <- yy[, -rh, with=FALSE]}
  rh <- "CNTR_CODE"; if(rh %in% names(yy)) {yy <- yy[, -rh, with=FALSE]}
  if(! "rows" %in% names(yy)) {yy <- melt.data.table(yy, id.vars = c("rall", "cols", "y"), variable.name = "rows")}
  y <- merge(yy, fsu_nuts)
  
  cat("\nCalculating statistics for UAAR data")
  stats <- y[cols=="UAAR", .( min = min(value),
                              max = max(value),
                              mean = mean(value),
                              std = sd(value),
                              median = median(value)), by=c("CAPRINUTS2", "rows", "y")]
  xlsws <- addWorksheet(xlswb, sheetName = "UAAR_stats")
  xlsws <- writeData(xlswb, sheet = "UAAR_stats", x = stats)
  
  cat("\nCalculating statistics for non-UAAR data")
  stats <- y[cols!="UAAR", .( min = min(value),
                              max = max(value),
                              mean = mean(value),
                              std = sd(value),
                              median = median(value)), by=c("CAPRINUTS2", "rows", "y")]
  xlsws <- addWorksheet(xlswb, sheetName = "nonUAAR_stats")
  xlsws <- writeData(xlswb, sheet = "nonUAAR_stats", x = stats)
  
  cat("\nGetting upper 100 values for parameters")
  for (p in unique(y$rows)){
    upperq <- y[rows==p]
    upperq <- head(upperq[order(value, decreasing = TRUE)], n = 100)
    xlsws <- addWorksheet(xlswb, sheetName = paste0(p, "_top100"))
    xlsws <- writeData(xlswb, sheet = paste0(p, "_top100"), x = upperq)
  }
  
  xlswf <- saveWorkbook(xlswb, file = xlsfile, overwrite = TRUE)
  return(y)
  
}
cleandate4kipinca <- function(filename = NULL){
  
  # Calculates statistics to check distributions
  # Creates a readable data table
  # Cleans very small values adn checks on outliers
  # Puts an upper cap on very high N input
  
  #save(list=objects(), file = "y.rdata")
  y <- calcstats2excel(filename = filename, objectname = 'ghgnfsu', suffix = "_beforecleaning") 
  
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  fsu_nuts <- unique(fsu_delimdata[, .(rall=fsuID, CAPRINUTS2, CNTR_CODE)])
  write.csv(fsu_nuts, file = gsub(".rdata", "_fsu_nuts.csv", filename))
  
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
  #xf <- paste0("NinSOI.gt.", as.character(xlimit))
  #xlsws <- addWorksheet(xlswb, sheetName = xf)
  #xlsws <- writeData(xlswb, sheet = xf, x = xl)
  
  #xlsws <- addWorksheet(xlswb, sheetName = "ATMOSD.gt.80")
  #xlsws <- writeData(xlswb, sheet = "ATMOSD.gt.80", x = nbudget[ATMOSD>80 & cols=="UAAR"])
  
  cat("\nHigh SURSOI. If due to VERY high NMANAP - reduce all down to 400")
  cat("\nFirst correct those that depend on one input term")
  
  sLimit <- 400
  nbudget[SURSOI > sLimit]
  
  xl <- nbudget[SURSOI > sLimit]
#  xf <- paste0(nbfile, "_SURSOI.gt.", sLimit)
#  xlsws <- addWorksheet(xlswb, sheetName = paste0("SURSOI.gt.", sLimit))
#  xlsws <- writeData(xlswb, sheet = paste0("SURSOI.gt.", sLimit), x = xl)
  
  xHigh_SURSOI <- nbudget[SURSOI > sLimit & NMANAP > 400]
  XHigh_Sursoi_correct <- xHigh_SURSOI[SURSOI > sLimit & NMANAP > sLimit, 
                                       `:=` ( SURSOI = 400, 
                                              NMANAP = NMANAP+400-SURSOI, 
                                              NinSOI = NinSOI+400-SURSOI,
                                              RUNSUR = RUNSUR * (NMANAP+NMANGR+400-SURSOI)/(NMANGR+NMANAP),
                                              N2OAPP = N2OAPP * (NMANAP+400-SURSOI)/NMANAP,
                                              NH3APP = NH3APP * (NMANAP+400-SURSOI)/NMANAP,
                                              NOXAPP = NOXAPP * (NMANAP+400-SURSOI)/NMANAP )]
  # NMANAP > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP > sLimit & CRESID < sLimit & NMINSL < sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+400-SURSOI, 
                                  NinSOI = NinSOI+400-SURSOI,
                                  RUNSUR = RUNSUR * (NMANAP+NMANGR+400-SURSOI)/(NMANGR+NMANAP),
                                  N2OAPP = N2OAPP * (NMANAP+400-SURSOI)/NMANAP,
                                  NH3APP = NH3APP * (NMANAP+400-SURSOI)/NMANAP,
                                  NOXAPP = NOXAPP * (NMANAP+400-SURSOI)/NMANAP )]
  # CRESID > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID > sLimit & NMINSL < sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, CRESID = CRESID+400-SURSOI, NinSOI = NinSOI+400-SURSOI)]
  #NMINSL > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID < sLimit & NMINSL > sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, NMINSL = NMINSL+400-SURSOI, NinSOI = NinSOI+400-SURSOI,
                                  RUNMIN = RUNMIN * (NMINSL+400-SURSOI)/NMINSL,
                                  N2OSYN = N2OSYN * (NMINSL+400-SURSOI)/NMINSL,
                                  NH3SYN = NH3SYN * (NMINSL+400-SURSOI)/NMINSL,
                                  NOXSYN = NOXSYN * (NMINSL+400-SURSOI)/NMINSL )]
  #NMANGR > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID < sLimit & NMINSL < sLimit & NMANGR > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, NMANGR = NMANGR+400-SURSOI, NinSOI = NinSOI+400-SURSOI,
                                  RUNSUR = RUNSUR * (NMANAP+NMANGR+400-SURSOI)/(NMANGR+NMANAP),
                                  N2OGRA = N2OGRA * (NMANGR+400-SURSOI)/NMANGR,
                                  NH3GRA = NH3GRA * (NMANGR+400-SURSOI)/NMANGR,
                                  NOXGRA = NOXGRA * (NMANGR+400-SURSOI)/NMANGR )]
  # CRESID > sLimit, others below
  nbudget[SURSOI > sLimit]
  max(nbudget$SURSOI, na.rm=TRUE)
  
  cat("\nThen correct in pairs")
  #NMANAP and NMANGR > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP > sLimit & CRESID < sLimit & NMINSL < sLimit & NMANGR > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR), 
                                  RUNSUR = RUNSUR * (NMANAP+NMANGR+400-SURSOI)/(NMANGR+NMANAP),
                                  N2OAPP = N2OAPP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR))/NMANAP,
                                  NH3APP = NH3APP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR))/NMANAP,
                                  NOXAPP = NOXAPP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR))/NMANAP,
                                  NMANGR = NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR), 
                                  N2OGRA = N2OGRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR))/NMANGR,
                                  NH3GRA = NH3GRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR))/NMANGR,
                                  NOXGRA = NOXGRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR))/NMANGR,
                                  NinSOI = NinSOI+(400-SURSOI))]
  #CRESID and NMINSL > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP < sLimit & CRESID > sLimit & NMINSL > sLimit & NMANGR < sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  CRESID = CRESID+(400-SURSOI)*CRESID/(CRESID+NMINSL), 
                                  NMINSL = NMINSL+(400-SURSOI)*NMINSL/(CRESID+NMINSL), 
                                  RUNMIN = RUNMIN * (NMINSL+(400-SURSOI)*NMINSL/(CRESID+NMINSL))/NMINSL,
                                  N2OSYN = N2OSYN * (NMINSL+(400-SURSOI)*NMINSL/(CRESID+NMINSL))/NMINSL,
                                  NH3SYN = NH3SYN * (NMINSL+(400-SURSOI)*NMINSL/(CRESID+NMINSL))/NMINSL,
                                  NOXSYN = NOXSYN * (NMINSL+(400-SURSOI)*NMINSL/(CRESID+NMINSL))/NMINSL,
                                  NinSOI = NinSOI+(400-SURSOI))]
  #CRESID, NMANAP and NMANGR > sLimit, others below
  xsel <- nbudget[SURSOI > sLimit & NMANAP > sLimit & CRESID > sLimit & NMINSL < sLimit & NMANGR > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID), 
                                  RUNSUR = RUNSUR * (NMANAP+NMANGR+CRESID+400-SURSOI)/(NMANGR+NMANAP+CRESID),
                                  N2OAPP = N2OAPP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID))/NMANAP,
                                  NH3APP = NH3APP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID))/NMANAP,
                                  NOXAPP = NOXAPP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID))/NMANAP,
                                  
                                  NMANGR = NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID), 
                                  N2OGRA = N2OGRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID))/NMANGR,
                                  NH3GRA = NH3GRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID))/NMANGR,
                                  NOXGRA = NOXGRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID))/NMANGR,
                                  CRESID = CRESID+(400-SURSOI)*CRESID/(NMANAP+NMANGR+CRESID), 
                                  NinSOI = NinSOI+(400-SURSOI))]
  nbudget[SURSOI > sLimit]
  max(nbudget$SURSOI, na.rm=TRUE)
  
  cat("\nFinally scale")
  xsel <- nbudget[SURSOI > sLimit]
  nbudget <- nbudget[xsel, `:=` ( SURSOI = 400, 
                                  NMANAP = NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  RUNSUR = RUNSUR * (NMANAP+NMANGR+CRESID+NMINSL+400-SURSOI)/(NMANGR+NMANAP+CRESID+NMINSL),
                                  N2OAPP = N2OAPP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID+NMINSL))/NMANAP,
                                  NH3APP = NH3APP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID+NMINSL))/NMANAP,
                                  NOXAPP = NOXAPP * (NMANAP+(400-SURSOI)*NMANAP/(NMANAP+NMANGR+CRESID+NMINSL))/NMANAP,
                                  
                                  NMANGR = NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  N2OGRA = N2OGRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID+NMINSL))/NMANGR,
                                  NH3GRA = NH3GRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID+NMINSL))/NMANGR,
                                  NOXGRA = NOXGRA * (NMANGR+(400-SURSOI)*NMANGR/(NMANAP+NMANGR+CRESID+NMINSL))/NMANGR,
                                  
                                  CRESID = CRESID+(400-SURSOI)*CRESID/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  
                                  NMINSL = NMINSL+(400-SURSOI)*NMINSL/(NMANAP+NMANGR+CRESID+NMINSL), 
                                  RUNMIN = RUNMIN * (NMINSL+(400-SURSOI)*NMINSL/(NMANAP+NMANGR+CRESID+NMINSL))/NMINSL,
                                  N2OSYN = N2OSYN * (NMINSL+(400-SURSOI)*NMINSL/(NMANAP+NMANGR+CRESID+NMINSL))/NMINSL,
                                  NH3SYN = NH3SYN * (NMINSL+(400-SURSOI)*NMINSL/(NMANAP+NMANGR+CRESID+NMINSL))/NMINSL,
                                  NOXSYN = NOXSYN * (NMINSL+(400-SURSOI)*NMINSL/(NMANAP+NMANGR+CRESID+NMINSL))/NMINSL,
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
  
  # Avoid that equation above creates 'NaN's
  nbudget[NMANGR==0, `:=` (N2OGRA = 0, NH3GRA=0, NOXGRA=0)]
  nbudget[NMANAP==0, `:=` (N2OAPP = 0, NH3APP=0, NOXAPP=0)]
  nbudget[NMINSL==0, `:=` (N2OSYN = 0, NH3SYN=0, NOXSYN=0)]
  
  cat("\nCheck for negative values where there shouldn't be")
  xneg <- nbudget[NinSOI < 0]
#  xlsws <- addWorksheet(xlswb, sheetName = "NinSOInegative")
#  xlsws <- writeData(xlswb, sheet = "NinSOInegative", x = xneg)
  nbudget <- nbudget[NinSOI >= 0]
  
  xneg <- nbudget[NMANGR < 0]
  xneg <- nbudget[NMANAP < 0]
  
  cat("\nSave cleaned data")
  
  save(nbudget, file=paste0(gsub(".rdata", "_dcastclean.rdata", filename)))
  
  saveeachcountry <- FALSE
  if(saveeachcountry){
  for (c in unique(nbudget$CNTR_CODE)){
    assign(paste0("GHGN", c), nbudget[CNTR_CODE==c])
    save(list=paste0("GHGN", c), file=paste0(gsub(".rdata", paste0("_", c, "_dcastclean.rdata"), filename)))
  }}
  
  # NRET timeseries
  nrettimeseries <- dcast.data.table(y[rows=="NRET" | rows=="LEVL"], rall + cols + rows ~ y, value.var="value")
  
  cat("\nCalculating statistics for cleaned data UAAR data")
  y <- calcstats2excel(filename = paste0(gsub(".rdata", "_dcastclean.rdata", filename)), objectname = 'nbudget', suffix = "_aftercleaning") 
  

  return(nbudget)
  
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
                    "\n#     - A shapefile with the geometry of the FSU is available on request at",
                    "\n#       https://jrcbox.jrc.ec.europa.eu/index.php/apps/files/?dir=/afoludata/FSU_database&fileid=9738314RENATE pleaseX.",
                    "\n#     - For a description of crop and livestock activities see file ", fsets,
                    "\n# Variables are as follows:",
                    "\n#        Nitrogen flows relevant for SURSOI",
                    "\n#        Soil budgets terms [kg N / ha / yr] with ha referring to crop activities",
                    "\n#             - NinSOI N inputs to soil as the farmer applies emissions from application have yet to occur",
                    "\n#                      NinSOI = BIOFIX + NMINSL + NMANAP + NMANGR + ATMOSD + CRESID - ",
                    "\n#             - BIOFIX Biological fixation",
                    "\n#             - NMINSL/PMINSL Mineral fertilizer N/P input net of gaseous losses and run-off",
                    "\n#             - NMANAP/PMANAP Manure input of N/P net of all surface losses. Part applied intentionally to agricultural land",
                    "\n#             - NMANGR/PMANGR Manure input of N/P net of all surface losses. Part deposited by grazing animals",
                    "\n#             - ATMOSD Atmospheric deposition",
                    "\n#             - CRESID/PRESID N/P in Crop residues ",
                    "\n#             - SURSOI/PSURSOI Soil surface surplus of N/P: all gaseous emissions from manure and mineral fertilizer as well as runoff already subtracted",
                    "\n#                      SURSOI = NinSOI - NRET",
                    "\n#                      SURSOI = Leaching plus denitrification (N2)",
                    "\n#                      PSURSOI = PMINSL + PMANAP + PMANGR + PRESID - NRET",
                    "\n#             - NRET/PRET   Crop retention of N/P",
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
  
  
  writeLines(paste0("# @CAPRI code: ",svnpath),con)
  writeLines(paste0("# @CAPRI results: ",svnrespath),con)
  writeLines(paste(c("\n#", names(capriversion)), collapse=","), con)
  write.table(capriversion, quote=FALSE, col.names=FALSE, row.names=rep("#", nrow(capriversion)), sep=",", con)
  
  writeLines(paste0("#\n# @Data processing: https://github.com/aleip/capriextract"),con)
  writeLines(paste0("#    Main files: capdistseries_functions.r, functions extractall and reload_and_write and other functions called therein."),con)
  writeLines(paste0("#                extract4kipinca_log.r"),con)
  writeLines("#\n# @author Adrian Leip adrian.leip@ec.europa.eu", con)
  
  writeLines(paste0("#\n# @references Petersen, J.-E., Schroeder, C., Mancosu, E., King, S. 2020 ",
                    "Methodological Proposal and Results for compiling EU-level Spatial Nutrient Condition Accounts. ",
                    "A summary report on pilot EU Nutrient Accounts. EEA, Copenhagen."), con)
  writeLines(paste0("# @references Leip A. Koeble R. 2021 The CAPRI disaggregation. Report in preparation"), con)
  writeLines(paste0("# @references Leip A. Koeble R. Reuter H.I. Lamboni M. Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284 Unpublished data archive"), con)
  writeLines(paste0("# @references Lamboni M. Koeble R. Leip A. 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028"), con)
  writeLines(paste0("# @references Leip A. Marchi G. Koeble R. Kempen M. Britz W. Li C. 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5 73-94. https://doi.org/10.5194/bg-5-73-2008"), con)
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
wrapoverwrite <- function(x, reginame, yearrange, concsvs){
  
  # Write out data
  params<-setdiff(names(x), c("rall", "cols", "y"))
  
  # Filter parameters to save time - no details on gases
  params <- params[! params %in% c(params[grepl("NH3", params)],
                                   params[grepl("NOX", params)],
                                   params[grepl("N2O", params)],
                                   params[grepl("CH4", params)],
                                   params[grepl("N2", params)],
                                   params[grepl("RUN", params)])]
  
  
  xcopy <- copy(x)
  for(p in params){
    datap <- xcopy[, c("rall", "cols", "y", p), with=FALSE]
    setnames(datap, p, "value")
    write1para(datap = datap, p = p, reginame = reginame, yearrange = yearrange)
    writeLines(p, concsvs)
  }
}

writelivestock <- function(x2wf, reginame, concsvs){
  
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
  file.remove(x2wf)
  wrapoverwrite(livestockfsu, reginame = reginame, concsvs = concsvs)
  
  #load(file=gsub("LIVESTOCK", "MAACTLEVL", x2wf))
  #maactlvl <- write1para(maactlevlfsu, "LEVLLIVESTOCK", reginame = reginame, yearrange = yearrange)
  return(livestockfsu)
  
}


dohistos <- function(forhist, filename){
  
  #save(forhist, filename, file="dohistos.rdata")
  params<-setdiff(names(forhist), c("rall", "cols", "y", "CAPRINUTS2", "CNTR_CODE"))
  
  histpath <- paste0(dirname(filename), "/histograms/")
  if(! dir.exists(histpath)) {  dir.create(histpath) }
  
  for (p in params){
    cat(p, "- ")
    dohist <- forhist[cols != "UAAR", p, with=FALSE]
    if(p=="MMSLOSSES"){dohist <- forhist[cols == "UAAR", p, with=FALSE]}
    setnames(dohist, p, "value")
    
    dohist[is.nan(value), value := 0]
    if(sum(dohist$value !=0)>0){
    # Check range to avoid errors if only few entries
    rhist <- range(unique(dohist$value)[!unique(dohist$value)==0])
    if(rhist[2]/rhist[1]>1.5 | rhist[2]/rhist[1]<0){
      dohistc <- dohist[value>quantile(dohist$value, probs = 0.0001) & value<quantile(dohist$value, probs = 0.999)]
      png(paste0(histpath, gsub("histogramsdone.*", paste0(p, "9999.png"), basename(filename))))
      hist(dohistc$value, breaks=100,
           main = paste0("0.001-99.99%ile for ", p),
           xlab = paste0("mean=", round(mean(dohist$value), 2),
                         ", med=", round(median(dohist$value), 2),
                         ", min=",round(min(dohist$value), 2),
                         ", max=",round(max(dohist$value), 2)))
      dev.off()
      
      # dohistc <- dohist[value>quantile(dohist$value, probs = 0.001) & value<quantile(dohist$value, probs = 0.999)]
      # png(paste0(histpath, gsub("histogramsdone.*", paste0(p, "999.png"), basename(filename))))
      # hist(dohistc$value, breaks=100,
      #      main = paste0("0.1-99.9%ile for ", p),
      #      xlab = paste0("mean=", round(mean(dohist$value), 2),
      #                    ", med=", round(median(dohist$value), 2), 
      #                    ", min=",round(min(dohist$value), 2),
      #                    ", max=",round(max(dohist$value), 2)))
      # dev.off()
      
      dohistc <- dohist[value>quantile(dohist$value, probs = 0.01) & value<quantile(dohist$value, probs = 0.99)]
      png(paste0(histpath, gsub("histogramsdone.*", paste0(p, "99.png"), basename(filename))))
      hist(dohistc$value, breaks=100,
           main = paste0("1-99%ile for ", p),
           xlab = paste0("mean=", round(mean(dohist$value), 2),
                         ", med=", round(median(dohist$value), 2),
                         ", min=",round(min(dohist$value), 2),
                         ", max=",round(max(dohist$value), 2)))
      dev.off()
      
      dohistc <- dohist
      png(paste0(histpath, gsub("histogramsdone.*", paste0(p, ".png"), basename(filename))))
      hist(dohistc$value, breaks=100,
           main = paste0("Distribution of ", p),
           xlab = paste0("mean=", round(mean(dohist$value), 2),
                         ", med=", round(median(dohist$value), 2), 
                         ", min=",round(min(dohist$value), 2),
                         ", max=",round(max(dohist$value), 2)))
      dev.off()
      dohistc <- dohist
      png(paste0(histpath, gsub("histogramsdone.*", paste0(p, ".png"), basename(filename))))
      hist(dohistc$value, breaks=100, log='y',
           main = paste0("Distribution of ", p),
           xlab = paste0("mean=", round(mean(dohist$value), 2),
                         ", med=", round(median(dohist$value), 2), 
                         ", min=",round(min(dohist$value), 2),
                         ", max=",round(max(dohist$value), 2)))
      dev.off()
    }
    }
  }
  
}

checkregions <- function(folderdate='20191010', savedate='20191011'){
  
  if(is.null(savedate)){savedate <- folderdate}
  kipf <- paste0("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/", folderdate, "_", project, "/xobs_EU27_", yearrange, "_")
  
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
  nbpath <- paste0("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/", folderdate, "_", project, "/")
  nbfile <- paste0(nbpath, "xobs_EU27_", yearrange, "_NBUDGET_", date2load)
  write.csv(nbudcntr[cols=="UAAR"], paste0(nbfile, "_UAAR-NUTS0.csv"), na = "", row.names = FALSE)
  
  
}

