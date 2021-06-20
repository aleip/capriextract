checkwd <- function(){
  # Check working directory and set it to the capriextract folder
  curwd <- 
  curMachine <<- Sys.info()[4]
  curUser <<- Sys.info()[8]
  
  if(! grepl("capriextract", getwd())){
    if(Sys.info()[4] == "D01RI1600881") setwd("x:/adrian/tools/rprojects/capriextract/")
  }
  
}  

checkflparts<-function(i, x, flparts, nref){
  #cat("\n", flparts[[x]][i], "=", flparts[[1]][i])
  if(i > length(flparts[[x]])){
    xok <- FALSE
  }else if(i > length(flparts[[1]])){
    xok <- FALSE
  }else{
    xok <- flparts[[1]][i]==flparts[[x]][i]
  }
  if(x == nref) xok <- TRUE
  #cat("\n", x, i, xok)
  return(xok)
  
}

dcastformula <- function(x, rh, elim=""){
  
  rh <- "n"
  lh <- setdiff(names(x), c("value", rh))
  print(names(x))
  print(lh)
  f <- as.formula(paste0(paste(lh, collapse = " + "), " ~ ", paste(rh, collapse = " + ")))
  return(f)
}

getfilesfromfolder<-function(curfol = datapath, pattern='res.*.gdx$', flag = "", reference=NULL){
  
  # Change default datapath in global environment to curfol
  resultpath <<- curfol
  
  # Unique (daily) identifier of results
  flag <- paste0(flag, format(Sys.time(), "%Y%m%d"))
  
  fls <- list.files(path=curfol, 
                    pattern=pattern, 
                    recursive=FALSE, 
                    full.names = TRUE)
  
  flsn <- list.files(path=curfol, 
                     pattern=pattern, 
                     recursive=FALSE, 
                     full.names = FALSE)
  # Specific changes due to "_" in name elements
  flsn <- gsub("globiom_eu_2018\\+REFERENCE\\+REFERENCE", "globiomeu2018", flsn)
  
  
  return(fls)
}

ExtractShortCommonName <- function(fls, reference = NULL){
  
  flsn <- basename(fls)
  flsn <- gsub("globiom_eu_2018\\+REFERENCE\\+REFERENCE", "globiomeu2018", flsn)
  
  
  nfls <- length(flsn)
  if(! is.null(reference)) {nref <- which(flsn == reference)}else{nref<-nfls+1}
  flparts <- strsplit(flsn, "_")
  
  flsnparts <- Reduce(max, lapply(flparts, length))
  
  ok<-1; i<-1; ndiff<-0; diffflt <- list(); scenshort<-vector(); 
  commonpart<-vector(); ncomm<-0
  #cat("\n ", flsnparts)
  for (i in 1:flsnparts){
    
    #cat(" ", i)
    ok <- Reduce(prod, lapply(1:nfls, function(x) checkflparts(i, x, flparts, nref)))
    if(ok==0){
      
      ndiff<-ndiff+1
      for(j in 1:nfls){ 
        scenshort[j] <- flparts[[j]][i]
        if(is.na(flparts[[j]][i])) scenshort[j]<-0
      }
      diffflt[[ndiff]] <- scenshort
      
      
    }else{
      ncomm <- ncomm+1
      commonpart[ncomm] <- flparts[[1]][i]
    }
  }
  for(j in 1:nfls){
    scenshort[j] <- Reduce(paste0, lapply(1:ndiff, function(x) diffflt[[x]][j]))
    if(scenshort[j] == paste(rep("0", ndiff), collapse="")) { scenshort[j] <- 'ref'}
  }
  
  scenshort <- gsub("\\.gdx","",scenshort)
  scenshort <- gsub("\\.gdx","",scenshort)
  commonname <- gsub("\\.gdx","",paste(commonpart, collapse="_"))
  commonname <<- commonname
  return(list(scenshort, commonname))  
  
}
ExtractShortName <- function(fls, reference = NULL){
  scenshort <- ExtractShortCommonName(fls, reference=NULL)[[1]]
  return(scenshort)
}
ExtractCommonName <- function(fls, reference = NULL){
  scenshort <- ExtractShortCommonName(fls, reference=NULL)[[2]]
  return(scenshort)
}


getcapriversion<-function(){
  capriversion <- as.data.frame(matrix(nrow=1, ncol=6))
  colnames(capriversion) = c("CAPRI_task", "Date", "Revision", "FilesOutput","Branch", "Note")
  capriversion[1,] <- c("CAPREG-timeseriesGHG", "20180711", "7247", "res_%BAS%%MS%.gdx", "ecampa3", "BAS=Base year, MS=Member State") 
  capriversion[2,] <- c("CAPREG-12", "20181106", "7503", "res_time_series_GHG_%MS%.gdx'", "epnf", "MS=Member State") 
  capriversion[3,] <- c("CAPDIS-1212", "20181107", "7503", "xobs_2_%MS%_%BAS%%BAS%", "epnf", "BAS=Base year, MS=Member State") 
  capriversion[4,] <- c("CAPDIS-12-2xxx", "20181121", "7538", "xobs_2_%MS%_%BAS%%Y%", "epnf", "BAS=Base year 2 digits, Y=Simulation year 4 digits") 
  return(capriversion)
}
getfedm<-function(curcountry){
  
  #20170708 - Extraction of feed data to send to Olga Gavrilova [oggavrilova@gmail.com]
  #           For IPCC2019 refinement
  
  datafile<-"res_12"
  datafile<-paste0(datafile,curcountry,".gdx")
  datafile<-paste0(datapath,datafile)
  
  
  if(file.exists(datafile)){
    capridat<-rgdx.param(datafile,dataparm)
    names(capridat)<-c("RALL","COLS","ROWS","Y","VALUE")
    
    #Activities: maact and daact
    capridat<-capridat[capridat$COLS%in%c("FEDM"),]
    
    
  }else{
    cat("\nFile ",datafile," does not exist!")
  }
}


getmeta<-function(curcountry,curyear){
  if(scope%in%c("feed_marketbal","activities") | grepl("baseyear",scope)){
    datafile<-paste0("res_",curyear)
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,"capreg/",datafile)
  }
  if(grepl("nbalance|tseries_marketbal",scope)){
    datafile<-paste0(curcountry,"_",curyear,".gdx")
    datafile<-paste0(datapath,datafile)
  }
  caprimeta<-rgdx.set(datafile,"META",ts=TRUE,te=TRUE)
}


getmarketbalance<-function(capridat,d,curyear){
  rows<-c(as.character(frows$i),as.character(ico$i))
  cols<-c(as.character(frmbal_cols$i),as.character(mrkbal_cols$i))
  cols<-c(as.character(frmbal_cols$i),"FEDM")
  capridat<-filteropen(capridat,cols,rows,regi,d,curyear)
  return(capridat)
}

getfeed<-function(capridat,d,curyear){
  rows<-feed_rows
  cols<-c(maact,daact)
  capridat<-filteropen(capridat,cols,rows,regi,d,curyear)
}
getfeed<-function(capridat,d,curyear){
  rows<-feed_rows
  cols<-c(maact,daact)
  capridat<-filteropen(capridat,cols,rows,regi,d,curyear)
}
getmpact<-function(capridat,d,curyear){
  rows<-"LEVL"
  cols<-c(mpact)
  capridat<-filteropen(capridat,cols,rows,regi,d,curyear)
}
getuaarlevl<-function(capridat,d){
  cols<-"UAAR"
  rows<-"LEVL"
  capridat<-filteropen(capridat,cols,rows,regi,d)
}
getnbil<-function(capridat,d){
  cols<-c(as.character(nbil$i))
  if(scope=="nbalancemain")cols<-c("MINFER","EXCRET","ATMOSD","BIOFIX","CRESID",
                                   "SURTOT","EXPPRD","SURSOI","GASTOT","RUNTOT")
  rows<-"NITF"
  capridat<-filteropen(capridat,cols,rows,regi,d)
}

getdata<-function(scope,curcountry,curyear,curcols,currows=currows){
  
  capridat<-opendata(scope, curcountry, curyear)
  
  if(scope=="feed_marketbal"){
    capridat<-getmarketbalance(capridat,d,curyear)
    capridat<-rbind(capridat,getfeed(capridat,d,curyear))
  }
  if(scope=="activities"){
    capridat<-getmpact(capridat,d,curyear)
  }
  if(scope=="tseries_marketbal"){
    capridat<-getmarketbalance(capridat,d)
  }
  if(grepl("nbalance",scope)){
    capridat<-getnbil(capridat,d)
    capridat<-rbind(capridat,getuaarlevl(capridat,d))
  }
  if(grepl("nlca",scope)){
    capridat<-filteropen(capridat,curcols,currows,regi,d,curyear)
  }
  if(scope%in%c("baseyearnmin","baseyearpmin")){
    capridat<-filteropen(capridat,reload = 0,
                         cols = mcact,rows = currows, ydim = "Y", 
                         regi = srnuts2, curdim5 = NULL,curyear = curyear)
  }
  return(capridat)
}


getmultipleyears<-function(scope,cntr,curyears){
  
  
  if(curyears[1]==""){
    capridat<-Reduce(rbind,lapply(cntr,function(x) getdata(scope,x)))
    caprimeta<-Reduce(rbind,lapply(cntr,function(x) getmeta(x)))
  }else{
    cat("\n Retrieve data")
    capridat<-Reduce(rbind,
                     lapply(curyears,function(y) 
                       Reduce(rbind,
                              lapply(cntr,function(x) getdata(scope,curcountry = x,curyear = y,currows = currows)))
                     ))
    cat("\n Retrieve meta")
    caprimeta<-Reduce(rbind,
                      lapply(curyears,function(y) 
                        Reduce(rbind,
                               lapply(cntr,function(x) getmeta(curcountry = x,curyear = y)))
                      ))
  }
  return(list(capridat,caprimeta))
}

checkaggvsdet<-function(x, aggs, dets, listdetails="error"){
  
  # Compares N budget aggregates vs sum of detailed positions
  # x is a data frame or data table with two columns.
  #   column 1 should contain the element names and column 2 the values
  
  # ------- listdetails --------
  # all = all output given
  # error = only errors (missing elements and mismatch)
  # mismatch = only mismatches
  
  x<-as.data.table(x)
  names(x)<- c("x","y")
  miss<-""
  fail0<-0
  fail<-0
  
  if(sum(x$x%in%aggs)==0){    fail0<-1  }
  if(sum(x$x%in%dets)==0){   
    if(fail0 == 1) {  # There is no left nor right hand side ==> value==0     
      sumaggs<-0
      sumdets<-0
      misaggs<-""
      misdets<-""
    } else { fail <- 1 }            # Detailed flows are missing
  }else{
    if( fail0 == 1) {fail <- 2      # Aggregated flow is missing
    }else{ # Both detailed and aggregated flows available
      misaggs<-paste(aggs[!which(aggs%in%x$x)], collapse="+")
      misdets<-paste(dets[!which(dets%in%x$x)], collapse="+")
      sumaggs<-round(sum(x$y[x$x%in%aggs]),5)
      sumdets<-round(sum(x$y[x$x%in%dets]),5)
      if(sumaggs!=sumdets){fail<-3}  # Mismach between aggregated flow and sum of detailed flows
    }
  }
  
  if(fail==0){
    if(listdetails == "all") {
      cat("\n ", crop, ": ", paste(aggs,collapse="+"), " = ",paste(dets,collapse="+") ," = ", sumdets)
      if(misaggs!="") cat("\n Missing elements: ",misaggs)
      if(misdets!="") cat("\n Missing elements: ",misdets)
    }else if(listdetails != "mismatch"){
      cat(" .. OK")
    }
  }
  if(listdetails != "mismatch"){
    if(fail == 1 ) cat("\n ", crop, ": There is no ",dets," in the data set!")
    if(fail == 2 ) cat("\n ", crop, ": There is no ",aggs," in the data set!")
    if(fail == 1 | fail== 2) cat("\n ", crop, ": Cannot carry out check!", aggs, " = SUM(",paste(dets,collapse=", ") ,")")
  }
  if(fail == 3) {
    cat("\n ", crop, ": Mismach!! ", paste(aggs,collapse="+"), " = ",paste(dets,collapse="+"))
    cat("\n ", paste(aggs,collapse="+") ," = ", sumaggs,
        "; ", paste(dets,collapse="+") ," = ", sumdets,
        ";  Difference = ", sumaggs-sumdets)
    if(length(aggs)>1){cat("\n ");for(z in aggs){cat(z, " = ", x$y[x$x==z])}}
    if(length(aggs)>1 & length(dets)>1){cat("; ")}
    if(length(dets)>1){cat("\n");for(z in dets){cat("  ",z, " = ", x$y[x$x==z])}}
    # if(sumaggs>sumdets){ #List values of aggs
    #   for(z in aggs){cat("\n ", z, " = ", x$y[x$x==z])}
    # }else{
    #   for(z in dets){cat("\n ", z, " = ", x$y[x$x==z])}
    # }
  }
  if(listdetails=="details"){
    for(z in c(aggs,dets)){cat("\n ", z, " = ", x$y[x$x==z])}
  }
}

checkCropNbudget<-function(x, crop, output="error"){
  
  if(output != "mismatch"){
    cat("\n Check N-budget for ", crop)
  }
  nflows_crop<-filteropen(reload=0, 
                          capridat=x, 
                          cols=crop,
                          rows=currows, 
                          curdim5 = NULL,
                          regi=curcountry, 
                          ydim = "2030"
  )
  
  
  capridat<-nflows_crop[,c("ROWS","VALUE")]
  #x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]<-28/44 * x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]
  
  checkaggvsdet(capridat, "SURSOI",c("LEACHI", "DENITR"), output)
  checkaggvsdet(capridat, "N2ONOTH",c("N2ONDEP", "N2ONCRO", "N2ONHIS"), output)
  checkaggvsdet(capridat, c("SURTOT", "EXPPRD"),c("IMPORT", "N2ONOTH"), output)
  checkaggvsdet(capridat, "SURTOT",c("SURSOI", "N2ONOTH",
                                     "GASMAN","GASAPP", "GASGRA", "GASMIN", 
                                     "RUNMAN","RUNAPP", "RUNGRA", "RUNMIN"), output)
  checkaggvsdet(capridat, "IMPORT", c("ATMOSD", "CRESID", "BIOFIX", "MINSAT", "MINFER", "EXCRET"), output)
  checkaggvsdet(capridat, "EXCRET", c("MANAPP", "MANGRA", "GASMAN", "RUNMAN"), output)
  checkaggvsdet(capridat, "MANGRA", c("NMANGR", "GASGRA", "RUNGRA"), output)
  checkaggvsdet(capridat, "MANAPP", c("NMANAP", "GASAPP", "RUNAPP"), output)
  checkaggvsdet(capridat, "NMAN", c("NMANAP", "NMANGR"), output)
  
}

checkduration <- function(){
  # Check speed of capdis_timeseries
  require(openxlsx)
  source("R/initializecapri.R")
  currun <- "cur_run.gms"
  InitCapriEnv(capri.runfile = currun, addcomment = "KIP-INCA run")
  
  xobsfiles <- data.table(regions=list.files(path = paste0(cenv$resout, "capdis/xobstseries/"), pattern = "xobs.*2012.*gdx", full.names = FALSE))
  xobsfiles[, ctime12 := file.info(paste0(cenv$resout, "capdis/xobstseries/", regions))$mtime ]
  xobsfiles[, ctime18 := file.info(paste0(cenv$resout, "capdis/xobstseries/", gsub("2012", "2018", regions)))$mtime ]
  xobsfiles[ctime18>ctime12, tdiff_hours := as.numeric(ctime18 - ctime12)/60]
  hist(as.numeric(xobsfiles[!is.na(tdiff_hours)]$tdiff_hours), breaks = seq(0, ceiling(max(xobsfiles$tdiff_hours, na.rm = TRUE)), 0.5))
  head(xobsfiles[order(tdiff_hours, decreasing = TRUE)], 20)
  as.numeric(max(xobsfiles$ctime18, na.rm = TRUE)-min(xobsfiles$ctime12, na.rm = TRUE))*24
  write.xlsx(xobsfiles[order(tdiff_hours, decreasing = TRUE)], 
             file=paste0(cenv$resout, "capdis/xobstseries/duration_of_runs", format(Sys.time(), "%Y%m%d"), ".xlsx"))
  
}
