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


opendata<-function(scope,
                   curcountry,
                   curyear, 
                   baseyear='12',  # Required for scope capdistime and capmod
                   curscen=''      # Required for capmod
                   , curscenshort=''
){
  #' Open a Capri data file
  #' @description This function opens a Capri gdx data file also called a xobs file. 
  #' Depending on the scope parameter, conditional statements change the bahaviour of the opendata function. 
  #' @param scope character variable used to adapt the behaviour of the function to different input file formats and locations.
  #' The following 'scope's are currently defined:
  #' - feed_marketbal
  #' - activities
  #' - nbalance
  #' - tseries
  #' - nlca
  #' - lapm
  #' - capdisreg
  #' - capdistime: Timeseries for disaggregated data. 
  #'               curyear needs to be full year (e.g. 2012)
  #'               Requires 'baseyear' as additional parameter defined in the global environment (e.g. 12)
  #' 
  #' @param curcountry character iso2 country code
  #' @param curyear numeric year
  #' @return data frame
  #' @export
  #' 
  # Check if datafile contains already a path
  if(grepl(":", curscen) | grepl("jrciprap246p", curscen)) {
    pathincluded <- 1
    datapath <- ""
    datafile <- curscen
  }else{pathincluded <- 0}
  
  if(scope%in%c("feed_marketbal","activities") | grepl("baseyear",scope)){
    datafile<-paste0("res_",curyear)
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,"capreg/",datafile)
    dataparm<-"DATA2"
    ydim<-"Y"
  }
  if(grepl("nbalance|tseries",scope)){
    datafile<-paste0(curcountry,"_12.gdx")
    datafile<-paste0(datapath,datafile)
  }
  if(grepl("tseriesGHG", scope)){
    datafile<-paste0("Capreg_tseries/GHGperCountry/")
    datafile<-paste0(datafile, "res_time_series_GHG_", curcountry, ".gdx")
    datafile<-paste0(datapath, datafile)
    dataparm <- 'DATA2'
    datanames <- data4dim
  }
  if(grepl("nlca",scope)){
    datafile<-paste0("capmod/res_2_0830",curscen,".gdx")
    datafile<-paste0(datapath,datafile)
  }
  if(grepl("capmod",scope)){
    
    # Check if subfolde
    
    if(pathincluded == 0) datafile<-paste0("capmod/res_2_", baseyear, curyear,curscen,".gdx")
    datafile<-paste0(datapath, datafile)
    #datafile<-paste0(datapath,"/", datafile)
    dataparm<-"dataout"
    datanames<-c("rall","empty","cols","rows","y","value")
    
  }
  if(grepl("lapm", scope)){
    datafile<-paste0(cgams, "../dat/capdishsu/fssdata/")
    if(curyear=="_") curyear<-""
    datafile<-paste0(datafile, "capdis_", curcountry, "_10GRID", curyear, ".gdx")
    dataparm<-"p_capdis"
    ydim<-NULL
    datanames<-c("RALL", "ROWS", "COLS", "VALUE")
    
    if(curyear=="preds"){
      datafile<-paste0(cgams, "../dat/capdishsu/lapm/")
      datafile<-paste0(datafile, substr(curcountry, 1, 2), "_lapmpreds.gdx")
      dataparm<-"lapmpreds"
      ydim<-NULL
      datanames<-c("RALL", "COLS", "VALUE")
    }
  }
  if(grepl("capdis",scope)){
    if(scope=="capdis")       dataf<-paste0("capdis/xobs_2_",curcountry,"_",baseyear, baseyear)
    if(scope=="capdiscapreg") dataf<-paste0("capdis/xobs_2_",curcountry,"_",baseyear, baseyear)
    if(scope=="capdistimes")  dataf<-paste0("xobs_2_",curcountry,"_",baseyear,curyear)
    #if(scope=="capdistimes") datapath<-paste0(d5space, "capdis_results/20181121_timeseries/")
    if(grepl("capdistimes", scope))  dataf<-paste0("capdis/xobstseries/xobs_2_",curcountry,"_",baseyear,curyear)
    datafile<-paste0(datapath,dataf,".gdx")
    dataparm<-"xobs"
    ydim<-""
    datanames<-c("rall", "cols", "rows", "value")
    data4dim <- c("rall", "cols", "rows", "y", "value")
  }
  if(file.exists(datafile)){
    cat("\n ",datafile)
    #d<-list(datafile,dataparm,datanames,ydim)
    #Wrap a 'try' around in case there is a problem with the gdx file
    capridat <- NULL
    try(capridat<-as.data.table(rgdx.param(datafile,dataparm)), silent = TRUE)
    if(! is.null(capridat)){
      names(capridat)<-datanames
      if(scope=="capdistimes") {
        capridat$y <- curyear
        capridat<-capridat[,data4dim, with=FALSE]
      }
      if(grepl("lapm", scope)){
        capridat$y<-gsub("_","",curyear)
        if(curyear=="")capridat$y<-"2010"
        #capridat$NUTS2<-curcountry
        if(curyear=="preds"){
          capridat$ROWS<-"LEVL"
        }
        #capridat<-capridat[, c("NUTS2", data4dim)]
      }
      #setattr(capridat,paste0(datafile,n),datafile)
      fattr<-data.frame(nrow=1)
      f<-gsub(".*/","", datafile)
      fattr$filename[1]<-f
      fattr$filepath[1]<-gsub(f, "", datafile)
      fattr$filemtime[1]<-as.character(file.mtime(datafile))
    }else{
      cat("\n", datafile, " cannot be opened\n")
      capridat<-datafile
      fattr <- 0
    }
  }else{
    cat("\n", datafile, " does not exist\n")
    capridat<-datafile
    fattr <- 0
  }
  
  return(list(capridat,fattr))
}

# filteropen<-function(scope, reload=0, capridat=capridat, cols=curcols, 
#                      rows=currows, ydim="Y", curdim5=NULL,regi, 
#                      curcountry, curyear="08", baseyear='08', 
#                      curscen='', curscenshort=''){
#   
#   if(reload==1){
#     capridat<-opendata(scope,curcountry,curyear,baseyear, curscen, curscenshort)
#     fattr<-capridat[[2]]
#     if(fattr[1] == 0) return(0)
#     capridat<-capridat[[1]]
#   }
#   capridat<-as.data.table(capridat)
#   #View(capridat)
#   
#   fattr$filterCOLS<-paste(cols, collapse="-")
#   fattr$filterROWS<-paste(rows, collapse='-')
#   fattr$filterCountry<-paste(curcountry, collapse="-")
#   fattr$filterRegi<-paste(regi, collapse="-")
#   
#   #COLS (activities, variables for products)
#   if(!is.null(cols)) capridat<-capridat[capridat$cols%in%cols,]
#   
#   #ROWS (products, variables for activities)
#   if(!is.null(rows)) capridat<-capridat[capridat$rows%in%rows,]
#   
#   #Filter regional level 
#   if(!is.null(regi)){
#     if(length(regi)==1){  
#       if(regi=="HSU"){
#         capridat<-capridat[grepl("U[1-9]",capridat$rall),]   
#         capridat<-capridat[! grepl("HU[1-9]",capridat$rall),]   
#       }
#     }else{
#       capridat<-capridat[capridat$rall%in%regi,]
#     }
#   }
#   
#   #Filter time dimension only if 
#   if(!scope%in%c("capdistimes","capmod", "tseriesGHG", "lapm")){
#     if(ncol(capridat)>4){
#       if(exists("ydim")) capridat<-capridat[capridat$Y%in%as.character(ydim),]
#       if(curyear != ""){
#         if(!grepl("^20",curyear)){curyear<-paste0("20",curyear)}
#         capridat$Y<-curyear
#       }
#     }
#   }
#   
#   if(scope%in%c("tseriesGHG")){
#     capridat<-capridat[capridat$y%in%as.character(curyear)]
#   }
#   #capridat<-capridat[,setdiff(names(capridat),c("Y"))]
#   if(!is.null(curdim5)){
#     #print("select curdim5")
#     if(curdim5[1]=="nonempty"){
#       capridat<-capridat[capridat$empty!='',]
#     }else{
#       capridat<-capridat[capridat$empty%in%curdim5,]
#     }
#   }
#   if(grepl("capmod", scope)){
#     capridat$scen<-curscenshort
#   }
#   #print(attributes(capridat))
#   #cat("-->")
#   #print(fattr)
#   # Return of information fattr does work over list as this disturbs the 'Reduce' function
#   #return(capridat)  #xavi20190122: I cannot see where the limitation of returning a list is.
#   #                                 It works perfectly, at least for mbal plots.
#   #                                 In case it's absolutely necessary to return only capridat, 
#   #                                 it needs to be addapted 'filtermultiple' (and maybe others)
#   #                                 in order to avoid trying to read 'fattr' 
#   return(list(capridat, fattr))
# }

# filtermultiple<-function(scope, 
#                          cols=curcols, rows=currows, ydim="Y", curdim5=NULL, regi, 
#                          curcountries, curyears="08", baseyear='08', curscens='', curscensshort='',
#                          resultfile=NULL){
#   nfiles<-length(curcountries)*length(curscens)*length(curyears)
#   cat("\n", length(curcountries), curcountries, length(curscens), length(curyears), nfiles)
# 
#   # for(x in 1:max(1,length(curyears))){
#   #   for(y in max(1,1:length(curcountries))){
#   #     for(z in 1:max(1,length(curscens))){
#   #       cat("\n", x,curyears[x], y,curcountries[y], z,curscens[z])
#   #     }}}
#   # return()
#   cdat<-list()
#   #fdat<-data.frame(nrow=1)
#   fdat<-data.frame(nrow=0)
#   n<-0
#   for(x in 1:max(1,length(curyears))){
#     for(y in max(1,1:length(curcountries))){
#       for(z in 1:max(1,length(curscens))){
#         n<-n+1
#         capridat<- filteropen(scope, 
#                                reload=1, 
#                                # Filtering options
#                                cols=cols, 
#                                rows=rows,
#                                ydim=ydim, 
#                                curdim5=curdim5,
#                                regi=regi,
#                                # Opening options
#                                curcountry = curcountries[y], 
#                                curyear = curyears[x],
#                                baseyear = baseyear,
#                                curscen = curscens[z],
#                                curscenshort = curscensshort[z]
#         )
#         cdat[[n]]<-capridat[[1]]
#         #View(cdat[[n]], as.character(n))
#         #cdat[[n]]<-capridat   #xavi20190122: filteropen no longer produces a list (commit d327009bdb2319d3ee3e20c3eec2f1ed143b4d6b)
#                                #xavi20190122_2: at the end, it keeps returning a list (see comment in L184)
#         #print(str(capridat[[1]]))
#         #cat("print from filter")
#         #print(capridat[[2]])
#         if(length(capridat)>1){
#           if(n==1){
#             fdat<-capridat[[2]]
#           }else{
#             fdat<-rbind(fdat, capridat[[2]])
#           }
#         }else{
#           n <- n-1
#         }
#       }
#     }
#   }
#   
#   if(! exists("commonname")) commonname <- ""
#   if(! exists("flag")) flag <- paste0("temp_", paste(curcountries, collapse = ""), scope)
#   capridat<-Reduce(rbind, cdat)
#   #print(capridat)
#   info <<- fdat
#   caprid <<- capridat
# 
#   if(! is.null(resultfile)) {
#     cat("\nSave to ", resultfile)
#     save(caprid, info, file=paste0(resultfile, ".rdata"))
#   }
#   
#   return(list(capridat, fdat))
# }

convertarguments2values<-function(f=NULL, ...){
  # Function that helps debugging - converts all arguments into values
  #          in the global environment so that the function can
  #          be checked directly line by line
  
  l<-as.list(match.call())
  save(l, file="l.rdata")
  for (i in 2:length(l)){
    #cat("\n", i, "Assigning ", l[[i]], " to ", paste(names(l)[i],collapse="") , "...")
    cat("\nstart",i,":", names(l)[i])
    n<-names(l)[i]
    if(! exists("n")){
      cat("\nNo argument name given for ", l[[i]],". Tentatively set to ",l[[i]])
      n<-l[[i]]
    }
    g<-eval(l[[i]])
    cat(". ", n, " is character", g)
    assign(n, g, envir=.GlobalEnv)
  }
  #print(as.list(match.call()))
  # Get all arguments requested and their default values
  if(! is.null(f)) {
    a <- formals(f)
    na <- names(a)
    va <- as.character(unlist(a))
    
  }
  
  return(l)
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







