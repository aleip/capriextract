#' Wrapper to open multiple CAPRI gdx result files and filter for relevant variables
#' @description The function \code{filtermultiple()} is a function that wraps around the
#' function \code{filteropen()} which itself is a wrapper around the 
#' function \code{opendata} which opens one gdx-resultfile, with hard-coded settings depending on 
#' the 'scope' (capmod, capdis etc.) and further options.
#' @param scope 
#' @param cols 
#' @param rows 
#' @param ydim 
#' @param curdim5 
#' @param regi 
#' @param curcountries 
#' @param curyears 
#' @param baseyear 
#' @param curscens 
#' @param curscenshort 
#' @param resultfile 
#' @return List of two data tabes: (i) the combined capri result data table containing 
#' the data from all files openend and (ii) information about the data files.
#' @examples \dontrun{
#' caprirunfile <- "x:\\dev\\epnf\\gams\\fortran.gms"
#' InitCapriEnv(caprirunfile)
#' str(c)
#' }
#' @export


filtermultiple<-function(scope, 
                         cols=curcols, rows=currows, ydim="Y", curdim5=NULL, regi=NULL, 
                         curcountries, curyears="12", baseyear='12', curscens='', curscensshort='',
                         resultfile=NULL){
  
  nfiles<-length(curcountries)*length(curscens)*length(curyears)
  #cat("\n", length(curcountries), curcountries, length(curscens), length(curyears), nfiles)
  
  if(length(curscensshort) == 1){
    # They get all the name - copy to vector of length curscens
    curscensshort <- rep(curscensshort, length(curscens))
  }
  cdat<-list()
  fdat<-data.frame(nrow=0)
  n<-0
  for(x in 1:max(1,length(curyears))){
    for(y in 1:max(1,1:length(curcountries))){
      for(z in 1:max(1,length(curscens))){
        n<-n+1
        #cat("\n filtermultiple curcountries", curcountries)
        capridat<- filteropen(scope, 
                              reload=1, 
                              # Filtering options
                              curcols=cols, 
                              currows=rows,
                              ydim=ydim, 
                              curdim5=curdim5,
                              regi=regi,
                              # Opening options
                              curcountry = curcountries[y], 
                              curyear = curyears[x],
                              baseyear = baseyear,
                              curscen = curscens[z],
                              curscenshort = curscensshort[z]
        )
        capridat[[1]]$n <- n
        cdat[[n]]<-capridat[[1]]
        if(length(capridat)>1){
          if(n==1){
            fdat<-capridat[[2]]
          }else{
            fdat<-rbind(fdat, capridat[[2]])
          }
        }else{
          n <- n-1
        }
      }
    }
  }
  
  if(! exists("commonname")) commonname <- ""
  if(! exists("flag")) flag <- paste0("temp_", paste(curcountries, collapse = ""), scope)
  capridat<-Reduce(rbind, cdat)
  info <- fdat
  caprid <- capridat
  
  if(! is.null(resultfile)) {
    cat("\nSave to ", resultfile)
    save(caprid, info, file=paste0(resultfile, ".rdata"))
  }
  
  return(list(capridat, fdat))
}

filteropen<-function(scope, reload=0, capridat=capridat, curcols=NULL, currows=NULL,
                     ydim="Y", curdim5=NULL,regi=NULL, 
                     curcountry, curyear="12", baseyear='12', 
                     curscen='', curscenshort=''){
  #cat("\n filteropen: curcountry", curcountry)
  if(reload==1){
    capridat<-opendata(scope,curcountry,curyear,baseyear, curscen, curscenshort)
    fattr<-capridat[[2]]
    if(fattr[1] == 0) return(0)
    capridat<-capridat[[1]]
  }
  capridat<-as.data.table(capridat)
  #View(capridat)
  #save(list=objects(), file="test105.rdata")
  
  save(list=objects(), file="t.rdata")
  fattr$filterCOLS<-paste(curcols, collapse="-")
  fattr$filterROWS<-paste(currows, collapse='-')
  fattr$filterCountry<-paste(curcountry, collapse="-")
  fattr$filterRegi<-paste(regi, collapse="-")
  
  #COLS (activities, variables for products)
  if(!is.null(curcols)) capridat<-capridat[cols%in%curcols,]
  
  #ROWS (products, variables for activities)
  #save(list=objects(), file="test.rdata")
  if(!is.null(currows)) capridat<-capridat[rows %in% currows]
  
  #Filter regional level 
  if(!is.null(regi)){
    if(length(regi)==1){  
      if(regi=="HSU"){
        capridat<-capridat[grepl("U[1-9]",capridat$rall),]   
        capridat<-capridat[! grepl("HU[1-9]",capridat$rall),]   
      }
    }else{
      capridat<-capridat[capridat$rall%in%regi,]
    }
  }
  
  #Filter time dimension only if 
  if(! scope%in%c("capdistimes","capdistimesLU","capmod", "tseriesGHG", "lapm")){
    if(! grepl("capmod", scope)){
      if(ncol(capridat)>4){
        if(exists("ydim") & ! grepl("baseyear|capreg", scope)) capridat<-capridat[capridat$Y%in%as.character(ydim),]
        if(! is.null(curyear)){
          if(!grepl("^20",curyear)){curyear<-paste0("20",curyear)}
          capridat$y<-curyear
        }
      }
    }
  }
  
  if(scope%in%c("tseriesGHG")){
    capridat<-capridat[capridat$y%in%as.character(curyear)]
  }
  #capridat<-capridat[,setdiff(names(capridat),c("Y"))]
  if(!is.null(curdim5)){
    #print("select curdim5")
    if(curdim5[1]=="nonempty"){
      capridat<-capridat[capridat$empty!='',]
    }else{
      capridat<-capridat[capridat$empty%in%curdim5,]
    }
  }
  if(grepl("capmod", scope)){
    cat("\ncurscenshort", curscenshort)
    capridat$scen<-curscenshort
    if(grepl("SSP", curscen)){
      ssp <- substr(gsub(".*SSP", "SSP", curscen), 1, 4)
    }else{
      ssp <- ""
    }
    yrs <- unique(capridat$y)[1]
    capridat$ssp <- ssp
    capridat$scen <- paste0(curscenshort, ssp, "_", yrs)
    if(grepl("run", scope)){
      # If the keyword 'run' is given in the scope, then the files vary in a certain 
      # parameter which must be taken up. The syntax is 
      # 'run' followed by a minus '-' 
      #       followed by the keyword that must be found in the file name (curscen)
      #       followed by a comma ',' and the number of characters that should be taken up.
      #       Optionally, a delimitation from the right side can be included, if the number
      #       of characters is followed by a comma ',' and the character that ends focus.
      #       E.g scope <- "capmod_run-tarShft,4,_" removes the "_" and any character right of this
      #       from the 'run' flag
      runfocus <- gsub(".*run-", "", scope)
      runfocus <- strsplit(runfocus, ",")[[1]]
      checkfocus <- gsub(paste0(".*", runfocus[1]), "", basename(curscen))
      cat("\nfunfocus=", runfocus, "checkfocus=", checkfocus)
      if(checkfocus != basename(curscen)){
        #run <- substr(checkfocus, 1, nchar(runfocus[1])+as.numeric(runfocus[2]))
        run <- substr(checkfocus, 1, as.numeric(runfocus[2]))
      }else{
        run <- runfocus[1]
      }
      if(length(runfocus) == 3){
        run <- gsub(paste0(runfocus[3], ".*"), "", checkfocus)
      }
      capridat$run <- run
      capridat$scen <- paste0(curscenshort, ssp, "_", yrs, "_", run)
      cat("\n", paste0(curscenshort, ssp, "_", yrs, "_", run))
    }
  }
  save(capridat, file="test.rdata")
  #stop()
  return(list(capridat, fattr))
}

opendata<-function(scope,
                   curcountry,
                   curyear, 
                   baseyear='12',  # Required for scope capdistime and capmod
                   curscen='',      # Required for capmod
                   curscenshort=''
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
  datapath <- cenv$resin
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
    datanames<-c("rall", "cols", "rows", "y", "value")
    ydim<-"y"
  }
  if(grepl("capreg", scope)){
    datafile<-paste0("res_", baseyear)
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,"capreg/",datafile)
    dataparm<-"DATA2"
    datanames<-c("rall", "cols", "rows", "y", "value")
    ydim<-"y"
  }
  if(grepl("nbalance|tseries",scope)){
    datafile<-paste0(curcountry,"_12.gdx")
    datafile<-paste0(datapath,datafile)
  }
  if(grepl("tseriesGHG", scope)){
    datafile<-paste0("inventories/GHGperCountry/")
    datafile<-paste0(datafile, "res_time_series_GHG_", curcountry, curyear, ".gdx")
    datafile<-paste0(datapath, datafile)
    dataparm <- 'DATA2'
    datanames<-c("rall", "cols", "rows", "y", "value")
  }
  if(grepl("nlca",scope)){
    datafile<-paste0("capmod/res_2_0830",curscen,".gdx")
    datafile<-paste0(datapath,datafile)
  }
  if(grepl("capmod",scope)){
    
    # Check if subfolde
    
    if(pathincluded == 0) datafile<-paste0("capmod/res_2_", baseyear, curyear,curscen,".gdx")
    if(exists("datapath")){
      datafile <-paste0(datapath, datafile)
    }else{
      datafile <- paste0(cenv$resdir, "/")
    }
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
    if(scope=="capdis") datafile<-paste0("capdis/xobs_2_",curcountry,"_",baseyear, baseyear)
    if(scope=="capdiscapreg") datafile<-paste0("capdis/xobs_2_",curcountry,"_",baseyear, baseyear)
    #if(scope=="capdistimes") datapath<-paste0(d5space, "capdis_results/20181121_timeseries/")
    #if(scope=="capdistimes") datafile<-paste0("capdis/xobs_2_",curcountry,"_",baseyear,curyear)
    if(grepl("capdistimes", scope)) datafile<-paste0("capdis/xobstseries/xobs_2_",curcountry,"_",baseyear,curyear)
    if(grepl("capdistimes", scope)) if(grepl("NO", curcountry)) {
      datafile<-paste0("capdis/xobstseries/xobs_2_",curcountry,"000_",baseyear,curyear)}
    datafile<-paste0(cenv$resout,datafile,".gdx")
    dataparm<-"XOBS"
    ydim<-""
    datanames<-c("rall", "cols", "rows", "value")
    if(scope=="capdistimesLU"){
      dataparm <- "p_livestock"
      datanames<-c("rall", "cols", "type", "rows", "value")
    }
  }
  if(file.exists(datafile)){
    cat("\n ",datafile)
    #d<-list(datafile,dataparm,datanames,ydim)
    #Wrap a 'try' around in case there is a problem with the gdx file
    capridat <- NULL
    #if(igdx()==TRUE){
    #  try(capridat<-as.data.table(rgdx.param(datafile,dataparm)), silent = TRUE)
    #}else{
      system(paste0("gdxdump.exe ", datafile, " output=capriextract.csv format=CSV symb=", dataparm))
      capridat <- fread("capriextract.csv")
    #}
    if(! is.null(capridat)){
      names(capridat)<-datanames
      if(scope=="capdistimes") {
        capridat$y <- curyear
        capridat<-capridat[,c("rall", "cols", "rows", "y", "value")]
      }
      if(scope=="capdistimesLU"){
        capridat$y <- curyear
        capridat<-capridat[,c("rall", "cols", "type", "rows", "y", "value")]
      }
      if(grepl("lapm", scope)){
        capridat$Y<-gsub("_","",curyear)
        if(curyear=="")capridat$Y<-"2010"
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


selectOnlyCountries <- function(x){
  caprall <-   unique(x$rall)
  caprall <- caprall[! grepl("0000", caprall)]
  caprall <- caprall[! grepl("NO0", caprall)]
  caprall <- caprall[! caprall %in% c("EU027000", "EU27yr19",   "EU025000",   "EU013000",   "EU012000")]
  caprall <- union(s$nuts0, caprall)
  
  x <- x[rall %in% caprall]
  
  
}

selectcolelements <- function(reffilename = NULL,#if NULL use default file 
                              reload = FALSE,    #if TRUE, reload even if file exists
                              keepcols = NULL    #if NULL use default selection
                              ){
  # Obtain a capmod-result file that can be used to check dimensions
  
  if(is.null(reffilename)) {
    reffilename <- "res_2_0830mtr_rd_ref_endManAvail"
  }
  reffil <- paste0(cenv$capri, cenv$leipadr, cenv$resdir, "/capmod/", reffilename, ".rdata")
  if(reload | (! file.exists(reffil))){
    #            ... in case capmode is already included in resin
    filgdx <- paste0(gsub("\\/capmod\\/", "", cenv$resin), "/capmod/", reffilename, ".gdx")
    filgdx <- gsub("rdata", "gdx", reffil)
    dim5 <- c("rall", "empty", "cols", "rows", "y", "value")
    reffile <- as.data.table(rgdx.param(filgdx, "dataOut", names=dim5))
    save(reffile, file=reffil)
  }else{
    load(file=reffil)
  }
  capcols <- unique(reffile$cols)
  
  if(is.null(keepcols)){
    keepcols <- c(s$mbal, "INDM", "HCON", "PROC", "IMPORTS", "EXPORTS", "INHA", "INCE", "INDMsh", "DEMD", 
                  # MQPOS_SET
                  "FOOD", "SHARE", "FATSVAL", "PROTVAL", "Corr", "PPRI", "CPRI",
                  as.character(capcols[grepl("LOS", capcols)]), 
                  as.character(capcols[grepl("N_", capcols)]),
                  #as.character(capcols[grepl("VIT_", capcols)]),
                  s$nbalsoil, s$nbil, s$mpact, 
                  as.character(capcols[grepl("Impact", capcols)]), 
                  "mitiEff", "mitiNetC",
                  "GWPA", "GWPT", "GLUC", "GWPR", 
                  "PROD", "PROD_LO", "PROD_UP", 
                  "YILD", "YILD_LO", "YILD_UP",
                  s$set_act_agg, "UAAR",
                  "WELFARE", "FEOG", "EAAG"

    )
  }
  
  caprall <-   unique(reffile$rall)
  caprall <- caprall[! grepl("0000", caprall)]
  caprall <- caprall[! grepl("NO0", caprall)]
  caprall <- caprall[! caprall %in% c("EU027000", "EU27yr19",   "EU025000",   "EU013000",   "EU012000")]
  caprall <- union(s$nuts0, caprall)

  
  return(list(keepcols, caprall))
}

