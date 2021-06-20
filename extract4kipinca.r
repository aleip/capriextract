#' Extraction of disaggregated CAPRI timeseries data
#' 
#' Preparation of a high resolution dataset of the Nitrogen Budget and Nitrogen Balance, 
#' as well as of GHG emissions for the KIP-INCA and VERIFY projects.
#'
#' @param capri.runfile String. Link to a CAPRI runfile that contains the CAPRI setting describing run parameters
#'  such as input and output folders, parameter settings etc. The information will be saved for each run.
#' @param savedate String.  
#' @param curcountries String vector. Contains the regions that have been disaggregated and/or that 
#'   are to be extracted. 
#' @param curyears String vector. Contains years of the time series to be extracted.
#' @param baseyear String. Current CAPRI base year - last two digits.
#' @param reginame String. Name of the result files, usually an acronym of the region set but can be any
#'   other name indentifying the current disaggregation.
#'   
#' @return Data table. Main purpose is to write out result data in *rdata and *csv formats and perform checks. 
#'   Upon completion the data table containing N-budet data will be returned if successful.
#'   
#' @author Adrian Leip \email{adrian.leip@ec.europa.eu}
#' @references Leip, A., Koeble, R., 2018 The CAPRI disaggregation. Report in preparation
#' @referemces Leip, A., Koeble, R., Reuter, H.I., Lamboni, M., Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284, Unpublished data archive
#' @references Lamboni, M., Koeble, R., Leip, A., 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82, 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028
#' @references Leip, A., Marchi, G., Koeble, R., Kempen, M., Britz, W., Li, C., 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5, 73-94. https://doi.org/10.5194/bg-5-73-2008
#' 
#' @export
#' 
#' @examples

dothisfuntionlinebyline <- function(){
  
  # Exported 'logged procedure' as function in extract4kipinca.r
  # Needs better documentation
  rm(list=objects())
  setwd("D:/dev/capriextract/")
  source("R/initializecapri.R")
  
  # 1. Run 'initializecapri.R' (file ./R/initializecapri.R)
  #    - indicating the cur_run.gms file. This is important as it contains all the settings.
  #      therefore prepare by chosing the cur_run.gms file & rename it properly.
  #      NEW: Add comments. Aslo startextractcapdis() not called automatically
  
  # Provide a cur_run.gms of the runs carried out (example) that contains all relevant information
  #   on paths, settings etc.
  # Make sure to update CAPRI revisions - description in startextractcapdis() in file capdistseries_functions.r
  currun <- "cur_ru_1~20210423.gms"
  folderdate <- '20210423'
  extractdate <- "20210423"
  project <- 'kipinca'
  reginame <- "EU"
  InitCapriEnv(capri.runfile = currun, addcomment = "KIP-INCA run")
    # 2. Run 'startextractcapdis' (file: capdistseries_function.r)
  source("capdistseries_function.r")
  source("extract4kipinca.r")
  startextractcapdis(folderdate = folderdate)

  # Determine regions and years
  regyrs <- regionsyears()
  curreg <- regyrs[[1]]
  curyears <- regyrs[[2]]
  
  # For testing: redo full extraction --> set startnew to FALSE
  startnew<-T; if(startnew) file.remove(list.files(path = savepath, all.files = TRUE, recursive = TRUE, pattern = "*", full.names = TRUE))
  
  #curreg <- curreg[1:4]; curyears <- curyears[1]
  # Recombine years & filter for most important variables
  avoidwrite <- TRUE
  donbudgetandcombine <- function(avoidwrite=TRUE){
    nbudgetfiles <- sapply(curyears, function(y) 
      extract4kipinca(capri.runfile = currun, savedate = NULL, extractdate = extractdate,
                      curcountries = curreg, reginame = reginame, project=project, 
                      curyears = y, baseyear = '12', avoidwrite = avoidwrite))
    
    
    nbudget <- peryearandrecombine(nbf = nbudgetfiles, avoidwrite = TRUE)
    return(nbudget)
  }
  nbudget <- donbudgetandcombine()
  nbudgetland <- nbudget[[1]]
  nbudgetfiles <- nbudget[[2]]
  
  source("verify_functions.r")
  # Calculate absolute emissions 
  nbudgetfiles <- sapply(curyears, function(x) 
    getnbudgetfiles(capri.runfile = currun, savedate = NULL, extractdate = extractdate,
                    curcountries = curreg, reginame = reginame , project=project, 
                    curyears = x, baseyear = '12'))
  
  
  verifyexp <- Reduce(rbind, lapply(1:length(nbudgetfiles), function(x) extractGHGs(data2load = nbudgetfiles[x], yr = curyears[x])))
  verifydata <- dcast.data.table(verifyexp, rall + y ~ gas, value.var = "value")
  save(verifydata, file=gsub(paste0(curyears[1], ".*"), project, "data_time.rdata", nbudgetfiles[1]))
  data2map <- verifydata[, .(rall, y, N2O)]
  ncexp1 <- export2netCDF(data2map = verifydata[, .(rall, y, N2O)])
  ncexp2 <- export2netCDF(data2map = verifydata[, .(rall, y, CH4)])
  
  
  ldirs <- setdiff(list.dirs(savepath, full.names = FALSE), "")
  jrcboxpath <- paste0("d:/share/leipadr/jrcbox/afoludata/kipinca/", folderdate, "_", project)
  setwd(savepath)
  lapply(ldirs, function (x) zip(paste0(jrcboxpath, "/", x), files = x))
  
}

regionsyears <- function(){
  
  xobsfiles <- list.files(path = paste0(cenv$resout, "capdis/"), pattern = "xobs.*gdx", full.names = FALSE)
  xobsfiles <- substr(xobsfiles, 8, 11)
  xobstseries <-  list.files(path = paste0(cenv$resout, "capdis/xobstseries"), pattern = "xobs.*gdx", full.names = FALSE)
  yrs <- unique(substr(xobstseries, 15, 18))
  yrs <- yrs[!grepl("^0", yrs)]
  xobstseries1 <-unique(substr(xobstseries, 8, 11))
  xobstseries1 <- xobstseries1[! grepl("NO", xobstseries1)]
  xobstseries2 <-unique(substr(xobstseries, 8, 12))
  xobstseries2 <- xobstseries2[ grepl("NO", xobstseries2)]
  xobstseries <- c(xobstseries1, xobstseries2)
  nyrs <- length(yrs)
  n <- sapply(1:length(xobstseries), function(x)
    length(list.files(path=paste0(cenv$resout, "capdis/xobstseries"), 
                      pattern = paste0("xobs.*", xobstseries[x], ".*gdx"), 
                      full.names = FALSE)))
  n
  non <- which(n!=nyrs)
  non <- xobstseries[which(n!=nyrs)]
  # --> Two resgions (ES21 and HR00 have not completed disaggregation --> restart)
  #curreg <- xobstseries[grepl("AT", xobstseries)]
  curreg <- xobstseries[which(n==nyrs)]
  curyears <- as.numeric(yrs)
  
  return(list(curreg, curyears))
  
}

peryearandrecombine <- function(
  nbf = nbudgetfiles,
  # currun = currun, 
  # savedate = savedate, 
  # extractdate = extractdate,
  # curreg = curreg, 
   rgn = reginame, 
  # project=project, 
  cyr = curyears, 
  #baseyear = '12',
  avoidwrite = FALSE
  ){
  
  cat("Function peryearandrecombine, started at ", format(Sys.time(), "%Y%m%d-%H:%M"))
  nbudgetfiles <- nbf
  reginame <- rgn
  curyears <- cyr
  load(nbudgetfiles[1])
  fsumap <- unique(nbudget[, .(rall, CAPRINUTS2, CNTR_CODE)])
  nbudgetall <- Reduce(rbind, lapply(1:length(nbudgetfiles), function(x) {
    load(nbudgetfiles[x])
    gasdetails <- names(nbudget)[grepl("^CH4|^N2|^NH3|^NOX|^RUN", names(nbudget))]
    #fsumap <- unique(nbudget[, .(rall, CAPRINUTS2, CNTR_CODE)])
    nbudget <- nbudget[, -c("CAPRINUTS2", "CNTR_CODE"), with=FALSE]
    nbudget[, NH3NOX := rowSums(.SD), .SDcols=names(nbudget)[grepl("^NH3|^NOX", names(nbudget))]]
    nbudget[, N2O := rowSums(.SD), .SDcols=names(nbudget)[grepl("^N2O", names(nbudget))]]
    nbudget[, CH4 := rowSums(.SD), .SDcols=names(nbudget)[grepl("^CH4", names(nbudget))]]
    gasbudget <- nbudget[, gasdetails, with=FALSE] 
    nbudget <- nbudget[, -gasdetails, with=FALSE]
    save(gasbudget, file = gsub("dcastclean", "gasdetails", nbudgetfiles[x]))
    return(nbudget)
  }))
  yearrange <- paste0(min(as.numeric(curyears)), "-", max(as.numeric(curyears)))
  nbudgetland <- nbudgetall[cols %in% c(s$mcact, "UAAR", "OLND")]
  nbudgetanim <- nbudgetall[! cols %in% c(s$mcact, "UAAR", "OLND"), .(rall, cols, y, LEVL, LU)]
  save(nbudgetland, fsumap, file=paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot_land.rdata"))
  save(nbudgetanim, fsumap, file=paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot_anim.rdata"))
  
  gasdetails <- Reduce(rbind, lapply(1:length(nbudgetfiles), function(x) {
    load(gsub("dcastclean", "gasdetails", nbudgetfiles[x]))
    return(gasbudget)
  }))
  save(gasdetails, fsumap, file=paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHG_details.rdata"))
  
  
  calcstats2excel(paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot_land.rdata"), objectname = "nbudgetland")
  
  file6 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_histogramsdone", ".txt")
  if(! file.exists(file6)){
    dohistos(forhist = nbudgetland, filename = file6)
    writeLines("Histograms done", con = file6)
  }  
  
  # Delete annual files 
  deleted <- sapply(1:length(nbudgetfiles), function(x) {
    file.remove(nbudgetfiles[x])
    file.remove(gsub("dcastclean", "gasdetails", nbudgetfiles[x]))
  })
  
  cat("\nFunction peryearandrecombine, ended at ", format(Sys.time(), "%Y%m%d-%H:%M"))
  return(list(nbudgetland, nbudgetfiles))
  
}

extract4kipinca <- function(
  
  capri.runfile = NULL, 
  
  savedate = NULL, 
  folderdat = folderdate,
  extractdate = NULL,
  
  curcountries = NULL, 
  curyears = '2012',
  baseyear = '12',
  
  reginame = NULL, project=project,
  avoidwrite = FALSE
  
  ){
  
  
  if(is.null(savedate)){savedate <- paste0("_",format(Sys.time(), "%Y%m%d"))}
  yearrange <- paste0(min(curyears), "-", max(curyears))
  
  
  # 2. Run 'startextractcapdis' (file: capdistseries_function.r)
  source("capdistseries_function.r")
  startextractcapdis(folderdate = folderdate)
  
  
  # check if the script has already been done until a certain step:
  curdate <- paste0("_",format(Sys.time(), "%Y%m%d"))
  if(! is.null(extractdate)){curdate <- paste0("_", extractdate)}
  file1 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGN", curdate, ".rdata")
  file2 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot", curdate, ".rdata")
  file3 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot", curdate, "_dcastclean", ".rdata")
  file4 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot", curdate, ".xlsx")
  file5 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_files4kipinca", curdate, ".txt")
  file6 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_histogramsdone", curdate, ".txt")
  #return(file3)
  
  # 3. Start extraction
  #    Note that below steps are checked one-by-one.
  # Sequence (<-- means: is RH is called by L:H):
  # extractall  <-- loadcurfile (both in: capdistseries_function.r)
  #             <-- filtermultiple (file: openfiltermultiple.r)
  #             <-- filteropen (file: openfiltermultiple.r)
  #             <-- opendata (file: openfiltermultiple.r)
  if(! file.exists(file2) & ! file.exists(file3)) {
    cat("\n Generating ", file2)
    xobshsu<-extractall(reginame=reginame, scope="capdis", cols=mcactuaar, ydim=NULL, #curdim5=NULL, 
                        curcountries=curcountries, regi=NULL, curyears=curyears, baseyear='12', 
                        curscens='', curscensshort='', extractdate = curdate)
    file.remove(file1)
  }else{cat("\n", file2, " already created.")}
  
  # 4. Checkregions makes only sense of all EU regions are calculated
  if(reginame=="EU27") {checkregions(folderdate=folderdate, savedate=savedate)}
  
  # 5. Combine data for KIP-INCA : extract relevant data that are submitted to the EEA
  #    Note 20210108 off-farm grazing data are not yet included
  # done <- combinedata4kipinca(date2load=savedate, reginame = reginame, yearrange = yearrange)
  
  # 6. Perform checks on distributions and outliers and clean very small (delete) and high MANAPP (put cap) values
  #    Some additional tests are carried out below with the nbudget data loaded. If still necessary 
  #    needs to be added to the cleandate4kipinca function
  if(! file.exists(file3) | ! file.exists(file4)){
    
    # file3 is the data file that is needed, but file 4 are checks that need to be done as well
    cleaned <- cleandate4kipinca(filename = file2)
    file.remove(file2)
    
  }else{cat("\n", file3, " already created.")}
  
  # 7. Write out in the format agreed with EEA
  #     Note here the data are re-loaded (should be the same as in the data.table 'cleaned' returned above) 
  #     Note 20200108 the function requires 'HeadperHa' which was loaded from p_livestock
  #      This is currently not extracted and needs to be added
  nbudgetfile <- file3
  cat("\n", nbudgetfile)
  load(     nbudgetfile)
  if(! avoidwrite){
    if(! file.exists(file5)){
      concsvs <- file(description = file5, open = "wa")
      wrapoverwrite(x = nbudget[, -c("CAPRINUTS2", "CNTR_CODE"), with=FALSE], reginame = reginame, yearrange=yearrange, concsvs = concsvs)
      writelivestock(paste0(savepath, "/xobs_", reginame, "_", yearrange, "_LIVESTOCK", curdate, ".rdata"), reginame = reginame, concsvs = concsvs)
      close.connection(concsvs)
    }
  }
  
  save(list=objects(), file="extract114.rdata")
  if(! file.exists(file6)){
    dohistos(nbudget, file6)
    writeLines("Histograms done", con = file6)
  }  
    
  return(file3)
}
getnbudgetfiles <- function(
  
  capri.runfile = NULL, 
  
  savedate = NULL, 
  folderdat = folderdate,
  extractdate = NULL,
  
  curcountries = NULL, 
  curyears = '2012',
  baseyear = '12',
  
  reginame = NULL, project=project
  
){
  if(is.null(savedate)){savedate <- paste0("_",format(Sys.time(), "%Y%m%d"))}
  yearrange <- paste0(min(curyears), "-", max(curyears))
  
  
  # 2. Run 'startextractcapdis' (file: capdistseries_function.r)
  source("capdistseries_function.r")
  startextractcapdis(folderdate = folderdate)
  
  
  # check if the script has already been done until a certain step:
  curdate <- paste0("_",format(Sys.time(), "%Y%m%d"))
  if(! is.null(extractdate)){curdate <- paste0("_", extractdate)}
  
  file3 <- paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot", curdate, "_dcastclean", ".rdata")
  return(file3)
}
