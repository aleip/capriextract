#' Load $setglobals from CAPRI steering files
#' @description The function \code{loadglobalsfrombatch()} reads relevant CAPRI settings and saves them in one file.
#'              For runs in the batch mode it reads them fro the batchout folder.
#'              Alternatively can read multiple CAPRI steering files from the 'log' result folder
#' @param savepath Indicates where the otuput of the function should be saved
#' @param dp Datapath - uses a global environment variable if given; is used only of bathpath or savepath are NULL
#' @param bathpath Used to identify files to read. This concept still needs to be further developed
#' @param batchdir idem
#' @param logf idem

loadglobalsfrombatch <- function(savepath = NULL,
                                 dp = datapath,
                                 bathpath = NULL,
                                 batchdir = NULL,
                                 logf = NULL){
  
  # logf -> indicate logf if the settings are to be retrieve from a file that is written to %results_out%/log
  #         in the special case that this is copied to google drive (AL) use flag 'google' for the dp
  
  
  xlsok <- require(xlsx)
  if(is.null(logf)){
    
    # Retrieve fortran.gms from the batch output
    # Missing information: name of file
    
    # Default batchdir location
    if(is.null(bathpath)){
      batchf <- paste0(dp, "../batchout")
    }else{
      batchf <- bathpath
    }
    cat("\n batchf=", batchf, "\n", dp, "\n", datapath)
    if(is.null(batchdir)) {
      batchdirs <- file.info(list.files(batchf, full.names=TRUE))
      batchdirs <- batchdirs[with(batchdirs, order(as.POSIXct(ctime),decreasing=TRUE)), ]
      batchdir <- rownames(batchdirs)[1]
    }else{
      batchdir <- paste0(batchf, "/", batchdir)
    }
    
    cat(batchdir)
    # Check fortra_X files
    fortran <- list.files(batchdir, pattern = "fortra_[0-9]*.gms")
  }else{
    
    # Retrieve fortran.gms from log-folder
    batchdir <- paste0(dp, "/log/")
    
    # Specific setting (AL) - logfiles copied to google drive
    if(dp=="google") batchdir <- paste0(google, "/projects/capri_runs/log/")
    
    cat(batchdir)
    fortran <- list.files(batchdir, pattern = paste0(logf, ".*"))
  }
  
  if(is.null(savepath)) {
    savepath <- paste0(dp, "/log/")
    if(dp=="google") savepath <- paste0(google, "/projects/capri_runs/log/")
  }
  
  setglob <- lapply(1:length(fortran), function(x) {
  #setglob <- lapply(1:3, function(x) {
    
    curf <- paste0(batchdir, "/", fortran[x])
    cat("\n", curf)
    con <- file(curf, open = "r")
    setglobal <- readLines(con)
    setglobal <- setglobal[grepl("setglobal|time and date", setglobal, ignore.case=TRUE)]
    setglobal <- setglobal[! grepl("Rexe|Trollexe|gamsArg|procSpeed|JAVA|CMD|GAMSexe|gamsPath", setglobal)]
    setglobal <- setglobal[! grepl("regcge_scenario|countries|result_type_underScores|lst2|fst[24]", setglobal)]
    setglobal <- setglobal[! grepl("regLevel|initialLUfile_|tradeMatrixInputFileName_", setglobal)]
    setglobal <- setglobal[! grepl("policy_blocks|modArmington|explicit_NTM|tagg_module|yani_m|REGCGE", setglobal)]
    setglobal <- setglobal[! grepl("altLicense|NET_MIGR|FIX_BUDGET_FAC_SUBS|Supply|abMob|closure_|solpringSupply|limrow|limcol", setglobal)]
    setglobal <- c(setglobal, paste0("$SETGLOBAL batchfolder ", batchdir))
    setglobal <- gsub("\\*   Time and date   :", "$SETGLOBAL Time", setglobal)
    
    #only NOW
    setglobal <- setglobal[! grepl("curCCscen", setglobal)]
    
    close(con)
    
    
    
    setx <- as.data.table(Reduce(rbind, lapply(1:length(setglobal), function(y) {
      a <- strsplit(setglobal[y], " ")[[1]]
      a <- c(a[2], paste(a[3:length(a)], collapse=" "))
      return(a)
      
    })))
  }
  )
  setglobals <- setglob[[1]]
  curn <- names(setglobals)
  setglobals$n <- 1:nrow(setglobals)
  setglobals <- setglobals[,c("n", curn), with=FALSE]
  #for (i in 2:4){
  for (i in 2:length(fortran)){
    cat("\n - ", i)
    setglobals <- merge(setglobals, setglob[[i]], by = "V1")
    setnames(setglobals, colnames(setglobals), c("V1", "n", paste0("r", seq(1:i))))
    setglobals[get(paste0("r", i))==r1, paste0("r", i)] <- ""
    
  }
  setglobals<-setglobals[order(setglobals$n)]
  tt <- gsub(" ","_",gsub("-|:","",setglobals[V1=="Time",r1]))
  rt <- setglobals[V1=="GamsStartNo", 3:ncol(setglobals)]
  ort <- paste0("r", sort(as.numeric(rt)))
  #print(ort)
  newnames <- c("setglobal", "n", paste0("r", rt))
  setnames(setglobals, colnames(setglobals), newnames)
  setglobals <- setglobals[, c("setglobal", "n", ort), with=FALSE]
  View(setglobals)
  cat("\nWrite file ", paste0(savepath, "/", tt, "globals.csv"))
  write.csv(setglobals, file=paste0(savepath, "/", tt, "globals.csv"))
  if(xlsok) write.xlsx(setglobals, file = paste0(savepath, "/", tt, "global_test.xlsx"), sheetName = "setglobals", col.names=TRUE, row.names=FALSE)
  return(setglobals)
}



loadcapmod <- function(subf = "", tpath="temp", batchdir=NULL){
  startextract('capmod')
  glb <-loadglobalsfrombatch(batchdir=batchdir, savepath=paste0(datapath, "capmod/", subf))
  fls <- getfilesfromfolder(curfol = paste0(datapath, "capmod/", subf), 
                            flag = 'tariffs', 
                            reference=NULL
  )
  
  checkstepreports(tpath=tpath)
  capri<-filtermultiple(scope = scope,cols=NULL,rows=NULL,ydim=NULL,curdim5='',
                        regi=NULL,curcountries = "",curyears = '30',baseyear = '08', 
                        curscens = fls,
                        curscensshort = scenshort
  )
  
  return(capri)
  
  
}






extractlca <- function(fls = NULL, curcountries = "", curyears = '30', baseyear = '08', curscenshort = NULL){
  
  curcols <- c('PROD', 'YILD')
  #curcols <- unlist(lapply(1:2, function(x) paste(curcols[x], c('_LO', '_UP'))))
  capri<-filtermultiple(scope = 'capmod',
                        cols=curcols,
                        rows=NULL,
                        ydim=NULL,
                        curdim5='nonempty',
                        regi=NULL,
                        curcountries = curcountries,
                        curyears = curyears,
                        baseyear = baseyear, 
                        curscens = fls,
                        curscensshort = scenshort
  )
  
  deleteempties <- c("own", "cross", 
                     "total",      # has negative values?
                     "suppModel",  # idem
                     "mrkInfes",
                     "FATS", "PROT",
                     "FEDE",
                     "CERE", "OILS", "OAFC", "VGPM", "TROP", "OCRP", "MEAS", "OANP", "ACQU", "YANI", "MILC", "OILP", "CAKS", "SECO",
                     "LAND"
  )
  
  caprid <- caprid[! EMPTY %in% deleteempties]
}
