#' Script to monitor the outcome of a CAPRI (batch) run
#' @description The function \code{loadglobalsfrombatch()} is a function that 
#' return the settings steering the individual capmod runs reading the 'fortran.gms'
#' files and evaluating the $setglobal's. 
#' @param savepath = NULL. Folder into which result files are written: 
#' (i) file-name.csv: content of data table (see return)
#' (ii) file-name.xlsx: content of data table if library xlsx is available
#' (iii) file-name-batchout.html copy of the batchout.html file. 
#' Note that this file is complete only if the function is run once all simulations
#' are done.
#' @param batchf=paste0(cenv$resdir, "/../batchout"). Folder from which the batch-run
#' input and listing files are to be take. By default this is linked to the list 
#' 'cenv' (CAPRI environment settings) which needs to be available in the global
#' environment when calling the function. By default the batch files are assumed
#' to be located at the same level as the result folder in a folder called 'batchout'.
#' This is used if is.null(logf). Otherwise, the files with CAPRI settings are 
#' retrieved from the 'logf' folder.
#' @param dp = cenv$resdir. Parameter used to define where the results are written.
#' @param batchdir = NULL. 
#' @param logf=NULL. if is.null(logf) then the batch directories are retrieved from
#' a batchout folder (subfolder batchdir), which is screened to subfolders with numeric 
#' folder names (sequence of simulations in a batch run). 
#' Otherwise indicated the folder all "fortra_[0-9]*.gms" files are retrieved. 
#' (Note they are by default placed on the 'log' folder in the capri result directory; 
#' however as here are many, moving them into a separate folder per evaluation is recommended.)
#' @return data table with major settings for the simulations in a batch run.
#' Selection of 'major' settings are hard-coded in the function.
#' Further, two (optionally three) files are written into a result folder (see savepath)
#' @examples \dontrun{
#'   # Clean-up environment and initialize the CAPRI settings (list s) and 
#'   # environment (list cenv); set scope to 'capmod'
#'   rm(list=objects()); 
#'   source("R/initializecapri.R"); 
#'   InitCapriEnv(scope = 'capmod')
#'   # Define the savepath and run function
#'   spath <- paste0(cenv$resout, "/20190319_ssp35trade")
#'   globals <- loadglobalsfrombatch(batchdir = "Mar_19__2019_9.20.45_AM", savepath=spath)


#' caprirunfile <- "x:\\dev\\epnf\\gams\\fortran.gms"
#' InitCapriEnv(caprirunfile)
#' str(c)
#' }
#' @export
loadglobalsfrombatch <- function(savepath = NULL,
                                 tpath = cenv$scrdir, 
                                 batchf = paste0(cenv$resdir, "/../batchout"),
                                 dp = cenv$resdir,
                                 batchdir = NULL,
                                 logf = NULL){
  
  # logf -> indicate logf if the settings are to be retrieve from a file that is written to %results_out%/log
  #         in the special case that this is copied to google drive (AL) use flag 'google' for the dp
  
  
  xlsok <- require(xlsx)
  if(is.null(logf)){
    
    # Retrieve fortran.gms from the batch output
    # Missing information: name of file
    
    cat("\n batchf=", batchf, "\n", dp, "\n", cenv$resdir)
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
#    fortran <- list.files(batchdir, pattern = paste0(logf, ".*"))
  }
  
  if(is.null(savepath)) {
    savepath <- paste0(dp, "/log/")
    if(dp=="google") savepath <- paste0(google, "/projects/capri_runs/log/")
  } 
  
  cat("\nfortran=", tpath)
  fortran <- gettmpsubfolders(tpath=tpath)
  cat("\nfortran=", fortran)
  fortran <- fortran[file.exists(paste0(tpath, "/", fortran, "/fortran.gms"))]
  
  setglob <- lapply(1:length(fortran), function(x) {
    
    curf <- paste0(tpath, "/", fortran[x], "/fortran.gms")
    cat("\n", curf)
#    if(file.exists(curf)){
      con <- file(curf, open = "r")
      setglobal <- readLines(con)
      save(setglobal, file="setglobal.rdata")
      setglobal <- setglobal[grepl("setglobal|time and date", setglobal, ignore.case=TRUE)]
      setglobal <- setglobal[! grepl("Rexe|Trollexe|gamsArg|procSpeed|JAVA|CMD|GAMSexe|gamsPath", setglobal)]
      setglobal <- setglobal[! grepl("regcge_scenario|countries|result_type_underScores|lst2|fst[24]", setglobal)]
      setglobal <- setglobal[! grepl("regLevel|initialLUfile_|tradeMatrixInputFileName_", setglobal)]
      setglobal <- setglobal[! grepl("policy_blocks|modArmington|explicit_NTM|tagg_module|yani_m|REGCGE", setglobal)]
      setglobal <- setglobal[! grepl("altLicense|NET_MIGR|FIX_BUDGET_FAC_SUBS|Supply|abMob|closure_|solpringSupply|limrow|limcol", setglobal)]
      setglobal <- setglobal[! grepl("irR", setglobal)]
      setglobal <- c(setglobal, paste0("$SETGLOBAL batchfolder ", basename(batchdir)))
      setglobal <- gsub("\\*   Time and date   :", "$SETGLOBAL Time", setglobal)
      #only NOW
      setglobal <- setglobal[! grepl("curCCscen", setglobal)]
      
      scrdirt <- strsplit(setglobal[grepl("scrdir", setglobal)], "  *")[[1]][3]
      scrpath <- dirname(scrdirt)
      scrdir <- gsub("'", "", basename(scrdirt))
      setglobal <- setglobal[!grepl("scrdir", setglobal)]
      setglobal <- c(setglobal, paste0("$SETGLOBAL scrpath ", scrpath))
      setglobal <- c(paste0("$SETGLOBAL scrdir ", scrdir), setglobal)
      
      close(con)
      
      
      setx <- as.data.table(Reduce(rbind, lapply(1:length(setglobal), function(y) {
        a <- strsplit(setglobal[y], " ")[[1]]
        a <- c(a[2], paste(a[3:length(a)], collapse=" "))
        return(a)
      })))
#    }
  })
  reportsummary <- unlist(lapply(1:length(fortran), function(x){
    #reportsummary <- Reduce(rbind,unlist(lapply(19:21, function(x){
    #reportsummary <- lapply(22:22, function(x){
    
    #curf <- paste0(batchdir, "/", x, ".lst")
    #con <- file(curf)
    #open(con)
    curf <- paste0(batchdir, "/", x, ".lst")
    if(file.exists(curf)){
      a <- try(readLines(curf, warn=FALSE), silent=TRUE)
      #GamsStartNo <- strsplit(a[grepl("GamsStartNo", a)][1], "  *")[[1]][5]
      #close(con)
      a <- a[(length(a)-40):length(a)]
      a <- a[a!=""]
      report <- which(grepl("REPORT FILE SUMMARY", a))
      error <- grepl("USER ERROR.*ENCOUNTERED", a)
      success <- grepl("SUCCESS", a)
      if(sum(error)==0 & sum(success)>0) {
        error <- "Success"
      }else if(sum(error)==0 & sum(success)==0){
        error <- "Running"
      }else{
        error <- which(grepl("ERROR|Error", a))
        error <- a[error[1]:(report-1)]
        error <- paste(error, collapse="\n")
      }
    }else{
      error <- "Not started"
    }
    
  }))
  setglobals <- setglob[[1]]
  curn <- names(setglobals)
  setglobals$n <- 1:nrow(setglobals)
  setglobals <- setglobals[,c("n", curn), with=FALSE]
  #for (i in 2:4){
  
  # cleaning
  #cat("\n", colnames(setglobals))
  #scrbase <- dirname(dirname(setglobals[V1=="scrdir", V2]))
  save(list=objects(), file="loadglobalsfrombatch173.rdata")
  moreruns <- 2:length(fortran)
  if(length(fortran)>1){
    for (i in moreruns){
      #cat("\n - ", i)
      setglobals <- merge(setglobals, setglob[[i]], by = "V1")
      setnames(setglobals, colnames(setglobals), c("V1", "n", paste0("r", seq(1:i))))
      
      # In case it is easier to have blanks where the same value is as in the first column
      # set keepsameasfirstempty to 1
      keepsameasfirstempty <- 1
      if(keepsameasfirstempty==1){
        setglobals[get(paste0("r", i))==r1, paste0("r", i)] <- ""
        #setglobals[V1=="scrdir", paste0("r", i)] <- ""
      }
      
    }
  }else{
    setglobals <- setglobals[, .(V1, n, V2)]
    setnames(setglobals, colnames(setglobals), c("V1", "n", "r1"))
  }
  #setglobals[V1=="scrdir", .SD, .SDcols=paste0("r", moreruns)] <- 
  #  basename(as.character(setglobals[V1=="scrdir", .SD, .SDcols=paste0("r", moreruns)]))
  setglobals<-setglobals[order(setglobals$n)]
  
  save(list=objects(), file="glob180.rdata")
  # Check which entries have been 'deleted' as they are the same as for the first run
  # Copy those that vary with the runs to the top of the table
  nentries <- as.data.table(setglobals[, .SD=="", .SDcols=paste0("r", 1:length(fortran))])
  nentries <- apply(nentries, 1, sum)
  setglobals <- rbind(setglobals[nentries!=(length(fortran)-1)], setglobals[nentries==(length(fortran)-1)])
  
  # Subfolders accessed in the order 1, 10, 11, ..., 2, 21, 22, ... 3, ....
  # Needs to be re-ordered according ot the start number 1, 2, 3, 4, ....
  tt <- gsub(" ","_",gsub("-|:","",setglobals[V1=="Time",r1]))
  rt <- setglobals[V1=="scrdir", 3:ncol(setglobals)]
  ort <- paste0("r", sort(as.numeric(rt)))
  newnames <- c("setglobal", "n", paste0("r", rt))
  setnames(setglobals, colnames(setglobals), newnames)
  setglobals <- setglobals[, c("setglobal", "n", ort), with=FALSE]
  
  # Add information on simulation status (success - running - not started - Error message)
  save(list=objects(), file="glob.rdata")
  reportsummary<-as.data.table(t(c("status", 99, reportsummary)))
  names(reportsummary)<-names(setglobals)
  setglobals<-rbind(setglobals, reportsummary)
  setglobals$n <- as.numeric(setglobals$n)
  
  #View(setglobals)
  cat("\nWrite file ", paste0(savepath, "/", tt, "globals.csv"))
  if(! dir.exists(savepath)) dir.create(savepath)
  write.csv(setglobals, file=paste0(savepath, "/", tt, "globals.csv"))
  if(xlsok) {
    testwrite <- try(write.xlsx(setglobals, file = paste0(savepath, "/", tt, "global.xlsx"), sheetName = "setglobals", col.names=TRUE, row.names=FALSE), silent=TRUE)
    if(class(testwrite)=="try-error"){
      cat("\n\n", strsplit(testwrite[[1]], "\\n")[[1]][2])
      stop("Please correct path or close file!")
    }
  }
  cat("\n", paste0(batchdir, "/batch.html"))
  cat("\n", paste0(savepath, "/", tt, "batchout.html"))
  file.copy(paste0(cenv$capri, batchdir, "/batch.html"), paste0(savepath, "/", tt, "batchout.html"))
  return(setglobals)
}


checkstepreports <- function(runasbatch=1, nruns=NULL, tpath=cenv$scrdir, 
                             outpath=cenv$resout, commonname = NULL){
  
  #con <- list.files(path=spath, pattern=".*global.xlsx",recursive=FALSE, full.names = TRUE)
  if(! is.null(commonname)) {
    con <- paste0(outpath, "/", commonname, ".xlsx")
  } else {
    con <- paste0(outpath, "/stepreport.xlsx")
  }
  require(gdxrrw)
  if(gdxrrw::igdx() == FALSE){
    gdxrrw::igdx(cenv$gamsdir)
  }
  
  if(is.null(tpath)){
    message("Please indicate a folder used by CAPRI as 'scrdir'. ",
            "\nTried to get it from the CAPRI environment cenv$scrdir but found nothing.")
    invsible(0)
  }
  if(is.null(outpath)){
    message("Please indicate a folder used by CAPRI as 'resout'. ",
            "\nTried to get it from the CAPRI environment cenv$scrdir but found nothing.")
    invsible(0)
  }else{
    conpath <- outpath
    if(! grepl("/$", conpath)) conpath <- paste0(conpath, "/")
  }
  # if(! exists("commonname")){
  #   commonname <- format(Sys.time(), "%Y%m%d%H%M")
  # }
  
  #cat("\ntpath=", tpath)
  if(runasbatch == 1){
    if(is.null(nruns)) {
      flsn <- list.files(path=tpath, 
                         pattern="^[0-9]*$", 
                         recursive=FALSE, 
                         full.names = FALSE)
      flsn <- flsn[order(as.numeric(flsn))]
    }else{
      
      flsn <- as.character(nruns)
      
    }
    nruns <- length(flsn)
  }else{
    
    message("So far used only for batch runs")
    
  }
  print(flsn)
  
  for (i in 1:nruns){
    #for (i in 1:1){
    if(i == 1) iter_chgtable <- data.table()
    if(i == 1) iter_chgmax <- data.table()
    if(i == 1) iter_chgmxmx <- data.table()
    stepfile <- paste0(tpath, "/", flsn[i], "/stepOutput.gdx")
    #cat("\n", stepfile)
    if(file.exists(stepfile)){
      cat("\n", stepfile)
      step<-try(rgdx.param(stepfile, "stepOutput"), silent=TRUE)
      if(class(step)=="data.frame"){
        step<-as.data.table(step)
        setnames(step, paste0(".i", 1:5), c("RALL", "COLS", "ROWS", "Y", "STEP"))
        
        iter_chg <- step[COLS == 'iter_chg']
        time <- step[COLS == 'TIME']
        timesteps <- unique(time$STEP)
        haslast <- "LST" %in% timesteps
        laststep <- if(haslast){"LST"}else if(sum(grepl("^S", timesteps))>0){timesteps[max(which(grepl("^S", timesteps)))]}
        tottime <- if(haslast){
          time[STEP == "LST" & ROWS == "TOTSTEP"]$stepOutput/3600
        }else{
          sum(time[STEP %in% timesteps[which(grepl("^S", timesteps))] & ROWS == "TOTSTEP"]$stepOutput)/3600
        }
        save(list=objects(), file="tmp.rdata")
        nsteps <- time[STEP %in% timesteps[which(grepl("^S", timesteps))] & ROWS=='Start']$STEP
        lsteps <- nsteps[length(nsteps)]
        numinfes <- time[STEP %in% timesteps[which(grepl("^S", timesteps))] & ROWS=="NUMINFES_Market" & STEP %in% nsteps]$stepOutput
        numinfes[is.null(numinfes)] <- 0
        suminfes <- time[STEP %in% timesteps[which(grepl("^S", timesteps))] & ROWS=="SUMINFES_Market" & STEP == lsteps]$stepOutput
        curtime <- as.numeric(format(Sys.time(), "%Y%m%H%M"))
        if(nrow(iter_chg)>0){
          iter_chg$SCEN <- i
          
          # Keep only the maximum changes per country and step
          iter1 <- iter_chg[, max:= max(.SD), .SDcols = "stepOutput", by = .(RALL, STEP)]
          iter1 <- iter1[stepOutput == max]
          iter_chgtable <- rbind(iter_chgtable, iter1)
          
          
          iter2 <- iter_chg[ROWS == 'MAX']
          iter2 <- iter2[,max:=max(.SD), .SDcols = "stepOutput", by = .(STEP)]
          iter3 <- iter2[stepOutput == max]
          iter3 <- rbind(iter3, list("TOT", "iter_chg", "MAX", "", "TIME", tottime, i, ""))
          #iter3 <- rbind(iter3, list("TOT", "iter_chg", "MAX", "", "CURTIME", curtime, i, ""))
          iter3 <- rbind(iter3, list("TOT", "iter_chg", "MAX", "", "STOP", as.logical(haslast), i, ""))
          if(length(nsteps)>0){
            for (sp in 1:length(nsteps)){
              iter3 <- rbind(iter3, list("NUMINFES", "iter_chg", "MAX", "", nsteps[sp], numinfes[sp], i, ""))
            }
            iter3 <- rbind(iter3, list("NUMINFES", "iter_chg", "MAX", "", "SUMINFES", suminfes[1], i, ""))
          }
          iter_chgmax <- rbind(iter_chgmax, iter2)
          iter_chgmxmx <- rbind(iter_chgmxmx, iter3)
        }
      }
    }
  }
  # iter_chgtable <- dcast.data.table(iter_chgtable, RALL + SCEN + ROWS ~ STEP, value.var="stepOutput")
  # iter_chgmax <- dcast.data.table(iter_chgmax, RALL + SCEN ~ STEP, value.var="stepOutput")
  # iter_chgmxmx <- dcast.data.table(iter_chgmxmx, SCEN + RALL ~ STEP, value.var="stepOutput")
  #commonname <- paste0(conpath, commonname)
  save(list=objects(), file="tmp283.rdata")
  
  if(length(iter_chgtable)>0){
    iter_chgtable <- dcast.data.table(iter_chgtable, STEP + RALL + SCEN ~ ROWS, value.var="stepOutput")
    iter_chgmax <- dcast.data.table(iter_chgmax, STEP + SCEN ~ RALL, value.var="stepOutput")
    iter_chgmxmx <- dcast.data.table(iter_chgmxmx, STEP + RALL ~ SCEN, value.var="stepOutput")
    iter_chgmxmxtot <- iter_chgmxmx[RALL %in% c("TOT", "NUMINFES")]
    stepss <- sort(unique(as.numeric(gsub("S", "", iter_chgmxmxtot[!STEP%in%c("SUMINFES", "STOP") & grepl("S", STEP)]$STEP))))
    stepss <- paste0("S", stepss)
    stepst <- stepss[1:(length(stepss)-1)]
    stepsl <- stepss[length(stepss)]
    # iter_chgmxmxtot <- rbind(iter_chgmxmxtot[grepl("^S[1-9]", iter_chgmxmxtot$STEP)], 
    #                          iter_chgmxmxtot[!grepl("^S[1-9]", iter_chgmxmxtot$STEP)])
    iter_chgmxmxtot <- rbind(iter_chgmxmxtot[  STEP %in% stepst], 
                             iter_chgmxmxtot[  STEP %in% stepsl],
                             iter_chgmxmxtot[! STEP %in% stepss])
    iter_chgmxmxtot <- rbind(iter_chgmxmxtot[! STEP %in% stepss & RALL == "TOT"],
                             iter_chgmxmxtot[  STEP %in% stepss & RALL == "TOT"],
                             iter_chgmxmxtot[! STEP %in% stepss & RALL == "NUMINFES"],
                             iter_chgmxmxtot[  STEP %in% stepss & RALL == "NUMINFES"])
    #cat("\nSave file ", paste0(commonname, "_stepreport.rdata"))
    
    # Fill empty columns to 'see' gaps
    n <- as.numeric(names(iter_chgmxmx)[! names(iter_chgmxmx) %in% c("STEP", "RALL")])
    nadd <- length(as.character(1:max(n))[! (1:max(n)) %in% n])>0
    if(nadd){iter_chgmxmx <- iter_chgmxmx[, as.character(1:max(n))[! (1:max(n)) %in% n] :=  ""]}
    iter_chgmxmx <- iter_chgmxmx[, .SD, .SDcols = c("STEP", "RALL", as.character(1:max(n)))]
    n <- as.numeric(names(iter_chgmxmxtot)[! names(iter_chgmxmxtot) %in% c("STEP", "RALL")])
    if(nadd){iter_chgmxmxtot <- iter_chgmxmxtot[, as.character(1:max(n))[! (1:max(n)) %in% n] :=  ""]}
    iter_chgmxmxtot <- iter_chgmxmxtot[, .SD, .SDcols = c("STEP", "RALL", as.character(1:max(n)))]
    save(iter_chgtable, iter_chgmax, iter_chgmxmx, iter_chgmxmxtot, file=gsub(".xlsx", ".rdata", con))
    #View(iter_chgmxmxtot, paste0(basename(commonname), format(Sys.time(), "%H%M")))
    View(iter_chgmxmxtot, paste0(basename(commonname)))
  }
  
  
  save(list=objects(), file="284.rdata")
  if(require(xlsx)){
    if(file.exists(con)){
      wb <- loadWorkbook(con)
      ws <- getSheets(wb)
      if(! is.null(ws)) {
        wsn <- unlist(lapply(1:length(ws), function(x) ws[[x]]$getSheetName()))
        for(iname in c("iterchg", "iterchgmx", "iterchgmxmx", "iterchgmxmxtot")){
          if(iname %in% wsn) {
            #cat("\n Remove Sheet ", iname, " from ", con)
            removeSheet(wb, iname)
            saveWorkbook(wb, file=con)
          }
        }
      }
    }
    #write.xlsx(x=iter_chgtable,  file=con, sheetName="iterchg", col.names=TRUE, row.names=FALSE, showNA=FALSE, append=TRUE)
    save(list=objects(), file="ch.rdata")
    if(length(iter_chgmax)>0){
      #write.xlsx(x=iter_chgmax,    file=con, sheetName="iterchgmx", col.names=TRUE, row.names=FALSE, showNA=FALSE, append=TRUE)
      write.xlsx(x=iter_chgmxmx,   file=con, sheetName="iterchgmxmx", col.names=TRUE, row.names=FALSE, showNA=FALSE, append=TRUE)
      write.xlsx(x=iter_chgmxmxtot,file=con, sheetName="iterchgmxmxtot", col.names=TRUE, row.names=FALSE, showNA=FALSE, append=TRUE)
    }else{
      cat('\nThere are no data to write into the excel file!')
      iter_chgmxmxtot <- 0
    }    
  }else{
    con <- file(paste0(commonname, "_iterchngtable", ".csv"), open = "wt")
    cat("# ", file = con)
    cat("# Step reports, filtered for iter_chg and focusing on maximum changes ", file = con)
    cat("\n", file = con)
    write.csv(iter_chgtable, row.names = FALSE, quote = FALSE, na="", file=con)
    close(con)
    
    con <- file(paste0(commonname, "_iterchngmax", ".csv"), open = "wt")
    cat("# ", file = con)
    cat("# Step reports, filtered for iter_chg and focusing on maximum changes ", file = con)
    cat("\n", file = con)
    write.csv(iter_chgmax, row.names = FALSE, quote = FALSE, na="", file=con)
    close(con)
    
    con <- file(paste0(commonname, "_iterchngmxmx", ".csv"), open = "wt")
    cat("# ", file = con)
    cat("# Step reports, filtered for iter_chg and focusing on maximum changes ", file = con)
    cat("\n", file = con)
    write.csv(iter_chgmxmx, row.names = FALSE, quote = FALSE, na="", file=con)
    close(con)
    
    con <- file(paste0(commonname, "_iterchngmxmxtot", ".csv"), open = "wt")
    cat("# ", file = con)
    cat("# Step reports, filtered for iter_chg and focusing on maximum changes ", file = con)
    cat("\n", file = con)
    write.csv(iter_chgmxmxtot, row.names = FALSE, quote = FALSE, na="", file=con)
    close(con)
  }
  cat("\nSaved to ", conpath)
  return(iter_chgmxmxtot)
}

checkinfesS50 <- function(tpath){
  
  fls <- list.files(tpath, pattern = "[0-9]*.lst")
  
  for(fl in fls){
    curf <- paste0(tpath, "/", fl)
    if(file.exists(curf)){
      cat(curf, " - ")
      a1 <- try(readLines(curf, warn=FALSE), silent=TRUE)
      s50 <- which(grepl("LOOP.*step.*S50", a1))
      cat(s50)
      if(length(s50) >0){
        a <- a1[s50[length(s50)]:length(a1)]
        rsm <- which(grepl("\\*\\*\\*\\* REPORT SUMMARY : ", a))
        a <- a[1:(rsm[1]+20) ]
        a <- a[!a==""]
        a <- a[!grepl(".*MARGINAL$", a)]
        a <- a[!grepl("GAMS", a)]
        a <- a[!grepl("G e n e r a l", a)]
        a <- a[!grepl("Solution Report", a)]
        a <- a[!grepl("     $", a)]
        a <- a[! duplicated(a)]
        a <- a[!grepl("     EQU", a)]
        a <- a[!grepl("     VAR", a)]
        infes <- which(grepl(".*INFES$", a))
        for(i in infes){
          a[infes] <-paste0(a[infes], a[infes-1])
        }
        a <- a[!grepl("^---- EQU", a)]
        a <- a[!grepl("^---- VAR", a)]
        
        conopt1 <- which(grepl("CONOPT 3", a))
        conopt2 <- which(grepl("DK-2880 Bagsvaerd", a))
        if(length(conopt1)>0) a <- a[c(1:(conopt1-1), (conopt2+1):length(a))]
        
        write(a, file=paste0(cenv$capri, cenv$resdir, "/capmod/", fld, "/", fld, gsub(".lst", "infes.lst", fl)))
      }
    }
  }
}


convbatchdate <- function(batchdate){
  m <-(strsplit(batchdate, "_")[[1]][1])
  m <- which(month.abb == m)
  if(m<10) m <- paste0("0", m)
  d <-as.numeric(strsplit(batchdate, "_")[[1]][2])
  if(d<10) d <- paste0("0", d)
  y <-as.numeric(strsplit(batchdate, "_")[[1]][4])
  h <-as.numeric(strsplit(strsplit(batchdate, "_")[[1]][5], "\\.")[[1]][1])
  M <-as.numeric(strsplit(strsplit(batchdate, "_")[[1]][5], "\\.")[[1]][2])
  if(M<10) M <- paste0("0", M)
  s <-as.numeric(strsplit(strsplit(batchdate, "_")[[1]][5], "\\.")[[1]][3])
  if(s<10) s <- paste0("0", s)
  t <-strsplit(batchdate, "_")[[1]][6]
  if(t=="PM") h <- h+12
  if(h<10) h <- paste0("0", h)
  batchdatetime <- paste0(y, m, d, "_", h, M, s)
  return(batchdatetime)
}
gettemp <- function(globals){
  a <- gsub("'", "", gsub("\\\\", "/", globals[setglobal=="scrdir", r1]))
  if(basename(a) =="1") a <- basename(gsub("/1", "", a))
  return(a)
}


globalsandsteps <- function(batchout, tmpfld=NULL, capmodsubfld, dostep=1, nruns=NULL){
  spath <- paste0(cenv$capri, cenv$resout, "/", capmodsubfld)
  if(is.null(tmpfld)){
    tmpfld <- paste0(convbatchdate(batchout), "global")
  }
  tpath <- paste0(cenv$capri, gsub(basename(cenv$scrdir), tmpfld, cenv$scrdir))
  globals <- loadglobalsfrombatch(batchdir = batchout, tpath=tpath, savepath=spath)
  if(dostep == 1){
    save(list=objects(), file="t.rdata")
    tmptrade <- checkstepreports(tpath=tpath,
                                 commonname=capmodsubfld,
                                 outpath=spath,
                                 nruns=nruns)
  }else{
    tmptrade <- 0
  }
  return(tmptrade) 
}

loadmultipleglobal <- function(savepath = NULL,
                               dp = cenv$resdir,
                               batchf = paste0(cenv$resdir, "/../batchout"),
                               pattern = NULL,
                               batchdir = NULL,
                               logf = NULL){
  fls <- list.files(paste0(cenv$resdir, "/../batchout"), pattern = pattern)
  for(f in fls){a<-f; x <- loadglobalsfrombatch(batchdir=a)}
  
}

cpchkmagpie <- function(temp="temp", capmodsubfld=NULL, n=NULL, 
                        file="chk_kcalMAgPIE",
                        cpfix = TRUE, # Copy also (large) file with input data DATA, p_dataOuttemp, etc.
                        cpstep = FALSE # Copy gdx file 'stepOutput.gdx' (full step output)
                        ){
  tpath <- paste0(cenv$capri, cenv$scrdir, "/../", temp)
  if(! dir.exists(paste0(cenv$capri, cenv$resout, "/", capmodsubfld, "/"))){
    dir.create(paste0(cenv$capri, cenv$resout, "/", capmodsubfld, "/"))
  }
  if(is.null(n)){
    n <- sort(as.numeric(basename(list.dirs(tpath, recursive=FALSE))))
  }
  if(cpfix){
    fxfile <- paste0(cenv$capri, cenv$scrdir, "/../", temp, "/",1,"/", file, "fix.gdx")
    if(file.exists(fxfile)){
      file.copy(fxfile, paste0(cenv$capri, cenv$resout, "/", capmodsubfld, "/", capmodsubfld, "_", file, "fix.gdx"), overwrite=TRUE)
    }
  }
  for(i in n){
    vrfile <- paste0(cenv$capri, cenv$scrdir, "/../", temp, "/",i,"/", file, "var.gdx")
    file.copy(vrfile, paste0(cenv$capri, cenv$resout, "/", capmodsubfld, "/",capmodsubfld , "_", file, "var_", i, ".gdx"), overwrite=TRUE)
  }
  if(cpstep){
  for(i in n){
    vrfile <- paste0(cenv$capri, cenv$scrdir, "/../", temp, "/",i,"/", "stepOutput.gdx")
    file.copy(vrfile, paste0(cenv$capri, cenv$resout, "/", capmodsubfld, "/",capmodsubfld , "_", "stepOutput_", i, ".gdx"), overwrite=TRUE)
  }
  }
  
}


gettmpsubfolders <- function(tpath=cenv$scrdir, runasbatch=1, nruns=NULL){
  
  if(is.null(tpath)){
    message("Please indicate a folder used by CAPRI as 'scrdir'. ",
            "\nTried to get it from the CAPRI environment cenv$scrdir but it is not set.")
    invsible(0)
  }
  #cat("\ntpath=", tpath)
  if(runasbatch == 1){
    if(is.null(nruns)) {
      flsn <- list.files(path=paste0(tpath), 
                         pattern="^[0-9]*$", 
                         recursive=FALSE, 
                         full.names = FALSE)
      flsn <- flsn[order(as.numeric(flsn))]
    }else{
      
      flsn <- as.character(nruns)
      
    }
    nruns <- length(flsn)
  }else{
    
    message("So far used only for batch runs")
    
  }
  return(flsn)
}
