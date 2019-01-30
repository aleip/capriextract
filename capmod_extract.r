loadcapmod <- function(){
  
  
  # Default batchdir location
  batchout <- paste0(datapath, "../batchout")
  batchdirs <- file.info(list.files(batchout, full.names=TRUE))
  batchdirs <- batchdirs[with(batchdirs, order(as.POSIXct(mtime),decreasing=TRUE)), ]
  batchdir <- rownames(batchdirs)[1]
  
  # Check fortra_X files
  fortran <- list.files(batchdir, pattern = "fortra_[0-9]*.gms")
  
  # Extract setglobals
  x<-1
  con <- file(paste0(batchdir, "/", fortran[x]), open = "r")
  setglobal <- readLines(con)
  setglobal <- setglobal[grepl("setglobal", setglobal, ignore.case=TRUE)]
  a <- strsplit(setglobal[86], " ")[[1]]
  a <- c(a[2], paste(a[3:length(a)], collapse=" "))
  
  
  
  startextract('capmod')
  fls <- getfilesfromfolder(curfol = paste0("\\\\s-jrciprap246p.jrc.it/dev/CAPRImodel/users/leipadr/results/capmod/20190125_mititech"), 
                            flag = 'mititech', 
                            reference="res_2_0830mtr_rd_refON.gdx"
                            )
  
  
  
  
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
