#' Load $setglobals from CAPRI steering files
#' @description The function \code{loadglobalsfrombatch()} reads relevant CAPRI settings and saves them in one file.
#'              For runs in the batch mode it reads them fro the batchout folder.
#'              Alternatively can read multiple CAPRI steering files from the 'log' result folder
#' @param savepath Indicates where the otuput of the function should be saved
#' @param dp Datapath - uses a global environment variable if given; is used only of bathpath or savepath are NULL
#' @param bathpath Used to identify files to read. This concept still needs to be further developed
#' @param batchdir idem
#' @param logf idem



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
