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
