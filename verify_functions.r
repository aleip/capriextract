extractGHGs <- function(date2load = "", reginame="EU27", yearrange){
  
  curfile<-paste0(savepath, "/xobs_", reginame, "_", yearrange, "_")
  load(paste0(curfile, "GHGNtot", date2load, ".rdata"))
  
  selemissions <- c("N2OAPP", "N2OGRA", "N2OHOU", "N2OSTO", "CH4ENT", "CH4MAN", "CH4RIC")
  
  ghgs <- ghgnfsu[rows %in% selemissions]
  ghgtotsource <- ghgs[cols=="UAAR", .(value = sum(value)), by=.(rall, y, rows)]
  
  tmp <- copy(ghgtotsource)  
  tmp[, gas := substr(rows, 1, 3)]
  ghgtot <- tmp[, .(value = sum(value)), by=.(rall, y, gas)]
  ghgtime <- dcast.data.table(ghgtot, rall + gas ~ y, value.var = "value")
  
  ghgtimec <- copy(ghgtime) 
  ghgtimec[, `2016` := 0.5 * (`2014`+`2018`)]
  return(ghgtime)
}

firstderivate <- function(xx, curyears){
  
  for(t in 2:length(curyears)){
    slope <- paste0(curyears[t], "incr")
    t1 <- as.character(curyears[t-1])
    t2 <- as.character(curyears[t])
    xx[, (slope) := get(t2) / get(t1)]
  }
  xx1 <- melt.data.table(xx, id.vars = c("rall", "gas"), variable.name = "y")
  xx1 <- xx1[grepl("incr", y)]
  # Filter those with a relative change above factor 2 
  xx2 <- xx1[(value > 2 | value < 0.5)]
  hist(xx2$value)
}
