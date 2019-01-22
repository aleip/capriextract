#setwd("E:/capriextract")   #xavi
source("capri_packages.r")
source("capri_dirs.r") # Directory settings depending on computer one is working

scope<-"nbalance"
scope<-"nbalancemain" #Only those which are reported in IMPACT
scope<-"tseries_marketbal"
scope<-"feed_marketbal"
scope<-"baseyearnmin"
scope<-"activities"
scope<-"nlca"

source("capri_sets.r")
source("capriextract_functions.r")

getwd()
curcountry<-"AT110000"
curscen<-"epnf_refpol_endotechall_MAgPIE_onlyemptyCUR"
curcols<-mpact
currows<-soilbalpos
curdim5<-""

#xavi181106: from here on, it doesn't work
capridat<-selectrowscolsregi(reload=1, capridat=NULL, cols=curcols,
                             rows=currows, regi=curcountry, curyear=curyear)
capripivot<-dcast(capridat, RALL + COLS ~ ROWS, value.var = "VALUE")

onedataset<-getdata(scope=scope,curcountry,curyear="08")

#alldatasets<-Reduce(rbind,lapply(cntr,function(x) getdata(scope,x)))

alldatasets<-getmultipleyears(scope,cntr,curyears)
capridat<-alldatasets[[1]]
caprimeta<-alldatasets[[2]]

#onefeed<-getfeed(cntr[1])
#allfeed<-Reduce(rbind,lapply(cntr,function(x) getfeed(x)))
#allfedm<-Reduce(rbind,lapply(cntr,function(x) getfedm(x)))
#write.csv(allfedm,paste0("crops2feed",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)



