#Used Packages
rm(list=objects())
library(data.table)
library(plyr)
#library(devtools)
#install_github("hadley/dplyr@master")   #to install the last version of dplyr
library(dplyr)    #installed the dev version from https://github.com/hadley/dplyr
#library(reshape)
library(reshape2)
library(stats)
library(gdxrrw)

#Set working directory and load of general (used to update all) datasets.
# These databases have been updated by Renate Koeble and delivered in the folder capri/hsu2_database_update_2016_02
curfolder<-"capriextract"
if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
    gamspath<-"C:/GAMS/win64/24.4"
    workpath<-"C:/adrian/tools/rprojects/"
    capridat<-"C:/adrian/models/capri/trunk20160810/dat/"
}else if(Sys.info()[4]=="D01RI1600881"){ #checks machine name
    serverpath<-"\\\\s-jrciprap246p.jrc.it/dev/CAPRImodel/capri_out_after2016/results/"
    serverpath<-"\\\\s-jrciprap246p.jrc.it/dev/CAPRImodel/capriresults_ecampa3/"
    gamspath<-"X:/dev/GAMS/win64/24.4"
    gamspath<-"C:/Program Files/GAMS/win64/24.8"
    workpath<-"x:/adrian/tools/rprojects/"
    capridat<-"x:/adrian/models/capri/trunk20160810/dat/"
    capridat<-"x:\\dev\\capri_out_after_2016\\"
}else if(Sys.info()[4]=="MacBook-Pro-de-Xavier.local"){ #checks machine name
    workpath<-"/Users/xavi/Documents/JRC_MARS/hsu2_statistics_xavi2/"
    gamspath<-"/Applications/GAMS24.6/sysdir"
    capridat<-workpath
}else{
    workpath<-"X:/MARS_disaggregation/hsu2_statistics_xavi2/"
    capridat<-workpath
    gamspath<-"X:/GAMS/win64/24.7"
}
workpath<-paste0(workpath,curfolder,"/")
setwd(workpath)

#link with gams directory
igdx(gamspath)

setfile<-paste0(getwd(),"/LAPMcapdis_END.gdx")
setfile<-"x:\\adrian\\models\\capri\\dndc\\results\\20110722\\nitrogen\\nitrogenlca_sets.gdx"
rows<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "ROWS")
# set FROWS / set.fco,comi,comf,beef,pork,sgmi,sgmf,sgmt,eggs,poum,oani/;
# set cropo / set.fco,set.ico/;
# set animo_rows / die oben + set.oyani_rows,mann,manp,mank,lres/;
frows<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FROWS")
ico<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "ICO")
mpactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MPACT")
mpact<-as.character(mpactexp[,1])
maactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MAACT")
maact<-as.character(maactexp[,1])
daactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "DAACT")
daact<-as.character(daactexp[,1])
names(maactexp)<-c("MAACT","Description")
names(daactexp)<-c("MAACT","Description")
feed_rowsexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FEED_ROWS")
feed_rows<-as.character(feed_rowsexp[,1])
feed_to_o<-(rgdx.set(setfile,ts = TRUE,symName = "FEED_TO_O"))
frmbal_cols<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "frmbal_cols"))
mrkbal_cols<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MRKBAL_COLS"))
nbil<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NBIL"))
# Countries and regions
nuts0_exp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NUTS0")
nuts0<-as.character(nuts0_exp[,1])
cntr<-substr(nuts0,1,2)
srnuts2<-as.character(rgdx.set(setfile,ts = TRUE,symName = "SRNUTS2")[,1])
nuts2<-substr(srnuts2,1,4)
rall<-rgdx.set(setfile,ts = TRUE,te=TRUE,symName = "RALL")
meta2keep<-c("DATE OF VERSION","NAME OF PROCESSOR ORGANISATION","User","Regional breakdown")

scope<-"nbalance"
scope<-"nbalancemain" #Only those which are reported in IMPACT
scope<-"tseries_marketbal"
scope<-"feed_marketbal"
scope<-"activities"

datanames<-c("RALL","COLS","ROWS","Y","VALUE")
if(scope%in%c("feed_marketbal","activities")){
    datapath<-"c:/adrian/models/capri/trunk_to_star/output/results/capreg/"
    datapath<-paste0(capridat,"capreg/")
    datapath<-paste0(serverpath,"capreg/")
    svnpath<-"https://svn.jrc.es/repos/GHG/ECAMPA3Results/results"
}else if(scope%in%c("tseries_marketbal")){
    datapath<-paste0(serverpath,"Capreg_tseries/")
}else if(grepl("nbalance",scope)){
    datapath<-paste0("\\\\s-jrciprap246p.jrc.it/dev/CAPRImodel/capriresults_ecampa3/","Capreg_tseries/")
}
if(scope%in%c("feed_marketbal","activities")){
    dataparm<-"DATA2"
}else if(grepl("tseries_marketbal|nbalance",scope)){
    dataparm<-"DATA"
}    
if(scope%in%c("feed_marketbal","activities")){
    ydim<-"Y"
    curyears<-c("08","12")
}else if(grepl("tseries_marketbal|nbalance",scope)){
    ydim<-c(1990:2014)
    curyears<-""
}    
if(scope%in%c("feed_marketbal","tseries_marketbal","activities")){
    regi<-c(nuts0,srnuts2)
}else if(grepl("nbalance",scope)){
    regi<-srnuts2
}    
source("capriextract_functions.r")

#curcountry<-"AT"
#onedataset<-getdata(scope=scope,curcountry)
#alldatasets<-Reduce(rbind,lapply(cntr,function(x) getdata(scope,x)))

alldatasets<-getmultipleyears(scope,cntr,curyears)
capridat<-alldatasets[[1]]
caprimeta<-alldatasets[[2]]

#onefeed<-getfeed(cntr[1])
#allfeed<-Reduce(rbind,lapply(cntr,function(x) getfeed(x)))
#allfedm<-Reduce(rbind,lapply(cntr,function(x) getfedm(x)))
#write.csv(allfedm,paste0("crops2feed",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)

write.csv(rbind(maactexp,daactexp),paste0("set_animalactivities",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(feed_rowsexp,paste0("set_feedingstuff",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(feed_to_o,paste0("set_feedingstuffcomposition",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rows,paste0("set_rows",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(frows,ico),paste0("set_crop_and_animalproducts",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(frmbal_cols,mrkbal_cols),paste0("set_products_marketbalance",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(rall),paste0("set_rall",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)

metafile<-paste0("data_",scope,"_meta",format(Sys.time(), "%Y%m%d"),".csv")
con<-file(paste0("data_",scope,format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
if(scope%in%c("feed_marketbal","activities")) writeLines(paste0("# Data Source: CAPREG - 21.03.2018"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Data Source: CAPREG-time series - 06.04.2018"),con)
writeLines(paste0("# Repository: ",svnpath),con)
if(scope%in%c("feed_marketbal","activities"))writeLines(paste0("# Files used: res_%Y%%RALL%.gdx"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Files used: %MSALL%_12.gdx"),con)
if(scope%in%c("feed_marketbal","activities")) writeLines(paste0("# Parameter: DATA2"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Parameter: DATA"),con)
writeLines(paste0("# Suggested quotation:"),con)
writeLines(paste0("#                      Wolfgang Britz and Peter Witzke (editors): CAPRI model documentation 2015. Available at https://svn1.agp.uni-bonn.de/svn/capri/trunk/doc. [This is the latest version. A public (older) version is available at the CAPRI model website at https://www.capri-model.org/dokuwiki/doku.php?]"),con)
writeLines(paste0("#                      Markus Kempen and Peter Witzke (2018). Improvement of the stable release of the CAPRI model: Fertilizer and Feed allocation routines. Deliverable 3: Revised feed module for CAPRI. Specific contract No. Joint Research Centre 154208.X39"),con)
if(scope=="tseries_marketbal")writeLines(paste0("#                      Responsible for (integrating feed data into) the time series: Alexander Gocht"),con)
if(scope%in%c("feed_marketbal","activities")) writeLines(paste0("# Definition (livestock activities) of columns and rows (feed stuff)"),con)
if(scope%in%c("feed_marketbal","tseries_marketbal"))writeLines(paste0("# Definition columns: market balance - rows: crop and animal product."),con)
if(scope%in%c("feed_marketbal","tseries_marketbal"))writeLines(paste0("# For definition of sets see files set_*"),con)
writeLines(paste0("#\n# Detailed metainformation see file ",metafile),con)
if(scope%in%c("activities")) writeLines(paste0("#Units: crop activities in [1000 ha]; livestock activities in [1000 head]; poultry activities [1000000 heads]"),con)
write.csv(capridat,con,row.names=FALSE)
close(con)


con<-file(metafile,open = "wt")
write.csv(caprimeta[caprimeta$.i4%in%meta2keep,],con,row.names=FALSE)
close(con)


