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
    gamspath<-"C:/GAMS/win64/24.4"
    workpath<-"x:/adrian/tools/rprojects/"
    capridat<-"x:/adrian/models/capri/trunk20160810/dat/"
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

setfile<-"c:\\adrian\\models\\capri\\dndc\\results\\20110722\\nitrogen\\nitrogenlca_sets.gdx"
maactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MAACT")
maact<-as.character(maactexp[,1])
daactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "DAACT")
daact<-as.character(daactexp[,1])
feed_rowsexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FEED_ROWS")
feed_rows<-as.character(feed_rowsexp[,1])
feed_to_o<-(rgdx.set(setfile,ts = TRUE,symName = "FEED_TO_O"))
nuts0<-as.character(rgdx.set(setfile,ts = TRUE,symName = "NUTS0")[,1])
cntr<-substr(nuts0,1,2)
datapath<-"c:/adrian/models/capri/trunk_to_star/output/results/capreg/"
dataparm<-"DATA2"


getfeed<-function(curcountry){

    #20170708 - Extraction of feed data to send to Olga Gavrilova [oggavrilova@gmail.com]
    #           For IPCC2019 refinement
    
    datafile<-"res_12"
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,datafile)
    
    if(file.exists(datafile)){
        capridat<-rgdx.param(datafile,dataparm)
        
        #Activities: maact and daact
        capridat<-capridat[capridat$i2%in%c(maact,daact),]
        
        #Columns: feed_rows
        capridat<-capridat[capridat$i3%in%c(feed_rows),]
        
        #Restrict to country-data
        capridat<-capridat[grepl("000000",capridat$i1),]
        
    }else{
        cat("\nFile ",datafile," does not exist!")
    }
}

onefeed<-getfeed(cntr[1])
allfeed<-Reduce(rbind,lapply(cntr,function(x) getfeed(x)))

write.csv(allfeed,paste0("feedintakebyanimalactivity",format(Sys.time(), "%Y%m%d"),".csv"))
write.csv(rbind(maactexp,daactexp),paste0("animalactivities",format(Sys.time(), "%Y%m%d"),".csv"))
write.csv(feed_rowsexp,paste0("feedingstuff",format(Sys.time(), "%Y%m%d"),".csv"))
write.csv(feed_to_o,paste0("feedingstuffcrops",format(Sys.time(), "%Y%m%d"),".csv"))



