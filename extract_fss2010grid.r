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
    gamspath<-"X:/dev/GAMS/win64/24.4"
    gamspath<-"C:/Program Files/GAMS/win64/24.8"
    workpath<-"x:/adrian/tools/rprojects/"
    capridat<-"x:/adrian/models/capri/trunk20160810/dat/"
    capridat<-"x:\\dev\\capri_out_after_2016\\"
    capridat<-"X:/dev/ec3branch/dat/capdishsu/fssdata/"
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

# Get grid codes
setfile<-paste0(capridat,"p_hsu_grid10n23.gdx")
grids<-rgdx.set(setfile,te=FALSE,ts = TRUE,symName = "grid10n23")

# Get FSS codes
fsscode<-read.table("fsscode.r",header=TRUE)

# Get countries
setfile<-paste0(capridat,"../../../gams/capdis/capdis_relevantsets_sets.gdx")
nuts0<-as.character(rgdx.set(setfile,ts = TRUE,symName = "NUTS0")[,1])
cntr<-substr(nuts0,1,2)
cntr<-cntr[!cntr%in%c("HR","TU")]

datapath<-capridat
dataparm<-"results_gridunits"


getgriddata<-function(curcountry){

    #20170708 - Extraction of feed data to send to Olga Gavrilova [oggavrilova@gmail.com]
    #           For IPCC2019 refinement
    
    print(curcountry)
    datafile<-"fss2010grid_"
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,datafile)
    
    
    if(file.exists(datafile)){
        capridat<-rgdx.param(datafile,dataparm)
        names(capridat)<-c("grid10kmn23","fssCode","VALUE")
    }else{
        cat("\nFile ",datafile," does not exist!")
    }
    
    return(capridat)
}

griddata<-getgriddata(cntr[1])
allgriddata<-Reduce(rbind,lapply(cntr,function(x) getgriddata(x)))
allgriddata<-as.data.table(allgriddata)
allgriddata$VALUE<-as.numeric(allgriddata$VALUE)
allgriddataX<-dcast(allgriddata,grid10kmn23 ~ fssCode,value.var="VALUE",fun.aggregate=sum)

con<-file(paste0("fss2010grid10kmn23_gapfilled",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
writeLines(paste0("# Data Source: CAPRI FSS2010 Gap Filling - November 2017"),con)
writeLines(paste0("# Files used: fss2010grid_%NUTS0%.gdx"),con)
writeLines(paste0("# Parameter: results_gridunits"),con)
write.csv(allgriddataX,con,row.names=FALSE)

close(con)



