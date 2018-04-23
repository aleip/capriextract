# Request 20180420: Provide data as in Leip et al. (2015) 
# https://zenodo.org/record/58514#.Wt2HGr7RA-X
# per country for crops, including land use
# 
# .... to be used in the paper: 
# Parodi et al. Future foods: towards a sustainable and healthy diet for a growing population
# 
# Use of the following data: x:\adrian\models\capri\dndc\results\20110722\nitrogen\nitrogenlca.gdx
# Variables as in x:\adrian\google\literature\manuscripts\leip_livestockenvironment\livestockenv_erl.v2~20151015_IR.xlsx
# # 


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

nitrogenlcapath<-"x:\\adrian\\models\\capri\\dndc\\results\\20110722\\nitrogen\\"
setfile<-paste0(getwd(),"/LAPMcapdis_END.gdx")
setfile<-paste0(nitrogenlcapath,"nitrogenlca_sets.gdx")
datafile<-paste0(nitrogenlcapath,"nitrogenlca.gdx")

# Work only on crop products
cropoexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "CROPO")
cropo<-as.character(cropoexp[,1])
names(cropoexp)<-c("CROPO","Description")
rows<-cropo

# Countries and regions
nuts0<-as.character(rgdx.set(setfile,ts = TRUE,symName = "NUTS0")[,1])
cntr<-substr(nuts0,1,2)
srnuts2<-as.character(rgdx.set(setfile,ts = TRUE,symName = "SRNUTS2")[,1])
nuts2<-substr(srnuts2,1,4)

# GHG emissions
# Sets according to x:\adrian\models\capri\dndc\gams\capreg\nitrogenlcasets.gms
#
# SET sAREA(sNGHG) /LCAREA/;
# SET sGHGcdir(sNGHG) GHG fluxes - direct - from crop activities according to IPCC       /N2OAPL,N2OGRC,N2OSYN,N2OCRO,CH4RIC/; #N2OFIX,
# SET sGHGclus(sNGHG) GHG fluxes from land use - cultivated histosols according to IPCC  /N2OHIS,CO2HIS,CO2SEQ/;
# SET sGHGcind(sNGHG) GHG fluxes - indirect - form crop activities according to IPCC     /N2OLEA,N2OAMM/;
# SET sGHGcene(sNGHG) GHG fluxes from energy use in crop-production                      /N2OPRD,CO2PRD,CO2DIC,CO2OFC,CO2ELC,CO2INC,CO2SEE,CO2PPT/
# SET sGHGcluc(sNGHG) GHG fluxes from land use change in LCA for livestock production    /N2OBUR,N2OSOI,CH4BUR,CO2SOI,CO2BIO/;
# sGHGc(sNGHG)=sGHGcdir(sNGHG)+sGHGclus(sNGHG)+sGHGcind(sNGHG)+sGHGcluc(sNGHG)+sGHGcene(sNGHG);
sghgexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "sGHGc")
sghg<-as.character(sghgexp[,1])
sghg<-c(sghg,"LCAREA")
cols<-c(sghg,"GROF")

datapath<-"c:/adrian/models/capri/trunk_to_star/output/results/capreg/"
datapath<-paste0(capridat,"capreg/")
datapath<-nitrogenlcapath
dataparm<-"PRDD"


getdata<-function(datafile,dataparm,variables){
    capridat<-rgdx.param(datafile,dataparm)
    names(capridat)<-c("NUTS0","SRNUTS2","COLS","ROWS","Y","VALUE")

    #COLS (activities, variables for products)
    capridat<-capridat[capridat$COLS%in%cols,]
    
    #ROWS (products, variables for activities)
    capridat<-capridat[capridat$ROWS%in%rows,]
    
    #Filter nuts2 to only nuts2
    capridat<-capridat[capridat$SRNUTS2%in%srnuts2,]
    capridat<-capridat[,setdiff(names(capridat),c("Y"))]
    
    #Extract for weighting
    caprigrof<-filter(capridat,COLS=="GROF")
    caprigrof<-caprigrof[,setdiff(names(caprigrof),c("COLS"))]
    setnames(caprigrof,"VALUE","GROF")
    
    capridat<-filter(capridat,COLS!="GROF")
    
    capridat<-merge(capridat,caprigrof,by=setdiff(names(capridat),c("COLS","VALUE")))
    capridat$CONV[grepl("N2O",capridat$COLS)]<-298 #IPCC2007-AR4 to be consistent with Leip et al. (2015)
    capridat$CONV[grepl("CH4",capridat$COLS)]<-1 #was already converted with 25 same source
    capridat$CONV[grepl("CO2",capridat$COLS)]<-1 #was already converted with 25 same source
    capridat$CONV[grepl("AREA",capridat$COLS)]<-1 #was already converted with 25 same source
    capridat$VALGROF<-capridat$VALUE*capridat$GROF*capridat$CONV

    ghgcountry<-aggregate(capridat[,c("GROF","VALGROF")],by=as.list(capridat[,c("NUTS0","ROWS","COLS")]),sum,na.rm=TRUE)
    ghgcountry$VALUE<-ghgcountry$VALGROF/ghgcountry$GROF
    ghgcountry<-ghgcountry[,setdiff(names(ghgcountry),c("GROF","VALGROF"))]
    ghgcountry<-dcast(ghgcountry,NUTS0+COLS~ROWS,value.var="VALUE")
    
    area<-filter(ghgcountry,COLS=="LCAREA")
    ghgcountry<-filter(ghgcountry,COLS!="LCAREA")
    
    ghgtotal<-aggregate(ghgcountry[,intersect(cropo,names(ghgcountry))],
                        by=list(ghgcountry$NUTS0),
                        sum,na.rm=TRUE,drop=TRUE)

}












onefeed<-getfeed(cntr[1])
allfeed<-Reduce(rbind,lapply(cntr,function(x) getfeed(x)))
allfedm<-Reduce(rbind,lapply(cntr,function(x) getfedm(x)))

write.csv(rbind(maactexp,daactexp),paste0("animalactivities",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(feed_rowsexp,paste0("feedingstuff",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(feed_to_o,paste0("feedingstuffcrops",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(allfedm,paste0("crops2feed",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
con<-file(paste0("feedintakebyanimalactivity",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
writeLines(paste0("# Data Source: CAPREG - 20.10.2017"),con)
writeLines(paste0("# Repository: https://svn1.agp.uni-bonn.de/svn/capri_out_after2016/results/capreg"),con)
writeLines(paste0("# Download: 20171113"),con)
writeLines(paste0("# Files used: res_12%MSALL%.gdx"),con)
writeLines(paste0("# Parameter: DATA2"),con)
writeLines(paste0("# Definition (livestock activities) of columns and rows (feed stuff) selected: see other files"),con)
#write.csv(allfeed,paste0("feedintakebyanimalactivity",format(Sys.time(), "%Y%m%d"),".csv"))
write.csv(allfeed,con,row.names=FALSE)

close(con)



