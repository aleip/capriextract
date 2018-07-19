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

# Countries and regions
nuts0<-as.character(rgdx.set(setfile,ts = TRUE,symName = "NUTS0")[,1])
cntr<-substr(nuts0,1,2)
srnuts2<-as.character(rgdx.set(setfile,ts = TRUE,symName = "SRNUTS2")[,1])
nuts2<-substr(srnuts2,1,4)
rall<-rgdx.set(setfile,ts = TRUE,te=TRUE,symName = "RALL")

scope<-"tseries_marketbal"
scope<-"feed_marketbal"
if(scope=="feed_marketbal"){
    datapath<-"c:/adrian/models/capri/trunk_to_star/output/results/capreg/"
    datapath<-paste0(capridat,"capreg/")
}else if(scope=="tseries_marketbal"){
    datapath<-paste0(serverpath,"Capreg_tseries/")
}
getfeed<-function(curcountry){
    
    #20170708 - Extraction of feed data to send to Olga Gavrilova [oggavrilova@gmail.com]
    #           For IPCC2019 refinement
    
    datafile<-paste0(curcountry,"_12.gdx")
    datafile<-"res_12"
    datafile<-paste0(datapath,datafile)
    datafile<-paste0(datafile,curcountry,".gdx")
    
    
    if(file.exists(datafile)){
        capridat<-rgdx.param(datafile,dataparm)
        names(capridat)<-c("RALL","COLS","ROWS","Y","VALUE")
        
        #Activities: maact and daact
        capridat<-capridat[capridat$COLS%in%c(maact,daact),]
        
        #Columns: feed_rows
        capridat<-capridat[capridat$ROWS%in%c(feed_rows),]
        
        #Restrict to country-data
        capridat<-capridat[grepl("000000",capridat$RALL),]
        
    }else{
        cat("\nFile ",datafile," does not exist!")
    }
}
getfedm<-function(curcountry){
    
    #20170708 - Extraction of feed data to send to Olga Gavrilova [oggavrilova@gmail.com]
    #           For IPCC2019 refinement
    
    datafile<-"res_12"
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,datafile)
    
    
    if(file.exists(datafile)){
        capridat<-rgdx.param(datafile,dataparm)
        names(capridat)<-c("RALL","COLS","ROWS","Y","VALUE")
        
        #Activities: maact and daact
        capridat<-capridat[capridat$COLS%in%c("FEDM"),]
        
        
    }else{
        cat("\nFile ",datafile," does not exist!")
    }
}


selectrowscolsregi<-function(capridat,cols,rows,regi,d){
    capridat<-rgdx.param(d[[1]],d[[2]])
    names(capridat)<-d[[3]]

    #COLS (activities, variables for products)
    capridat<-capridat[capridat$COLS%in%cols,]
    
    #ROWS (products, variables for activities)
    capridat<-capridat[capridat$ROWS%in%rows,]
    
    #Filter regional level 
    capridat<-capridat[capridat$RALL%in%regi,]

    #Filter time dimension
    capridat<-capridat[capridat$Y%in%d[[4]],]
    #capridat<-capridat[,setdiff(names(capridat),c("Y"))]
    
    return(capridat)
}


getmarketbalance<-function(capridat,d){
    rows<-c(as.character(frows$i),as.character(ico$i))
    cols<-c(as.character(frmbal_cols$i),as.character(mrkbal_cols$i))
    cols<-c(as.character(frmbal_cols$i),"FEDM")
    regi<-nuts0
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d)
    return(capridat)
}

getfeed<-function(caprida,d){
    rows<-feed_rows
    cols<-c(maact,daact)
    regi<-nuts0
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d)
}

getdata<-function(scope,curcountry){
    if(scope=="feed_marketbal"){
        dataparm<-"DATA2"
        regi<-nuts0
        datafile<-"res_12"
        datafile<-paste0(datafile,curcountry,".gdx")
        datafile<-paste0(datapath,datafile)
        datanames<-c("RALL","COLS","ROWS","Y","VALUE")
        d<-list(datafile,dataparm,datanames,"Y")
    }
    if(scope=="tseries_marketbal"){
        dataparm<-"DATA"
        regi<-nuts0
        datafile<-"res_12"
        datafile<-paste0(curcountry,"_12.gdx")
        datafile<-paste0(datapath,datafile)
        datanames<-c("RALL","COLS","ROWS","Y","VALUE")
        d<-list(datafile,dataparm,datanames,c(2000:2014))
    }
    if(file.exists(datafile)){
        capridat<-rgdx.param(datafile,dataparm)
        names(capridat)<-datanames
        caprimeta<-rgdx.set(datafile,te=TRUE,ts = TRUE,symName = "meta")
        
        
        if(scope=="feed_marketbal"){
            capridat<-getmarketbalance(capridat,d)
            capridat<-rbind(capridat,getfeed(capridat,d))
        }
        if(scope=="tseries_marketbal"){
            capridat<-getmarketbalance(capridat,d)
        }
        return(capridat)
    }
}

onedataset<-getdata(scope=scope,curcountry=cntr[1])
alldatasets<-Reduce(rbind,lapply(cntr,function(x) getdata(scope,x)))

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

if(scope=="feed_marketbal")con<-file(paste0("data_feedintake_mktbal",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
if(scope=="tseries_marketbal")con<-file(paste0("data_mktbal_tseries",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
if(scope=="feed_marketbal")   writeLines(paste0("# Data Source: CAPREG - 21.03.2018"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Data Source: CAPREG-time series - 06.04.2018"),con)
writeLines(paste0("# Repository: https://svn1.agp.uni-bonn.de/svn/capri_out_after2016/results/Capreg_tseries"),con)
writeLines(paste0("# Download: 201800718"),con)
if(scope=="feed_marketbal")writeLines(paste0("# Files used: res_12%MSALL%.gdx"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Files used: %MSALL%_12.gdx"),con)
if(scope=="feed_marketbal")   writeLines(paste0("# Parameter: DATA2"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Parameter: DATA"),con)
writeLines(paste0("# Suggested quotation:"),con)
writeLines(paste0("#                      Wolfgang Britz and Peter Witzke (editors): CAPRI model documentation 2015. Available at https://svn1.agp.uni-bonn.de/svn/capri/trunk/doc. [This is the latest version. A public (older) version is available at the CAPRI model website at https://www.capri-model.org/dokuwiki/doku.php?]"),con)
writeLines(paste0("#                      Markus Kempen and Peter Witzke (2018). Improvement of the stable release of the CAPRI model: Fertilizer and Feed allocation routines. Deliverable 3: Revised feed module for CAPRI. Specific contract No. Joint Research Centre 154208.X39"),con)
if(scope=="tseries_marketbal")writeLines(paste0("#                      Responsible for (integrating feed data into) the time series: Alexander Gocht"),con)
if(scope=="feed_marketbal") writeLines(paste0("# Definition (livestock activities) of columns and rows (feed stuff)"),con)
writeLines(paste0("# Definition columns: market balance - rows: crop and animal product."),con)
writeLines(paste0("# For definition of sets see other files"),con)
#write.csv(allfeed,paste0("feedintakebyanimalactivity",format(Sys.time(), "%Y%m%d"),".csv"))
write.csv(alldatasets,con,row.names=FALSE)

close(con)



