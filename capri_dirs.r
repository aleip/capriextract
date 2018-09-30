#Set working directory and load of general (used to update all) datasets.
# These databases have been updated by Renate Koeble and delivered in the folder capri/hsu2_database_update_2016_02
curfolder<-"capriextract"
serverpath<-"\\\\s-jrciprap246p.jrc.it/dev/CAPRImodel/capriresults_ecampa3/"

if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
  gamspath<-"C:/GAMS/win64/24.8"
  curdir<-"x:/dev/"
  workpath<-"x:/adrian/tools/rprojects/"
  capridat<-"C:/adrian/models/capri/trunk20160810/dat/"
  datapath<-"x:/dev/capriresults_ecampa3/"
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
ecampa3res<-paste0(curdir,"capriresults_ecampa3/")
workpath<-paste0(workpath,curfolder,"/")
setwd(workpath)
#link with gams directory
igdx(gamspath)
