# Defines sets for extraction of CAPRI results

datanames<-c("RALL","COLS","ROWS","Y","VALUE")
data3dim<-c("RALL","COLS","ROWS","VALUE")
data4dim<-c("RALL","COLS","ROWS","Y","VALUE")
data5dim<-c("RALL","EMPTY","COLS","ROWS","Y","VALUE")
  
# if(scope%in%c("feed_marketbal","activities")|grepl("baseyear",scope)){
#   datapath<-"c:/adrian/models/capri/trunk_to_star/output/results/capreg/"
#   datapath<-paste0(capridat,"capreg/")
#   datapath<-paste0(serverpath,"capreg/")
#   svnpath<-"https://svn.jrc.es/repos/GHG/ECAMPA3Results/results"
# }else if(scope%in%c("tseries_marketbal")){
#   datapath<-paste0(serverpath,"Capreg_tseries/")
# }else if(grepl("nbalance",scope)){
#   datapath<-paste0("\\\\s-jrciprap246p.jrc.it/dev/CAPRImodel/capriresults_ecampa3/","Capreg_tseries/")
# }else if(scope%in%c("nlca")){
#   datapath<-ecampa3res
# }
# if(scope%in%c("feed_marketbal","activities")){
#   dataparm<-"DATA2"
# }else if(grepl("tseries_marketbal|nbalance",scope)){
#   dataparm<-"DATA"
# }else if(scope%in%c("nlca")){
#   dataparm<-"DATAOUT"
# }    
if(scope%in%c("feed_marketbal","activities")){
  ydim<-"Y"
  curyears<-c("08","12")
}else if(grepl("tseries_marketbal|nbalance",scope)){
  ydim<-c(1990:2014)
  curyears<-""
}else if(scope%in%c("nlca")){
  ydim<-"2030"
  curyears<-"2030"
  datanames<-c("RALL","EMPTY","COLS","ROWS","Y","VALUE")
  
}  


setfile<-paste0(getwd(),"/LAPMcapdis_END.gdx")
setfile<-"x:\\adrian\\models\\capri\\dndc\\results\\20110722\\nitrogen\\nitrogenlca_sets.gdx"
#setfile<-paste0(ecampa3res,"capdis\\CAPREGcapdis_END.gdx")
#setfile<-paste0(ecampa3res,"capdis\\LAPMcapdis_END.gdx")
setfile <- paste0(cenv$capri, cenv$leipadr, cenv$resdir, "sets/dump_epnf_scenarios_391.gdx")

if(scope%in%c("nlca")){
  setfilen<-paste0(ecampa3res,"sets/sets_nitrogen.gdx")
}   

rows<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "ROWS")
# set FROWS / set.fco,comi,comf,beef,pork,sgmi,sgmf,sgmt,eggs,poum,oani/;
# set cropo / set.fco,set.ico/;
# set animo_rows / die oben + set.oyani_rows,mann,manp,mank,lres/;
frows<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FROWS")
ico<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "ICO")
mpactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MPACT")
mpact<-as.character(mpactexp[,1])
names(mpactexp)<-c("Acronym","Description")
mcactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "mcact")
mcact<-as.character(mcactexp[,1])
names(mcactexp)<-c("Acronym","Description")
mcactnohighyild<-setdiff(mcact, c("NURS", "FLOW", "TOMA"))


maactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MAACT")
maact<-as.character(maactexp[,1])
daactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "DAACT")
daact<-as.character(daactexp[,1])

#fssactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "fssact")
#fssact<-as.character(fssactexp[,1])
# lapmactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "lapmact")
# lapmact<-as.character(lapmactexp[,1])
# lapmact_fssactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "lapmact_fssact")
# lapmact_fssact<-as.character(lapmact_fssactexp[,1])

# oseco contains also non-food items - restict to
cropexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FCO")
icoexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "ICO")
animoexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "ANIMO_ROWS")
oani<-rgdx.set(setfile,ts = TRUE,symName = "OYANI_ROWS")
animoexp <- animoexp[! animoexp$i %in% oani$i,]
animoexp <- animoexp[! animoexp$i %in% c("MANN", "MANK", "MANP"),]
secoexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "SECO_ROWS")

oaggexp <- rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "OAGG")
oagg <- as.character(oaggexp[, 1])

osecoexp<-rbind(cropexp, icoexp, animoexp, secoexp)
oseco<-as.character(osecoexp[,1])


names(maactexp)<-c("Acronym","Description")
names(daactexp)<-c("Acronym","Description")
feed_rowsexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FEED_ROWS")
feed_rows<-as.character(feed_rowsexp[,1])
fert_distexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "fert_dist")
fert_dist<-as.character(fert_distexp[,1])
feed_to_o<-(rgdx.set(setfile,ts = TRUE,symName = "FEED_TO_O"))
frmbal_cols<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "frmbal_cols"))
mrkbal_cols<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MRKBAL_COLS"))
ncnc_posexp<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NCNC_POS"))
ncnc_pos<-as.character(ncnc_posexp[,1])
nbil<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NBIL"))
# Countries and regions
nuts0_exp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NUTS0")
nuts0<-as.character(nuts0_exp[,1])
nuts0eu15<-as.character(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NUTS0_EU15")[,1])
nuts0eu10<-as.character(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NUTS0_EU10")[,1])
cntr<-substr(nuts0,1,2)

srnuts2names<-as.data.table(rgdx.set(setfile,te = TRUE, ts = TRUE,symName = "SRNUTS2"))
srnuts2<-as.character(rgdx.set(setfile,ts = TRUE,symName = "SRNUTS2")[,1])
nuts2<-substr(srnuts2,1,4)
rall<-rgdx.set(setfile,ts = TRUE,te=TRUE,symName = "RALL")
uaar<-as.data.frame(t(as.matrix(c("UAAR","Total agricultural activities"))))
names(uaar)<-c("Acronym","Description")

nbil_exp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NBIL_COLS_MAIN")
nbil<-as.character(nbil_exp[,1])

# Nitrogen and GHG relevant sets
if(exists("setfilen")){
  soilbalpos_exp<-rgdx.set(setfilen,te=TRUE,ts = TRUE,symName = "SOILBALPOS_R")
  soilbalpos<-as.character(soilbalpos_exp[,1])
  nflowsr_exp<-rgdx.set(setfilen,te=TRUE,ts = TRUE,symName = "nflowsr")
  nflowsr<-as.character(nflowsr_exp[,1])
  nemiscadd_exp<-rgdx.set(setfilen,te=TRUE,ts = TRUE,symName = "Nemiscadd")
  nemiscadd<-as.character(nemiscadd_exp[,1])
}
mbal<-c("GROF", "HCOM", "FEDM", "IMPT", "EXPT")

meta2keep<-c("DATE OF VERSION","NAME OF PROCESSOR ORGANISATION","User","Regional breakdown")

if(scope%in%c("feed_marketbal","tseries_marketbal","activities")){
  regi<-c(nuts0,srnuts2)
}else if(grepl("nbalance",scope) || scope %in% c("nlca")){
  regi<-srnuts2
}

gethsumap<-function(){
  mhsu<-as.data.table(rgdx.set(setfile,te=FALSE,ts = TRUE,symName = "m_srnuts2_hsu"))
  names(mhsu) <- c("RALL", "HSU")
  return(mhsu)
}
