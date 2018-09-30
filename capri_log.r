if(format(Sys.time(), "%Y%m%d")=="20180924"){

  # Request 
#   From: Payen, Sandra [Sandra.Payen@agresearch.co.nz]
#   Sent: 26 September 2018 06:08
#   To: LEIP Adrian (JRC-ISPRA)
#   Subject: RE: E-mail re Eutrophication and beef
#   Hi Adrian,
#   Thanks a lot for your reply and for offering to send me data.
#   Work is pretty hectic at the moment. but I am keen to make an attempt at doing these calculations J. It would be nice to have P emissions since they are the one contributing to freshwater eutrophication (from a LCA perspective).
#   If you send me what you have, I can give it a try?
#   Best regards,
#   Sandra
#   Sandra Payen
#   http://g10.agresearch.co.nz/support/communications/branding/Logos/AgResearch%20CMYK.jpg
#   Post-Doctoral Scientist
#   Farm Systems & Environment
#   T  +64 7 838 5047   
#   
#   From: Adrian.LEIP@ec.europa.eu [mailto:Adrian.LEIP@ec.europa.eu] 
#   Sent: Tuesday, 25 September 2018 12:31 AM
#   To: Payen, Sandra <Sandra.Payen@agresearch.co.nz>
#     Subject: RE: E-mail re Eutrophication and beef
#   hi sandra,
#   indeed, i urgently need a small project to work on P!!
#     in CAPRI, P input, retention and removals with crop residues are calculated (generic P coefficients), but they are not taken up in the LCA. for animals, P excretion is a static coefficient and not dynamic as for N. would be relatively easy to imrpove ... with a bit of time;)
# so, what i can offer you is P coefficients in crops and some information on the feed intake by animal type, animal production data - so you could make a back-on-the-envelope LCA to capture P flows.
# bw - adrian.

# Using data extracted already for Michaela (feed_marketbalance4michaela_20180822)
# Adding PRET, PMIN, PCRD, PMAN
  
  source("capri_packages.r")
  source("capri_dirs.r")
  scope<-"baseyearpmin"
  source("capri_sets.r")
  source("capriextract_functions.r")
  curyears<-c("08","12")
  curcols<-mcact
  currows<-c("PMIN", "PMAN", "PRET", "PCRD")
  currows<-c("YILD")
  alldatasets<-getmultipleyears(scope,cntr,curyears)
  capridat<-alldatasets[[1]]
  caprimeta<-alldatasets[[2]]
  source("capri_writedata.r")
  
  
  
    
}

# September 2018 - Check N balance in the N-LCA
# Request Aimable (email 201809)
#From: Uwizeye, Aimable (AGAL) [Aimable.Uwizeye@fao.org]
#Sent: 20 September 2018 09:58
#To: LEIP Adrian (JRC-ISPRA)
#Subject: Fertilizer data by crop by country in EU
#Dear Adrian,
#I am completing the Global N assessment in livestock analysis. I found that the country average data from FAOSTAT are not crop-specific, and they resulted in high values for fertilizer consumption. I am wondering if you have the fertilizer data for EU countries for 2010 to share with us. I contacted Fertilizer Europe for these data, but I did not yet get any response from them.
#This is the only step I want to improve in the global assessment before I share the outcomes with you for discussion and finalization of the manuscript.
#Many thanks and kind regards,
#Aimable
if(format(Sys.time(), "%Y%m%d")=="20180924"){
  source("capri_packages.r")
  source("capri_dirs.r")
  scope<-"baseyearnmin"
  source("capri_sets.r")
  source("capriextract_functions.r")
  curyears<-c("08","12")
  curcols<-mcact
  currows<-"NMIN"
  alldatasets<-getmultipleyears(scope,cntr,curyears)
  capridat<-alldatasets[[1]]
  caprimeta<-alldatasets[[2]]
  source("capri_writedata.r")
}


if(format(Sys.time(), "%Y%m%d")=="20180924"){
  source("capri_packages.r")
  scope<-"nlca"
  source("capri_dirs.r")
  source("capri_sets.r")
  source("capriextract_functions.r")
  
  curcountry<-"AT110000"
  curscen<-"envind/nflows_gases2030.gdx"
  curcols<-mpact
  currows<-c(nemiscadd,nemiscalc,"LEACHI", "DENITR", "N2OOTH") #add leach, denit because forgotten now added
  curdim5<-""
  datanames<-c("RALL","COLS","EMPTY","ROWS","Y","VALUE")
  
  
  gdxfil<-paste0(ecampa3res,curscen)
  emiscalc_all<-as.data.table(rgdx.param(gdxfil,"emiscalc"))
  names(emiscalc_all)<-datanames

  for(crop in mcact){checkCropNbudget(x = emiscalc_all, crop = crop)}
  
}
if(format(Sys.time(), "%Y%m%d")=="20180923"){
    
  # Addition of 'all' (?) positions needed for the N-budget by crop
  source("capri_packages.r")
  scope<-"nlca"
  source("capri_dirs.r")
  source("capri_sets.r")
  source("capriextract_functions.r")
  
  curcountry<-"AT110000"
  curscen<-"envind/nflows_gases2030.gdx"
  curcols<-mpact
  currows<-c(nemiscadd,nemiscalc,"LEACHI", "DENITR", "N2OOTH") #add leach, denit because forgotten now added
  curdim5<-""
  datanames<-c("RALL","COLS","EMPTY","ROWS","Y","VALUE")
  
  
  gdxfil<-paste0(ecampa3res,curscen)
  emiscalc_all<-as.data.table(rgdx.param(gdxfil,"emiscalc"))
  names(emiscalc_all)<-datanames
  emiscalc_swhe<-selectrowscolsregi(reload=0, capridat=emiscalc_all, 
                               cols="SWHE" ,rows=currows, curdim5 = "T1",
                               regi="AT110000", ydim = "2030")
  x<-emiscalc_swhe[,c("ROWS","VALUE")]
  x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]<-28/44 * x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]
  checkNbudget(x)
  
  checkaggvsdet(x,"SURSOI",c("LEACHI", "DENITR"))
  checkaggvsdet(x,"N2OOTH",c("N2ODEP", "N2OCRO", "N2OHIS"),0)
  checkaggvsdet(x,c("SURTOT", "EXPPRD"),c("IMPORT", "N2OOTH"))
  checkaggvsdet(x,"SURTOT",c("SURSOI", "N2OOTH",
                             "GASMAN","GASAPP", "GASGRA", "GASMIN", 
                             "RUNMAN","RUNAPP", "RUNGRA", "RUNMIN"))
  checkaggvsdet(x, "IMPORT", c("ATMOSD", "CRESID", "BIOFIX", "MINSAT", "MINFER", "EXCRET"))
  checkaggvsdet(x, "EXCRET", c("MANAPP", "MANGRA", "GASMAN", "RUNMAN"))
  checkaggvsdet(x, "MANGRA", c("NMANGR", "GASGRA", "RUNGRA"))
  checkaggvsdet(x, "MANAPP", c("NMANAP", "GASAPP", "RUNAPP"))
  checkaggvsdet(x, "NMAN", c("NMANAP", "NMANGR"))
  
  
}
if(format(Sys.time(), "%Y%m%d")=="20180921"){
  
  scope<-"nlca"
  curcountry<-"AT110000"
  curscen<-"epnf_refpol_endotechall_MAgPIE_onlyemptyCUR"
  curcols<-mpact
  currows<-c(soilbalpos,"IMPORT")
  curdim5<-""
  
  capridat<-opendata(scope = scope, curcountry = curcountry, curyear = curyear)
  capricur<-selectrowscolsregi(reload=0, capridat=capridat, cols=curcols,
                               rows=currows, regi=curcountry, curyear=curyear)
  capripiv<-dcast(capricur, RALL + COLS ~ ROWS, value.var = "VALUE")
  write.csv(capripivot, 
             file=paste0(curscen,"_",curcountry,".",format(Sys.time(), "%Y%m%d"),".csv"),
             quote = FALSE, row.names = FALSE)
  
  capricur<-selectrowscolsregi(cols=currows, rows="NITF", reload=0, capridat=capridat, regi=curcountry, curyear=curyear)
  x<-capricur[,c("COLS","VALUE")]
  checkNbudget(x)

  gdxfil<-paste0(ecampa3res,"sets/p_nflow_after_calc_nbalanceSIMY.gms.gdx")
  pnflow<-as.data.table(rgdx.param(gdxfil,"p_nflows"))
  names(pnflow)<-data4dim
  pcur<-selectrowscolsregi(cols="NITF", rows=NULL, reload=0, capridat=pnflow, regi=curcountry, curyear="2030")
  x<-pcur[,c("ROWS","VALUE")]
  checkNbudget(x)
  
  gdxfil<-paste0(ecampa3res,"capmod/res_2_0830epnf_refpol_endotechall_MAgPIE_onlyemptyCUR.gdx")
  shwebal<-as.data.table(rgdx.param(gdxfil,"dataout"))
  names(shwebal)<-data5dim
  pcur<-selectrowscolsregi(cols="SWHE", rows=currows, curdim5 = "", reload=0, capridat=shwebal, regi=curcountry, curyear="2030")
  x<-pcur[,c("ROWS","VALUE")]
  checkNbudget(x)
  
  #Some duplicate colnames (e.g. NH3HOU)??
  #capricurt<-selectrowscolsregi(cols="NH3HOU", rows=NULL, curdim5=="", reload=0, capridat=capridat, regi=curcountry, curyear=curyear)
  #Attention because 5th dimension contains NITF with 'perha' .. function corrected
}
if(format(Sys.time(), "%Y%m%d")=="20180920"){
  
  scope<-"nlca"
  curcountry<-"AT110000"
  curscen<-"epnf_refpol_endotechall_MAgPIE_onlyemptyCUR"
  curcols<-mpact
  currows<-soilbalpos
  curdim5<-""
  
  capridat<-selectrowscolsregi(reload=1, capridat=NULL, cols=curcols,
                               rows=currows, regi=curcountry, curyear=curyear)
  capripivot<-dcast(capridat, RALL + COLS ~ ROWS, value.var = "VALUE")
  
}