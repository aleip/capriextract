source("xobsfunctions.r")

opendata<-function(scope,curcountry,curyear){
  if(scope%in%c("feed_marketbal","activities") | grepl("baseyear",scope)){
    datafile<-paste0("res_",curyear)
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,"capreg/",datafile)
    dataparm<-"DATA2"
    ydim<-"Y"
  }
  if(grepl("nbalance|tseries",scope)){
    datafile<-paste0(curcountry,"_12.gdx")
    datafile<-paste0(datapath,datafile)
  }
  if(grepl("nlca",scope)){
    datafile<-paste0("capmod/res_2_0830",curscen,".gdx")
    datafile<-paste0(datapath,datafile)
  }
  if(grepl("lapm", scope)){
    datafile<-paste0(cgams, "../dat/capdishsu/fssdata/")
    if(curyear=="_") curyear<-""
    datafile<-paste0(datafile, "capdis_", curcountry, "_10GRID", curyear, ".gdx")
    dataparm<-"p_capdis"
    ydim<-""
    datanames<-c("RALL", "ROWS", "COLS", "VALUE")
    
    if(curyear=="preds"){
      datafile<-paste0(cgams, "../dat/capdishsu/lapm/")
      datafile<-paste0(datafile, substr(curcountry, 1, 2), "_lapmpreds.gdx")
      dataparm<-"lapmpreds"
      ydim<-""
      datanames<-c("RALL", "COLS", "VALUE")
    }
  }
  if(grepl("capdis",scope)){
    datafile<-paste0("capdis/xobs_2_",curcountry,"_",baseyear)
    if(scope=="capdiscapreg") datafile<-paste0(datafile,baseyear)
    if(scope=="capdistimes") datafile<-paste0(datafile,curyear)
    datafile<-paste0(datapath,datafile,".gdx")
    dataparm<-"xobs"
    ydim<-""
    datanames<-data3dim
  }
  if(file.exists(datafile)){
    cat("\n ",datafile)
    d<-list(datafile,dataparm,datanames,ydim)
    capridat<-rgdx.param(datafile,dataparm)
    names(capridat)<-datanames
    if(grepl("lapm", scope)){
      capridat$Y<-gsub("_","",curyear)
      if(curyear=="")capridat$Y<-"capdis"
      capridat$NUTS2<-curcountry
      if(curyear=="preds"){
        capridat$ROWS<-"LEVL"
      }
      capridat<-select(capridat, c("NUTS2", data4dim))
    }
  }
  
  return(capridat)
}

selectrowscolsregi<-function(reload=0, capridat=capridat, cols=curcols, 
                             rows=currows, ydim="Y", curdim5=NULL,regi, 
                             curcountry, curyear="08"){
  
  if(reload==1){
    capridat<-opendata(scope,curcountry,curyear)
  }
  
  #COLS (activities, variables for products)
  if(!is.null(cols)) capridat<-capridat[capridat$COLS%in%cols,]
  
  #ROWS (products, variables for activities)
  if(!is.null(rows)) capridat<-capridat[capridat$ROWS%in%rows,]
  
  #Filter regional level 
  capridat<-capridat[capridat$RALL%in%regi,]
  
  #Filter time dimension
  capridat<-capridat[capridat$Y%in%as.character(ydim),]
  if(curyear!=""){
    if(!grepl("^20",curyear)){curyear<-paste0("20",curyear)}
    capridat$Y<-curyear
  }
  #capridat<-capridat[,setdiff(names(capridat),c("Y"))]
  if(!is.null(curdim5)){
    print("select curdim5")
    capridat<-capridat[capridat$EMPTY%in%curdim5,]
  }
  
  return(capridat)
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

getmeta<-function(curcountry,curyear){
  if(scope%in%c("feed_marketbal","activities") | grepl("baseyear",scope)){
    datafile<-paste0("res_",curyear)
    datafile<-paste0(datafile,curcountry,".gdx")
    datafile<-paste0(datapath,"capreg/",datafile)
  }
  if(grepl("nbalance|tseries_marketbal",scope)){
    datafile<-paste0(curcountry,"_",curyear,".gdx")
    datafile<-paste0(datapath,datafile)
  }
  caprimeta<-rgdx.set(datafile,"META",ts=TRUE,te=TRUE)
}


getmarketbalance<-function(capridat,d,curyear){
    rows<-c(as.character(frows$i),as.character(ico$i))
    cols<-c(as.character(frmbal_cols$i),as.character(mrkbal_cols$i))
    cols<-c(as.character(frmbal_cols$i),"FEDM")
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d,curyear)
    return(capridat)
}

getfeed<-function(capridat,d,curyear){
    rows<-feed_rows
    cols<-c(maact,daact)
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d,curyear)
}
getfeed<-function(capridat,d,curyear){
    rows<-feed_rows
    cols<-c(maact,daact)
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d,curyear)
}
getmpact<-function(capridat,d,curyear){
    rows<-"LEVL"
    cols<-c(mpact)
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d,curyear)
}
getuaarlevl<-function(capridat,d){
    cols<-"UAAR"
    rows<-"LEVL"
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d)
}
getnbil<-function(capridat,d){
    cols<-c(as.character(nbil$i))
    if(scope=="nbalancemain")cols<-c("MINFER","EXCRET","ATMOSD","BIOFIX","CRESID",
                                     "SURTOT","EXPPRD","SURSOI","GASTOT","RUNTOT")
    rows<-"NITF"
    capridat<-selectrowscolsregi(capridat,cols,rows,regi,d)
}

getdata<-function(scope,curcountry,curyear,curcols,currows=currows){
        
  capridat<-opendata(scope, curcountry, curyear)
  
  if(scope=="feed_marketbal"){
    capridat<-getmarketbalance(capridat,d,curyear)
    capridat<-rbind(capridat,getfeed(capridat,d,curyear))
  }
  if(scope=="activities"){
    capridat<-getmpact(capridat,d,curyear)
  }
  if(scope=="tseries_marketbal"){
    capridat<-getmarketbalance(capridat,d)
  }
  if(grepl("nbalance",scope)){
    capridat<-getnbil(capridat,d)
    capridat<-rbind(capridat,getuaarlevl(capridat,d))
  }
  if(grepl("nlca",scope)){
    capridat<-selectrowscolsregi(capridat,curcols,currows,regi,d,curyear)
  }
  if(scope%in%c("baseyearnmin","baseyearpmin")){
    capridat<-selectrowscolsregi(capridat,reload = 0,
                                 cols = mcact,rows = currows, ydim = "Y", 
                                 regi = srnuts2, curdim5 = NULL,curyear = curyear)
  }
  return(capridat)
}


getmultipleyears<-function(scope,cntr,curyears){
  

  if(curyears[1]==""){
    capridat<-Reduce(rbind,lapply(cntr,function(x) getdata(scope,x)))
    caprimeta<-Reduce(rbind,lapply(cntr,function(x) getmeta(x)))
  }else{
    cat("\n Retrieve data")
    capridat<-Reduce(rbind,
                     lapply(curyears,function(y) 
                       Reduce(rbind,
                              lapply(cntr,function(x) getdata(scope,curcountry = x,curyear = y,currows = currows)))
                     ))
    cat("\n Retrieve meta")
    caprimeta<-Reduce(rbind,
                      lapply(curyears,function(y) 
                        Reduce(rbind,
                               lapply(cntr,function(x) getmeta(curcountry = x,curyear = y)))
                      ))
  }
  return(list(capridat,caprimeta))
}
  
checkaggvsdet<-function(x, aggs, dets, listdetails="error"){
  
  # Compares N budget aggregates vs sum of detailed positions
  # x is a data frame or data table with two columns.
  #   column 1 should contain the element names and column 2 the values
  
  # ------- listdetails --------
  # all = all output given
  # error = only errors (missing elements and mismatch)
  # mismatch = only mismatches
  
  x<-as.data.table(x)
  names(x)<- c("x","y")
  miss<-""
  fail0<-0
  fail<-0
  
  if(sum(x$x%in%aggs)==0){    fail0<-1  }
  if(sum(x$x%in%dets)==0){   
    if(fail0 == 1) {  # There is no left nor right hand side ==> value==0     
      sumaggs<-0
      sumdets<-0
      misaggs<-""
      misdets<-""
    } else { fail <- 1 }            # Detailed flows are missing
  }else{
    if( fail0 == 1) {fail <- 2      # Aggregated flow is missing
    }else{ # Both detailed and aggregated flows available
      misaggs<-paste(aggs[!which(aggs%in%x$x)], collapse="+")
      misdets<-paste(dets[!which(dets%in%x$x)], collapse="+")
      sumaggs<-round(sum(x$y[x$x%in%aggs]),5)
      sumdets<-round(sum(x$y[x$x%in%dets]),5)
      if(sumaggs!=sumdets){fail<-3}  # Mismach between aggregated flow and sum of detailed flows
    }
  }

  if(fail==0){
    if(listdetails == "all") {
    cat("\n ", crop, ": ", paste(aggs,collapse="+"), " = ",paste(dets,collapse="+") ," = ", sumdets)
    if(misaggs!="") cat("\n Missing elements: ",misaggs)
    if(misdets!="") cat("\n Missing elements: ",misdets)
    }else if(listdetails != "mismatch"){
      cat(" .. OK")
    }
  }
  if(listdetails != "mismatch"){
    if(fail == 1 ) cat("\n ", crop, ": There is no ",dets," in the data set!")
    if(fail == 2 ) cat("\n ", crop, ": There is no ",aggs," in the data set!")
    if(fail == 1 | fail== 2) cat("\n ", crop, ": Cannot carry out check!", aggs, " = SUM(",paste(dets,collapse=", ") ,")")
  }
  if(fail == 3) {
    cat("\n ", crop, ": Mismach!! ", paste(aggs,collapse="+"), " = ",paste(dets,collapse="+"))
    cat("\n ", paste(aggs,collapse="+") ," = ", sumaggs,
        "; ", paste(dets,collapse="+") ," = ", sumdets,
        ";  Difference = ", sumaggs-sumdets)
    if(length(aggs)>1){cat("\n ");for(z in aggs){cat(z, " = ", x$y[x$x==z])}}
    if(length(aggs)>1 & length(dets)>1){cat("; ")}
    if(length(dets)>1){cat("\n");for(z in dets){cat("  ",z, " = ", x$y[x$x==z])}}
    # if(sumaggs>sumdets){ #List values of aggs
    #   for(z in aggs){cat("\n ", z, " = ", x$y[x$x==z])}
    # }else{
    #   for(z in dets){cat("\n ", z, " = ", x$y[x$x==z])}
    # }
  }
  if(listdetails=="details"){
    for(z in c(aggs,dets)){cat("\n ", z, " = ", x$y[x$x==z])}
  }
}

checkCropNbudget<-function(x, crop, output="error"){
  
  if(output != "mismatch"){
    cat("\n Check N-budget for ", crop)
  }
  nflows_crop<-selectrowscolsregi(reload=0, 
                                  capridat=x, 
                                  cols=crop,
                                  rows=currows, 
                                  curdim5 = NULL,
                                  regi=curcountry, 
                                  ydim = "2030"
  )
  

  capridat<-nflows_crop[,c("ROWS","VALUE")]
  #x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]<-28/44 * x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]
  
  checkaggvsdet(capridat, "SURSOI",c("LEACHI", "DENITR"), output)
  checkaggvsdet(capridat, "N2ONOTH",c("N2ONDEP", "N2ONCRO", "N2ONHIS"), output)
  checkaggvsdet(capridat, c("SURTOT", "EXPPRD"),c("IMPORT", "N2ONOTH"), output)
  checkaggvsdet(capridat, "SURTOT",c("SURSOI", "N2ONOTH",
                             "GASMAN","GASAPP", "GASGRA", "GASMIN", 
                             "RUNMAN","RUNAPP", "RUNGRA", "RUNMIN"), output)
  checkaggvsdet(capridat, "IMPORT", c("ATMOSD", "CRESID", "BIOFIX", "MINSAT", "MINFER", "EXCRET"), output)
  checkaggvsdet(capridat, "EXCRET", c("MANAPP", "MANGRA", "GASMAN", "RUNMAN"), output)
  checkaggvsdet(capridat, "MANGRA", c("NMANGR", "GASGRA", "RUNGRA"), output)
  checkaggvsdet(capridat, "MANAPP", c("NMANAP", "GASAPP", "RUNAPP"), output)
  checkaggvsdet(capridat, "NMAN", c("NMANAP", "NMANGR"), output)
  
}







