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
  if(file.exists(datafile)){
    cat("\n ",datafile)
    d<-list(datafile,dataparm,datanames,ydim)
    capridat<-rgdx.param(datafile,dataparm)
    names(capridat)<-datanames
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
  
checkaggvsdet<-function(x, aggs, dets, listdetails=0){
  
  # Compares N budget aggregates vs sum of detailed positions
  # x is a data frame or data table with two columns.
  #   column 1 should contain the element names and column 2 the values
  x<-as.data.table(x)
  names(x)<- c("x","y")
  miss<-""
  fail<-0
  
  if(sum(x$x%in%aggs)==0){
    cat("\n There is no ",aggs," in the data set!")
    fail<-1
  }else{
    if(sum(x$x%in%dets)==0){   
      cat("\n There is none of ",dets," in the data set!")
      fail<-1
    }else{
      misaggs<-paste(aggs[!which(aggs%in%x$x)], collapse="+")
      misdets<-paste(dets[!which(dets%in%x$x)], collapse="+")
      sumaggs<-round(sum(x$y[x$x%in%aggs]),5)
      sumdets<-round(sum(x$y[x$x%in%dets]),5)
      if(sumaggs==sumdets){
        fail=0
      }else{
        fail<-2
      }
    }
  }
  if(fail==0) {
    cat("\n ", paste(aggs,collapse="+"), " = ",paste(dets,collapse="+") ," = ", sumdets)
    if(misaggs!="") cat("\n Missing elements: ",misaggs)
    if(misdets!="") cat("\n Missing elements: ",misdets)
  }

  if(fail==1) cat("\n Cannot carry out check!", aggs, " = SUM(",paste(dets,collapse=", ") ,")")
  if(fail==1) cat("\n Cannot carry out check!", aggs, " = SUM(",paste(dets,collapse=", ") ,")")
  if(fail==2) {
    cat("\n !! Mismach: \n ", 
                  "\n ", paste(aggs,collapse="+") ," = ", sumaggs,
                  "\n ", paste(dets,collapse="+") ," = ", sumdets,
                  "\n Difference = ", sumaggs-sumdets)
    if(sumaggs>sumdets){ #List values of aggs
      for(z in aggs){cat("\n ", z, " = ", x$y[x$x==z])}
    }else{
      for(z in dets){cat("\n ", z, " = ", x$y[x$x==z])}
    }
  }
  if(listdetails==1){
    for(z in c(aggs,dets)){cat("\n ", z, " = ", x$y[x$x==z])}
  }
}

checkCropNbudget<-function(x, crop){
  
  cat("\n Check N-budget for ", crop)
  emiscalc_crop<-selectrowscolsregi(reload=0, 
                                    capridat=x, 
                                    cols=crop,
                                    rows=currows, 
                                    curdim5 = "T1",
                                    regi=curcountry, 
                                    ydim = "2030")

  x<-emiscalc_swhe[,c("ROWS","VALUE")]
  x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]<-28/44 * x$VALUE[grepl("N2O",x$ROWS) & !grepl("N2ON",x$ROWS)]
  
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
    

