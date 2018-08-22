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
    if(scope%in%c("feed_marketbal","activities")){
        datafile<-paste0("res_",curyear)
        datafile<-paste0(datafile,curcountry,".gdx")
        datafile<-paste0(datapath,datafile)
    }
    if(grepl("nbalance|tseries_marketbal",scope)){
        datafile<-paste0(curcountry,"_",curyear,".gdx")
        datafile<-paste0(datapath,datafile)
    }
    caprimeta<-rgdx.set(datafile,"META",ts=TRUE,te=TRUE)
    
    
    
}


selectrowscolsregi<-function(capridat,cols,rows,regi,d,curyear){
    capridat<-rgdx.param(d[[1]],d[[2]])
    names(capridat)<-d[[3]]
    
    #COLS (activities, variables for products)
    capridat<-capridat[capridat$COLS%in%cols,]
    
    #ROWS (products, variables for activities)
    capridat<-capridat[capridat$ROWS%in%rows,]
    
    #Filter regional level 
    capridat<-capridat[capridat$RALL%in%regi,]
    
    #Filter time dimension
    capridat<-capridat[capridat$Y%in%as.character(d[[4]]),]
    if(curyear!=""){
        capridat$Y<-paste0("20",curyear)
    }
    #capridat<-capridat[,setdiff(names(capridat),c("Y"))]
    
    return(capridat)
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

getdata<-function(scope,curcountry,curyear){
    if(scope%in%c("feed_marketbal","activities")){
        datafile<-paste0("res_",curyear)
        datafile<-paste0(datafile,curcountry,".gdx")
        datafile<-paste0(datapath,datafile)
    }
    if(grepl("nbalance|tseries_marketbal",scope)){
        datafile<-paste0(curcountry,"_12.gdx")
        datafile<-paste0(datapath,datafile)
    }
    if(file.exists(datafile)){
        print(datafile)
        d<-list(datafile,dataparm,datanames,ydim)
        capridat<-rgdx.param(datafile,dataparm)
        names(capridat)<-datanames
        caprimeta<-rgdx.set(datafile,te=TRUE,ts = TRUE,symName = "meta")
        
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
        return(capridat)
    }
}

getmultipleyears<-function(scope,cntr,curyears){
    if(curyears[1]==""){
        capridat<-Reduce(rbind,lapply(cntr,function(x) getdata(scope,x)))
        caprimeta<-Reduce(rbind,lapply(cntr,function(x) getmeta(x)))
    }else{
        capridat<-Reduce(rbind,
                         lapply(curyears,function(y) 
                             Reduce(rbind,
                                    lapply(cntr,function(x) getdata(scope,x,y)))
                         ))
        caprimeta<-Reduce(rbind,
                         lapply(curyears,function(y) 
                             Reduce(rbind,
                                    lapply(cntr,function(x) getmeta(x,y)))
                         ))
    }
    return(list(capridat,caprimeta))
}
    
    
    

