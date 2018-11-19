source("xobsfunctions.r")

#' Open a Capri data file
#' @description This function opens a Capri gdx data file also called a xobs file. 
#' Depending on the scope parameter, conditional statements change the bahaviour of the opendata function. 
#' @param scope character variable used to adapt the behaviour of the function to different input file formats and locations.
#' @param curcountry character iso2 country code
#' @param curyear numeric year
#' @return data frame
#' @export
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






mapping <- function(scope = "capdiscapreg", curyears = "12", baseyear = "12", 
                    curcountries = NULL, crop = NULL, n_cuts = 6, variab = NULL,
                    hsu_dir = "\\\\ies\\d5\\agrienv\\Data\\HSU",
                    n23_dir = "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"){
  
  
  #--- reading CAPRI data for plotting
  
  capridat<-Reduce(rbind, lapply(1:length(curyears), function(x)
    Reduce(rbind, lapply(1:length(curcountries), function(y)
      opendata(scope = scope,curcountry = curcountries[y], curyear = curyears[x]))
    )
  )
  )
  print(" ")
  
  capridat <- capridat[grepl("^U", capridat$RALL), ]
  capridat <- capridat[capridat$ROWS %in% variab, ]
  head(capridat)
  
  capridat <- dcast(capridat, RALL + ROWS ~ COLS, drop = TRUE, value.var = "VALUE")
  if (length(unique(capridat$ROWS)) == 1) capridat <- capridat[, !names(capridat) %in% "ROWS"]
  head(capridat)
  
  #
  
  #--- reading NUTS3 and NUTS2 data
  
  #n23_dir <- "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"
  if(!exists("nuts23")) nuts23 <- readOGR(dsn = n23_dir, layer = "GISCO_NUTS2_3_2010_with_attr_laea")
  #ogrInfo(dsn = n23_dir)
  #summary(nuts23)
  #nuts23@proj4string
  #head(nuts23)
  #plot(nuts23)
  
  #--- reading HSU shapefile
  
  #hsu_dir <- "\\\\ies\\d5\\agrienv\\Data\\HSU"
  if(!exists("hsu")) hsu <- readOGR(dsn = hsu_dir, layer = "hsu_eu28_CH_NO_EXYUG")
  #hsu$CAPRI_HSU <- as.numeric(gsub("U", "", hsu$CAPRI_HSU))
  #head(hsu)
  #ogrInfo(dsn = hsu_dir)
  #hsu@proj4string
  
  
  # merging predictions to HSU shapefile
  preds_hsu <- merge(hsu, capridat, by.x = "CAPRI_HSU", by.y = "RALL", all.x = FALSE)
  #summary(preds_hsu)
  
  
  preds_hsu@data$EEZ_R <- as.character(preds_hsu@data$EEZ_R)
  #apply(preds_hsu@data, 2, function(x) sum(is.na(x)))
  preds_hsu@data$nuts2 <- as.vector(substr(unique(preds_hsu$EEZ_R), 1, 4))
  
  regs <- as.vector(unique(preds_hsu@data$nuts2))
  n_regs <- length(regs)
  
  sel_cols <- !names(preds_hsu) %in% c("CAPRI_HSU", "OBJECTID", "codes", "x", "y", "EEZ_R", "CNTR_ENGL", "AREA", "AREAcon", "UAAR", "nuts2")
  
  crps <- names(preds_hsu)[sel_cols]
  no_crps <- names(preds_hsu)[!sel_cols]
  
  if (!is.null(crop)){
    crps <- crps[crps %in% crop]
    preds_hsu@data <- preds_hsu@data[, c(no_crps, crps)]
  }
  
  length(crps)
  sel_cols_n3 <- !names(preds_hsu) %in% c("CAPRI_HSU", "OBJECTID", "codes", "x", "y", "EEZ_R", "CNTR_ENGL", "AREA", "AREAcon", "UAAR")
  
  
  # Find which categories should be plotted in a different scale because values are to big
  # Two different scales will be used (if necessary), and two different legends generated
  rnge_crps <- apply(preds_hsu@data[crps], 2, range, na.rm = T)
  #apply(preds_hsu@data[crps], 2, stats::quantile, na.rm = T)
  rnge_crps_mean <- apply(rnge_crps, 1, mean)
  rnge_crps_sd <- apply(rnge_crps, 1, sd)
  
  crps_over_sd <- names(which(rnge_crps[2,] > rnge_crps_sd[2] * 1.5))
  if(all(crps %in% crps_over_sd)) crps_over_sd <- NULL
  
  sum_nas <- function(x) {  #to check if every column (crop, etc.) is kept for plotting, for each nuts2
    sm_nas <- sum(is.na(x))
    if(sm_nas == length(x)){ a <- 0 }else{ a <- 1 }
    return(a)
  }
  
  categs2plot <- as.data.frame(as.data.frame(preds_hsu@data[, sel_cols_n3]) %>% group_by(nuts2) %>% summarise_all(funs(sum_nas)))
  categs2plot <- sum(categs2plot[, -1])
  
  if (length(crps_over_sd) != 0){ 
    n_legs <- 2
  }else{
    n_legs <- 1
  }
  n_plots <- categs2plot + n_legs
  
  rw <- floor(sqrt(n_plots))
  cl <- ceiling(n_plots / floor(sqrt(n_plots)))
  
  tot_pnls <- cl * rw
  free_pnls <- tot_pnls - categs2plot
  pnls4leg <- floor(free_pnls / 2) * 2
  
  
  if (length(crps_over_sd) != 0){
    
    cuts_1 <- stats::quantile(preds_hsu@data[crps_over_sd], probs = seq(0, 1, 1/n_cuts), na.rm = T)
    lev_1 <- levels(cut(as.numeric(unlist(preds_hsu@data[crps_over_sd])), cuts_1))
    
    crps_0 <- crps[!crps %in% crps_over_sd]
    cuts <- stats::quantile(preds_hsu@data[crps_0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
    lev_0 <- levels(cut(as.numeric(unlist(preds_hsu@data[crps_0])), cuts))
    
  }else{
    cuts <- stats::quantile(preds_hsu@data[crps], probs = seq(0, 1, 1/n_cuts), na.rm = T)
    lev_0 <- levels(cut(as.numeric(unlist(preds_hsu@data[crps])), cuts))
    
  }
  
  
  #preds_hsu@data <- melt(preds_hsu@data, id.vars = names(preds_hsu@data)[!sel_cols])
  
  
  if(!file.exists(paste0("capdis/plots"))) dir.create(paste0("capdis/plots"))
  
  #wdt <- hgt <- 21.12
  if (rw < 4){
    wdt <- (21 - 2.1)
    hgt <- (29.7 - 2.97) / 2
  }else{
    wdt <- 21 - 2.1
    hgt <- 29.7 - 2.97
  }
  
  cx <- 1.5 # for all
  
  
  #pdf(paste0(ecampa3res, "/capdis/plots/plot_check.pdf"), width = wdt, height = hgt, pointsize = 8)
  #pdf(paste0(ecampa3res, "/capdis/plots/plot_check.pdf"), width = wdt, height = hgt, pointsize = 8, paper =  "a4")
  jpeg(paste0(ecampa3res, "/capdis/plots/plot_check.jpg"), width = wdt, height = hgt, units = "cm", res = 150, quality = 100, pointsize = 8)
  
  par(#mfrow = c(cl, rw), 
    mar = c(0.5, 1.1, 2.0, 1.1),
    #mar = c(5.1, 4.1, 4.1, 2.1),
    #oma = c(2.5, 0.5, 4, 0.5)
    oma = c(4.5, 0.5, 9, 0.5)
  )
  
  
  lyt <- c(1:((rw-pnls4leg/2)*cl),
           (((rw-pnls4leg/2)*cl)+1):(((rw-pnls4leg/2)*cl)+(cl-pnls4leg/2)),
           rep((categs2plot+1), pnls4leg/2),
           (((rw-pnls4leg/2)*cl)+(cl-pnls4leg/2)+1):categs2plot,
           rep(0, free_pnls-pnls4leg),
           rep((categs2plot+1), pnls4leg/2))
  lyt <- lyt[1:(rw * cl)]
  
  if(rw < 3){
    lyt <- c(lyt[!lyt %in% lyt[which(duplicated(lyt))]], lyt[lyt %in% lyt[which(duplicated(lyt))]])
    lyt <- matrix(lyt, nrow = rw, ncol = cl, byrow = FALSE)
  }else{
    lyt <- matrix(lyt, nrow = rw, ncol = cl, byrow = TRUE)
  }
  
  lyt <- layout(lyt)
  
  for(crp in crps){
    
    if (crp %in% crps_over_sd){
      cuts1 <- cuts_1
      rbPal <- colorRampPalette(c('pink','red'))
      rbPal_1 <- rbPal
    }else{
      cuts1 <- cuts
      rbPal <- colorRampPalette(c('skyblue','darkblue'))
    }
    
    dt2plot <- preds_hsu
    dt2plot@data <- dt2plot@data[, c(no_crps, crp)]
    
    dt2plot$Col <- rbPal(n_cuts)[as.numeric(cut(dt2plot[[crp]], cuts1))]
    
    # labels for the legend
    #dt2plot$bin <- cut(dt2plot[[crp]], cuts1, include.lowest = TRUE, dig.lab = 4)
    #lev01 = levels(dt2plot$bin)
    
    #plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = 0.6)
    #plot(nuts23, lwd=0.5, col = "grey90", add=TRUE)
    plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = cx)
    plot(nuts23, add=TRUE, lwd=0.5)
    #legend("right", fill = rbPal(6), legend = lev, cex = 1.1, title = paste0("LPIS - ", crop))
    box(which = "figure")
    
  }
  
  if (length(crps_over_sd) != 0){
    pos_leg1 <- "top" 
  }else{ 
    if (rw == 1){
      pos_leg1 <- "bottom" 
    }else{
      pos_leg1 <- "top" 
    }
  } 
  
  par(mar = c(0.5, 1.2, 1, 1.2))
  
  plot(dt2plot, col = "white", border="white", main = "", cex.main = 1, col.main = "black")
  #plot(1, type="n", axes=FALSE, xlab="", ylab="", main = paste0(country, " - ", crop), cex.main = 1.2, col.main = "blue")
  sc_value <- round(((dt2plot@bbox[3] - dt2plot@bbox[1]) / 2), - 4)
  sc_b_x <- dt2plot@bbox[1, 1] + ((dt2plot@bbox[1, 2] - dt2plot@bbox[1, 1]) / 4)
  #sc_b_y <- dt2plot@bbox[2, 1] - 50000
  if(length(crps_over_sd) == 0){
    sc_b_y <- dt2plot@bbox[2, 1]
  }else{
    sc_b_y <- mean(dt2plot@bbox[2, ])
  }
  scalebar(sc_value, type = 'bar', divs = 4, cex = (cx - 0.1), below = "meters", xy = c(sc_b_x, sc_b_y))
  legend(pos_leg1, fill = rbPal(n_cuts), legend = lev_0, cex = cx, title = paste0("Legend 1"))
  if(length(crps_over_sd) != 0) legend("bottom", fill = rbPal_1(n_cuts), legend = lev_1, cex = cx, title = paste0("Legend 2"))
  box(which = "figure")
  
  mtext(text = paste0("Header of Plot, etc, etc.  ", paste0("Regions: ", paste(curcountries, collapse = ", "))), 
        side = 3, outer = TRUE, line =  3.1, cex = (1.5 * cx + 0))   #header
  mtext(text = paste0("Sub-title here, etc, etc.  more info more info"), 
        side = 3, outer = TRUE, line =  1, cex = (1.0 * cx + 0))   #sub-title
  mtext(text = paste0("Footer of Plot: here all the information we want to include  ", format(Sys.time(), "%d-%b-%Y")), 
        side = 1, adj = 1, outer = TRUE, line = 2, cex = (cx - 0.5))     #footer
  
  
  
  dev.off()
  
  
  
}



