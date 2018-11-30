loadGISenv<-function(hsu_dir = "\\\\ies\\d5\\agrienv\\Data\\HSU",
                     n23_dir = "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"
){
  #--- reading NUTS3 and NUTS2 data
  
  #n23_dir <- "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"
  if(!exists("nuts23")) {
    cat("\n GISCO_NUTS2_3_2010_with_attr_laea loading ...")
    nuts23 <<- readOGR(dsn = n23_dir, layer = "GISCO_NUTS2_3_2010_with_attr_laea")
  }else{
    cat("\n GISCO_NUTS2_3_2010_with_attr_laea already loaded")
  }
  #ogrInfo(dsn = n23_dir)
  #summary(nuts23)
  #nuts23@proj4string
  #head(nuts23)
  #plot(nuts23)
  
  #--- reading HSU shapefile
  
  #hsu_dir <- "\\\\ies\\d5\\agrienv\\Data\\HSU"
  if(!exists("hsu")) {
    cat("\n hsu_eu28_CH_NO_EXYUG loading...")
    hsu <<- readOGR(dsn = hsu_dir, layer = "hsu_eu28_CH_NO_EXYUG")
  }else{
    cat("\n hsu_eu28_CH_NO_EXYUG already loaded")
  }
  #hsu$CAPRI_HSU <- as.numeric(gsub("U", "", hsu$CAPRI_HSU))
  #head(hsu)
  #ogrInfo(dsn = hsu_dir)
  #hsu@proj4string
  return("")    
}


mapping <- function(scope = "capdiscapreg", 
                    dt_set = NULL,
                    #baseyear = "12", Not needed if data are not loaded
                    curcountries = NULL, by_country = "No", 
                    curyears = "12", 
                    curcrops = NULL, 
                    n_cuts = 6, 
                    cuts_hard = NULL,
                    maxpanelsonpage=7,
                    variab = NULL,
                    flagoutliers = c(NA, NA), # c(lowthreshold, highthreshold)
                    hsu_dir = "\\\\ies\\d5\\agrienv\\Data\\HSU",
                    n23_dir = "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"){
  
  
  cat("\n Load GIS environment")
  loadGISenv(hsu_dir=hsu_dir, n23_dir=n23_dir)
  
  #--- reading CAPRI data for plotting
  # if(is.null(capridat)){
  #   cat("\nLoading capridat", Sys.time())
  #   capridat<-Reduce(rbind, lapply(1:length(curyears), function(x)
  #     Reduce(rbind, lapply(1:length(curcountries), function(y)
  #       opendata(scope = scope,curcountry = curcountries[y], curyear = curyears[x]))
  #     )
  #   )
  #   )
  # }
  
  head(dt_set)
  capridat <- as.data.table(dt_set)
  rm(dt_set) ; gc()
  
  if(is.null(curcrops)) curcrops <- sort(as.vector(unique(capridat$COLS)))
  
  cat("\n Filtering out COLS:", curcrops, ", ROWS:", variab, " and Years:", curyears, 
      " and filtering out HSUs.", format(Sys.time(), "%Y%M%d %H:%M"))
  capridat <- capridat[capridat$COLS %in% curcrops, ]
  capridat <- capridat[capridat$ROWS %in% variab, ]
  capridat <- capridat[capridat$Y %in% curyears, ]

  capridat <- capridat[grepl("^U", capridat$RALL), ]

  capridat$VALUE <- capridat$VALUE + 1e-9
  capri4map <- capridat

  cat("\n Calculating number of panels to plot\n")
  cat(" 1. Regions ")                                                      
  rallinuse<-unique(capridat[,"RALL", with=FALSE])
  # merge hsuinuse with shapefile to extract regions and countries
  rallshape <- merge(hsu, rallinuse, by.x = "CAPRI_HSU", by.y = "RALL", all.x = FALSE)
  rallshape$nuts2 <- substr(rallshape$EEZ_R, 1, 4)
  
  #capridat <- merge(capridat, hsu@data[, c("CAPRI_HSU", "EEZ_R")], by.x = "RALL", by.y = "CAPRI_HSU", all.x = TRUE)
  #capridat$nuts0 <- substr(capridat$EEZ_R, 1, 2)
  #capridat <- as.data.table(capridat)
  #head(capridat)
  
  if (by_country %in% c("Y", "Yes")){       #plot country by country
    rallinuse <- curcountries
    nrall <- length(rallinuse)
  }else if (by_country %in% c("No", "N")){       # plot all Europe (default)
    rallinuse<-"Europe"
    nrall <- 1
  }else if (is.null(by_country)){      # plot NUTS2 by NUTS2
    stop("mapping is not implemented yet to plot data at NUTS2 level")
    rallinuse <- as.data.frame(as.data.frame(rallshape@data[, sel_cols_n3]) %>% group_by(nuts2) %>% summarise_all(funs(sum_nas)))
    nrall <- sum(rallinuse[, -1])
  }else{
    stop("You must define by_country: 'NULL', 'Yes', 'No' (default)")
  }
  
  cat(" 2. Activities (crops, ...) ")
  colsinuse<-as.character(unique(capridat$COLS))
  ncapricols<-length(colsinuse)
  
  cat(" 3. Variables (N2O emissions, area, ...) ")
  rowsinuse<-as.character(unique(capridat$ROWS))
  ncaprirows<-length(rowsinuse)
  
  cat(" 4. Years ")
  yearsinuse<-as.character(unique(capridat$Y))
  nyears<-length(yearsinuse)
  
  
  categs2plot<-nrall * ncapricols * ncaprirows * nyears
  
  cat("\n Determine number of pages")
  npages<-ceiling(categs2plot/maxpanelsonpage)
  if (by_country %in% c("Y", "Yes")) panperpage <- ceiling(categs2plot / npages)
  ipanel<-0
  plotmatrix<-matrix(nrow = categs2plot, ncol = 4)
  # Extract data table with all data that are to be plotted
  for(irall in 1:nrall){
    for(icols in 1:ncapricols){
      for(irows in 1:ncaprirows){
        for(iyears in 1:nyears){
          ipanel <- ipanel + 1
          plotmatrix[ipanel, 1]<-as.character(unlist(rallinuse[irall]))
          plotmatrix[ipanel, 2]<-as.character(unlist(colsinuse[icols]))
          plotmatrix[ipanel, 3]<-as.character(unlist(rowsinuse[irows]))
          plotmatrix[ipanel, 4]<-as.character(unlist(yearsinuse[iyears]))
        }
      }
    }
  }
  pagename_old <- "1"
  #curpanels02 <- c()
  for(page in 1:npages){
#    curpanels01 <- c()
#    cat("\n plotting page: ",page)
#    sel<-vector(length = nrow(capridat))
#    if (by_country %in% c("Y", "Yes") & (length(yearsinuse) %% maxpanelsonpage) != 0){
#     curpanels <- which(plotmatrix[, 1] %in% unique(plotmatrix[, 1])[page])[1:maxpanelsonpage]
#      curpanels1 <- which(plotmatrix[, 1] %in% unique(plotmatrix[, 1])[page])[(maxpanelsonpage + 1):length(which(plotmatrix[, 1] %in% unique(plotmatrix[, 1])[page]))]
#      if (!all(is.na(curpanels1))){
#        curpanels01 <- c(curpanels01, curpanels1)
#        curpanels02 <- c(curpanels02, curpanels01)
#      }else{
#        curpanels <- curpanels02
#      }
#      
#    }else{
#      curpanels<-(maxpanelsonpage * (page-1) +1): min(categs2plot, maxpanelsonpage * page)
#    }
    cat("\n plotting page: ",page)
    sel<-vector(length = nrow(capridat))
    #if(page == 24) stop()
    if (by_country %in% c("Y", "Yes")) {
      curpanels <- (panperpage * (page-1) +1): (panperpage * page)
    }else{
      curpanels<-(maxpanelsonpage * (page-1) +1): min(categs2plot, maxpanelsonpage * page)
    }

    for(ipanel in 1:sum(curpanels <= categs2plot)){
      sel <- sel |
        (capridat$COLS == plotmatrix[curpanels[ipanel],2] &
         capridat$ROWS == plotmatrix[curpanels[ipanel],3] &
         capridat$Y    == plotmatrix[curpanels[ipanel],4])
    }
    
    capripage<-capridat[sel, ]
    colsinpage<-paste(unique(capripage$COLS), collapse = "-")
    rowsinpage<-paste(unique(capripage$ROWS), collapse = "-")
    yearinpage<-range(unique(capripage$Y))
    if(yearinpage[1] == yearinpage[2]) yearinpage <- yearinpage[1]
    yearinpage<-paste(yearinpage, collapse = "-")
    rallinpage<-paste(unique(plotmatrix[curpanels[curpanels <= categs2plot], 1]), collapse = "-")
    pagename<-paste(c(rallinpage, colsinpage, rowsinpage, yearinpage), collapse = "_")
    if (pagename_old != pagename){
      pg <- 1
    }else{
      pg <- pg + 1
    }
    pagename_old <- pagename
    
    pagename1<-paste0("xobs12_", pagename, "_", pg, ".jpg")
    #if(page < 24) next
    
    #if(!file.exists(paste0("capdis/plots"))) dir.create(paste0("capdis/plots"))
    #pdf(paste0(ecampa3res, "/capdis/plots/plot_check.pdf"), width = wdt, height = hgt, pointsize = 8)
    #pdf(paste0(ecampa3res, "/capdis/plots/plot_check.pdf"), width = wdt, height = hgt, pointsize = 8, paper =  "a4")
    #jpeg(paste0(getwd(), "/capdis/plots/xobs12_", variab, "_", paste(rallinuse, collapse = "-"), "_", paste(range(curyears), collapse = "-"), "_", page, ".jpg"), 
    #jpeg(paste0("xobs12_", variab, "_", paste(rallinuse, collapse = "-"), "_", paste(range(curyears), collapse = "-"), "_", page, ".jpg"), 
         
    
    
    # Now make legend determination & layout of page
    # Loop over each panel and do
    # dcast data, merge with shape, plot panel
    
    

    # Here select only one variable
    capri4map <- dcast(capripage, RALL + ROWS + Y ~ COLS, drop = TRUE, value.var = "VALUE", sum, na.rm=TRUE)
    capri4map[capri4map==0]<-NA
    #if (length(unique(capri4map$ROWS)) == 1) capridat <- capridat[, !names(capridat) %in% "ROWS"]

    capri4map <- merge(capri4map, hsu@data[, names(hsu@data) %in% c("CAPRI_HSU", "EEZ_R")], by.x = "RALL", by.y = "CAPRI_HSU", all.x = TRUE)

    sel_cols <- !names(capri4map) %in% c("RALL", "ROWS", "Y", "EEZ_R")
    
    crps <- names(capri4map)[sel_cols]
    no_crps <- names(capri4map)[!sel_cols]
    
    #  if (!is.null(curcrops)){
    #    crps <- crps[crps %in% curcrops]
    #    preds_hsu@data <- preds_hsu@data[, c(no_crps, crps)]
    #  }
    
    length(crps)
    sel_cols_n3 <- !names(capri4map) %in% c("RALL", "ROWS", "Y", "EEZ_R")
    
    
    # Find which categories should be plotted in a different scale because values are to big
    # Two different scales will be used (if necessary), and two different legends generated
    rnge_crps <- apply(capri4map[crps], 2, range, na.rm = T)
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
    
    if (length(crps_over_sd) != 0){ 
      n_legs <- 2
    }else{
      n_legs <- 1
    }
    n_plots <- length(curpanels) + n_legs
    
    rw <- floor(sqrt(n_plots))
    cl <- ceiling(n_plots / floor(sqrt(n_plots)))
    
    tot_pnls <- cl * rw
    free_pnls <- tot_pnls - length(curpanels)
    pnls4leg <- floor(free_pnls / 2) * 2
    
    
    if (is.null(cuts_hard)){
      if (length(crps_over_sd) != 0){
        
        cuts_1 <- stats::quantile(capri4map[crps_over_sd][capri4map[crps_over_sd] > 0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
        lev_1 <- levels(cut(as.numeric(unlist(capri4map[crps_over_sd])), cuts_1))
        
        crps_0 <- crps[!crps %in% crps_over_sd]
        cuts <- stats::quantile(capri4map[crps_0][capri4map[crps_0] > 0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
        lev_0 <- levels(cut(as.numeric(unlist(capri4map[crps_0])), cuts))
        
      }else{
        
        #cuts <- stats::quantile(preds_hsu@data[crps], probs = seq(0, 1, 1/n_cuts), na.rm = T)
        cuts <- stats::quantile(capri4map[crps][capri4map[crps] > 0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
        lev_0 <- levels(cut(as.numeric(unlist(capri4map[crps])), cuts))
      }
      
    }else{
      cuts <- round(cuts_hard, 2)
      lev_0 <- c()
      for (c in 1:(length(cuts) - 1)) {
        lev_0 <- c(lev_0, paste0("(", cuts[c], ",", cuts[c + 1], "]" ))
      }
    }
    
    
    
    #wdt <- hgt <- 21.12
    if (rw == 1){
      wdt <- (21 - 2.1)
      hgt <- (29.7 - 2.97) / 3
    }else if (rw == 2){
      wdt <- (21 - 2.1)
      hgt <- (29.7 - 2.97) / 2
    }else if (rw == 3){
      wdt <- (21 - 2.1)
      hgt <- (29.7 - 2.97) * 3 / 4
    }else{
      wdt <- 21 - 2.1
      hgt <- 29.7 - 2.97
    }
    
    
    jpeg(pagename1, width = wdt, height = hgt, units = "cm", res = 300, quality = 100, pointsize = 8)
    
    cx <- 1.5 # for all
    #if(by_country %in% c("Y", "Yes")) cx <- 1.3
    
    
    par(#mfrow = c(cl, rw), 
      #mar = c(0.5, 1.1, 2.8, 1.1),
      mar = c(0.5, 1.1, 3.8, 1.1),
      #mar = c(5.1, 4.1, 4.1, 2.1),
      #oma = c(2.5, 0.5, 4, 0.5)
      oma = c(4.5, 0.5, 9, 0.5)
    )
    
    lyt <- c(1:((rw-pnls4leg/2)*cl),
             (((rw-pnls4leg/2)*cl)+1):(((rw-pnls4leg/2)*cl)+(cl-pnls4leg/2)),
             rep((length(curpanels)+1), pnls4leg/2),
             (((rw-pnls4leg/2)*cl)+(cl-pnls4leg/2)+1):length(curpanels),
             rep(0, free_pnls-pnls4leg),
             rep((length(curpanels)+1), pnls4leg/2))
    lyt <- lyt[1:(rw * cl)]
    
    if(rw < 3){
      lyt <- c(lyt[!lyt %in% lyt[which(duplicated(lyt))]], lyt[lyt %in% lyt[which(duplicated(lyt))]])
      lyt <- matrix(lyt, nrow = rw, ncol = cl, byrow = FALSE)
    }else{
      lyt <- matrix(lyt, nrow = rw, ncol = cl, byrow = FALSE)
    }
    
    lyt <- layout(lyt)
    
    
    if (by_country %in% c("Y", "Yes")){       #plot the country by country
      
      for (ct in sort(unique(plotmatrix[curpanels[curpanels <= categs2plot], 1]))){
        
        capri4map_ct <- capri4map[substr(capri4map$EEZ_R, 1, 2) %in% ct, ]
        
        for(crp in crps){
          if (crp %in% crps_over_sd){
            cuts1 <- cuts_1
            rbPal <- colorRampPalette(c('pink','red'))
            rbPal_1 <- rbPal
            col_neg <- "blue"
            col_outl <- "goldenrod2"
            
          }else{
            cuts1 <- cuts
            rbPal <- colorRampPalette(c('skyblue','darkblue'))
            rbPal_2 <- rbPal
            col_neg <- "red"
            col_outl <- "goldenrod2"
            
          }
          
        
          for (yr in sort(unique(plotmatrix[curpanels[curpanels <= categs2plot], 4]))){
            
            if (!paste(c(ct, crp, yr), collapse = "") %in% apply(plotmatrix[curpanels[curpanels <= categs2plot], -3], 1, paste, collapse = "")) next
            
            cat("\n plotting panel: ", ct, "-", yr,"-", crp, format(Sys.time(), "%Y%M%d %H:%M"))
            
            capri4map_yr <- capri4map_ct[capri4map_ct$Y %in% yr, ]
            
            preds_hsu <- merge(hsu, capri4map_yr[, !names(capri4map_yr) %in% c("EEZ_R") ], by.x = "CAPRI_HSU", by.y = "RALL", all.x = FALSE)
            names(preds_hsu)[1] <- "RALL"
            
            
            
            dt2plot <- preds_hsu
            dt2plot <- dt2plot[, c(no_crps, crp)]
            
            if (!is.na(flagoutliers[1])){
              dt2plot_negs <- dt2plot
              dt2plot_negs <- dt2plot_negs[which(dt2plot_negs@data[, crp] < flagoutliers[1]), ]
            }
            if (!is.na(flagoutliers[2])){
              dt2plot_outl <- dt2plot
              dt2plot_outl <- dt2plot_outl[which(dt2plot_outl@data[, crp] > flagoutliers[2]), ]
            }
            dt2plot$Col <- rbPal(n_cuts)[as.numeric(cut(dt2plot[[crp]], cuts1))]
            
            
            # labels for the legend
            #dt2plot$bin <- cut(dt2plot[[crp]], cuts1, include.lowest = TRUE, dig.lab = 4)
            #lev01 = levels(dt2plot$bin)
            
            #plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = 0.6)
            #plot(nuts23, lwd=0.5, col = "grey90", add=TRUE)
            
            nuts23_inuse <- nuts23
            nuts23_inuse <- nuts23_inuse[nuts23_inuse@data$NUTS3_ID10 %in% preds_hsu@data$EEZ_R, ]
            
            plot(nuts23_inuse, lwd=0.5, col = "white", border = "white", main = "", cex.main = cx)
            plot(nuts23, add = TRUE, lwd = 0.5)
            plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, cex.main = cx, add = TRUE)
            title(paste0(ct, " / ", yr, ": ", crp), adj = 0.5, line = 2)
            if(exists("dt2plot_negs") && nrow(dt2plot_negs@data) > 0){
              exis_negs <- 1
              plot(dt2plot_negs, col = col_neg, border = col_neg, add = TRUE, lwd = 3)
              #print(paste0(ct, " / ", crp, " : number of negative values = ", nrow(dt2plot_negs@data)))
              mtext(text = paste0("number of outliers smaller than threshold (", flagoutliers[1], ") = ", nrow(dt2plot_negs@data)), 
                    side = 3, cex = (cx - 1.0))  
            } 
            
            if(exists("dt2plot_outl") && nrow(dt2plot_outl@data) > 0){
              exis_outl <- 1
              plot(dt2plot_outl, col = col_outl, border = col_outl, add = TRUE, lwd = 3)
              #print(paste0(ct, " / ", crp, " : number of negative values = ", nrow(dt2plot_negs@data)))
              mtext(text = paste0("number of outliers bigger than threshold (", flagoutliers[2], ") = ", nrow(dt2plot_outl@data)), 
                    side = 3, line = 0.8, cex = (cx - 1.0))     
            } 
            #plot(nuts23, add=TRUE, lwd=0.5)
            #legend("right", fill = rbPal(6), legend = lev, cex = 1.1, title = paste0("LPIS - ", crop))
            box(which = "figure")
            
          }
        }
      } 
      
    }else{       # plot all Europe (default) or NUTS2 by NUTS2
      

      
      for (yr in sort(unique(capri4map$Y))){
        
        capri4map_yr <- capri4map[capri4map$Y %in% yr, ]
        
        preds_hsu <- merge(hsu, capri4map_yr, by.x = "CAPRI_HSU", by.y = "RALL", all.x = FALSE)
        names(preds_hsu)[1] <- "RALL"
        
        for(crp in crps){
          cat("\n plotting panel: ",yr,"-",crp, format(Sys.time(), "%Y%M%d %H:%M"))
          if (crp %in% crps_over_sd){
            cuts1 <- cuts_1
            rbPal <- colorRampPalette(c('pink','red'))
            rbPal_1 <- rbPal
            col_neg <- "blue"
            col_outl <- "goldenrod2"
            
          }else{
            cuts1 <- cuts
            rbPal <- colorRampPalette(c('skyblue','darkblue'))
            rbPal_2 <- rbPal
            col_neg <- "red"
            col_outl <- "goldenrod2"
          }
          
          dt2plot <- preds_hsu
          dt2plot <- dt2plot[, c(no_crps, crp)]
          #dt2plot <- dt2plot[dt2plot@data$Y %in% yr, ]
          if (!is.na(flagoutliers[1])){
            dt2plot_negs <- dt2plot
            dt2plot_negs <- dt2plot_negs[which(dt2plot_negs@data[, crp] < 0), ]
          }
          if (!is.na(flagoutliers[2])){
            dt2plot_outl <- dt2plot
            dt2plot_outl <- dt2plot_outl[which(dt2plot_outl@data[, crp] > flagoutliers[2]), ]
          }
          
          dt2plot$Col <- rbPal(n_cuts)[as.numeric(cut(dt2plot[[crp]], cuts1))]
          
          # labels for the legend
          #dt2plot$bin <- cut(dt2plot[[crp]], cuts1, include.lowest = TRUE, dig.lab = 4)
          #lev01 = levels(dt2plot$bin)
          
          #plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = 0.6)
          #plot(nuts23, lwd=0.5, col = "grey90", add=TRUE)
          
          nuts23_inuse <- nuts23
          nuts23_inuse <- nuts23_inuse[nuts23_inuse@data$NUTS3_ID10 %in% preds_hsu@data$EEZ_R, ]
          
          plot(nuts23_inuse, col = "white", border = "white", main = "", cex.main = cx)
          plot(nuts23, add = TRUE, lwd = 0.3, border = "grey")
          plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, add = TRUE)
          title(paste0(yr, " / ", crp), adj = 0.5, line = 2)
          
          if(exists("dt2plot_negs") && nrow(dt2plot_negs@data) > 0){
            exis_negs <- 1
            plot(dt2plot_negs, col = col_neg, border = col_neg, add = TRUE, lwd = 3)
            #print(paste0(ct, " / ", crp, " : number of negative values = ", nrow(dt2plot_negs@data)))
            mtext(text = paste0("number of outliers smaller than threshold (", flagoutliers[1], ") = ", nrow(dt2plot_negs@data)), 
                  side = 3, cex = (cx - 1.0))  
          } 
          
          if(exists("dt2plot_outl") && nrow(dt2plot_outl@data) > 0){
            exis_outl <- 1
            plot(dt2plot_outl, col = col_outl, border = col_outl, add = TRUE, lwd = 3)
            #print(paste0(ct, " / ", crp, " : number of negative values = ", nrow(dt2plot_negs@data)))
            mtext(text = paste0("number of outliers bigger than threshold (", flagoutliers[2], ") = ", nrow(dt2plot_outl@data)), 
                  side = 3, line = 0.8, cex = (cx - 1.0))     
            
          } 
          #plot(nuts23, add=TRUE, lwd = 0.1, border = "grey")
          #plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(yr, " / ", crp), cex.main = cx, add = T)
          
          #legend("right", fill = rbPal(6), legend = lev, cex = 1.1, title = paste0("LPIS - ", crop))
          box(which = "figure")
          
        }
      }
    }
    
    cat("\n plotting legends", format(Sys.time(), "%Y%M%d %H:%M"))
    if (length(crps_over_sd) != 0){
      pos_leg1 <- "top" 
    }else{ 
      if (rw == 1){
        pos_leg1 <- "top" 
      }else{
        pos_leg1 <- "center" 
      }
    } 
    
    par(mar = c(0.5, 1.2, 1, 1.2))
    
    plot(dt2plot, col = "white", border="white", main = "", cex.main = 1, col.main = "black")
    #plot(1, type="n", axes=FALSE, xlab="", ylab="", main = paste0(country, " - ", crop), cex.main = 1.2, col.main = "blue")
    
    if((tot_pnls - length(curpanels)) == 1) {
      cxl <- cx*2/3
    }else{
      cxl <- cx
    }
    
    if(variab == "SURSOI"){
      ttl <- "SOIL NITROGEN SURPLUS BELOW ROOT ZONE"
      lgd <- expression(paste("SURSOI [kg N ", "ha"^"-1", " yr"^"-1", "]"))

      if(!any(rallinuse %in% "Europe")){
        ttl <- paste0(ttl)
      }
    }else{
      ttl <- "title here"
      lgd <- "Legend"
    }
    
    sc_value <- round(((dt2plot@bbox[3] - dt2plot@bbox[1]) / 2), - 4)
    sc_b_x <- dt2plot@bbox[1, 1] + ((dt2plot@bbox[1, 2] - dt2plot@bbox[1, 1]) / 4)
    #sc_b_y <- dt2plot@bbox[2, 1] - 50000
    if(length(crps_over_sd) == 0){
      sc_b_y <- dt2plot@bbox[2, 1]
    }else{
      sc_b_y <- mean(dt2plot@bbox[2, ])
    }
    scalebar(sc_value, type = 'bar', divs = 4, cex = (cxl - 0.1), below = "meters", xy = c(sc_b_x, sc_b_y))
    
    if(exists("exis_negs") | exists("exis_outl")){
      cols_other <- c()
      cols_other1 <- c()
      cols_other2 <- c()
      tt <- c()
      tt1 <- c()
      tt2 <- c()
      for (i in c("exis_negs", "exis_outl")){
        if (exists(i) && i == "exis_negs"){ cols_other1 <- c("red"); tt1 <- paste0("values < ", flagoutliers[1])}
        if (exists(i) && i == "exis_outl"){ cols_other2 <- col_outl; tt2 <- paste0("values > ", flagoutliers[2])}
      }
      cols_other <- c(cols_other1, cols_other2)
      tt <- c(tt1, tt2)
      
      legend(pos_leg1, fill = c(rbPal_2(n_cuts), cols_other), legend = c(lev_0, tt), cex = cxl, title = lgd, ncol = 2)
      if(length(crps_over_sd) != 0) {
        cols_other <- gsub("red", "blue", cols_other)
        legend("bottom", fill = c(rbPal_1(n_cuts), cols_other), legend = c(lev_1, tt), cex = cx, title = lgd, ncol = 2)
      }
      if(exists("exis_negs")) rm(exis_negs)
      if(exists("exis_outl")) rm(exis_outl)
    }else{
      legend(pos_leg1, fill = rbPal_2(n_cuts), legend = lev_0, cex = cxl, title = lgd, ncol = 2)
      if(length(crps_over_sd) != 0) legend("bottom", fill = rbPal_1(n_cuts), legend = lev_1, cex = cx, title = lgd, ncol = 2)
    }
    box(which = "figure")
    
   
    mtext(text = paste0(ttl), 
          side = 3, outer = TRUE, line =  4, cex = (1.5 * cx + 0))   #header
    mtext(text = paste0("CAPRI DISAGGREGATION"), 
          side = 3, outer = TRUE, line =  1, cex = (1.0 * cx + 0))   #sub-title
    mtext(text = paste0("https://github.com/aleip/capriextract    ", format(Sys.time(), "%d-%b-%Y")), 
          side = 1, adj = 1, outer = TRUE, line = 3, cex = (cx - 0.9))     #footer
    mtext(text = paste0("CAPRI TIME SERIES 20180711"), 
          side = 1, adj = 0, outer = TRUE, line = 2, cex = (cx - 0.9))     #footer
    mtext(text = paste0("CAPRI Time series disaggregated 20181121"), 
          side = 1, adj = 0, outer = TRUE, line = 3, cex = (cx - 0.9))     #footer
    
    
    
    
    dev.off()
    
    
    
    
    
    
  }
  
  
}  
  
 
  

