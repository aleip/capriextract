

mapping <- function(scope = "capdiscapreg", curyears = "12", baseyear = "12", 
                    curcountries = NULL, by_country = "No", 
                    crop = NULL, n_cuts = 6, cuts = NULL,
                    variab = NULL,
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
  
  
  capri4map <- capridat[capridat$ROWS=="SURSOI",]
  capri4map$VALUE <- capri4map$VALUE + 1e-9
  
  # Here select only one variable
  capri4map <- dcast(capri4map, RALL + ROWS ~ COLS, drop = TRUE, value.var = "VALUE", sum, na.rm=TRUE)
  capri4map[capri4map==0]<-NA
  if (length(unique(capri4map$ROWS)) == 1) capridat <- capridat[, !names(capridat) %in% "ROWS"]
  head(capri4map)
  
  #
  
  #--- reading NUTS3 and NUTS2 data
  
  #n23_dir <- "\\\\ies\\d5\\agrienv\\Data\\GIS_basedata\\GISCO_2010_NUTS2_3"
  if(!exists("nuts23")) nuts23 <<- readOGR(dsn = n23_dir, layer = "GISCO_NUTS2_3_2010_with_attr_laea")
  #ogrInfo(dsn = n23_dir)
  #summary(nuts23)
  #nuts23@proj4string
  #head(nuts23)
  #plot(nuts23)
  
  #--- reading HSU shapefile
  
  #hsu_dir <- "\\\\ies\\d5\\agrienv\\Data\\HSU"
  if(!exists("hsu")) hsu <<- readOGR(dsn = hsu_dir, layer = "hsu_eu28_CH_NO_EXYUG")
  #hsu$CAPRI_HSU <- as.numeric(gsub("U", "", hsu$CAPRI_HSU))
  #head(hsu)
  #ogrInfo(dsn = hsu_dir)
  #hsu@proj4string
  
  
  # merging predictions to HSU shapefile
  preds_hsu <- merge(hsu, capri4map, by.x = "CAPRI_HSU", by.y = "RALL", all.x = FALSE)
  #summary(preds_hsu)
  
  
  preds_hsu@data$EEZ_R <- as.character(preds_hsu@data$EEZ_R)
  #apply(preds_hsu@data, 2, function(x) sum(is.na(x)))
  preds_hsu@data$nuts2 <- as.vector(substr(preds_hsu$EEZ_R, 1, 4))
  
  regs <- as.vector(unique(preds_hsu@data$nuts2))
  n_regs <- length(regs)
  
  sel_cols <- !names(preds_hsu) %in% c("CAPRI_HSU", "OBJECTID", "codes", "x", "y", "EEZ_R", "CNTR_ENGL", "AREA", "AREAcon", "UAAR", "nuts2", 
  sel_cols <- !names(preds_hsu) %in% c("CAPRI_HSU", "OBJECTID", "codes", "x", "y", "EEZ_R", "CNTR_ENGL", "AREA", "AREAcon", "UAAR", "nuts2")
  
  crps <- names(preds_hsu)[sel_cols]
  no_crps <- names(preds_hsu)[!sel_cols]
  
  if (!is.null(curcrops)){
    crps <- crps[crps %in% curcrops]
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
  
  
  if (by_country %in% c("Y", "Yes")){       #plot the country by country
    categs2plot <- length(unique(substr(preds_hsu$nuts2, 1, 2))) * length(crps)
  }else if (by_country %in% c("No", "N")){       # plot all Europe (default)
    categs2plot <- length(crps)
  }else if (is.null(by_country)){      # plot NUTS2 by NUTS2
    categs2plot <- as.data.frame(as.data.frame(preds_hsu@data[, sel_cols_n3]) %>% group_by(nuts2) %>% summarise_all(funs(sum_nas)))
    categs2plot <- sum(categs2plot[, -1])
  }else{
    stop("You must define by_country: 'NULL', 'Yes', 'No' (default)")
  }
  
  
  
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
  
  
  if (is.null(cuts)){
    if (length(crps_over_sd) != 0){
      
      cuts_1 <- stats::quantile(preds_hsu@data[crps_over_sd][preds_hsu@data[crps_over_sd] > 0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
      lev_1 <- levels(cut(as.numeric(unlist(preds_hsu@data[crps_over_sd])), cuts_1))
      
      crps_0 <- crps[!crps %in% crps_over_sd]
      cuts <- stats::quantile(preds_hsu@data[crps_0][preds_hsu@data[crps_0] > 0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
      lev_0 <- levels(cut(as.numeric(unlist(preds_hsu@data[crps_0])), cuts))
      
    }else{
      
      #cuts <- stats::quantile(preds_hsu@data[crps], probs = seq(0, 1, 1/n_cuts), na.rm = T)
      cuts <- stats::quantile(preds_hsu@data[crps][preds_hsu@data[crps] > 0], probs = seq(0, 1, 1/n_cuts), na.rm = T)
      lev_0 <- levels(cut(as.numeric(unlist(preds_hsu@data[crps])), cuts))
    }
    
  }else{
    cuts <- round(cuts, 2)
    lev_0 <- c()
    for (c in 1:(length(cuts) - 1)) {
      lev_0 <- c(lev_0, paste0("(", cuts[c], ",", cuts[c + 1], "]" ))
    }
  }
  
  
  #preds_hsu@data <- melt(preds_hsu@data, id.vars = names(preds_hsu@data)[!sel_cols])
  
  
  if(!file.exists(paste0("capdis/plots"))) dir.create(paste0("capdis/plots"))
  
  #wdt <- hgt <- 21.12
  if (rw == 1){
    wdt <- (21 - 2.1)
    hgt <- (29.7 - 2.97) / 3
  }else if (rw < 4){
    wdt <- (21 - 2.1)
    hgt <- (29.7 - 2.97) / 2
  }else{
    wdt <- 21 - 2.1
    hgt <- 29.7 - 2.97
  }
  
  cx <- 1.5 # for all
  
  
  #pdf(paste0(ecampa3res, "/capdis/plots/plot_check.pdf"), width = wdt, height = hgt, pointsize = 8)
  #pdf(paste0(ecampa3res, "/capdis/plots/plot_check.pdf"), width = wdt, height = hgt, pointsize = 8, paper =  "a4")
  jpeg(paste0(getwd(), "/capdis/plots/plot_check.jpg"), width = wdt, height = hgt, units = "cm", res = 150, quality = 100, pointsize = 8)
  
  par(#mfrow = c(cl, rw), 
    mar = c(0.5, 1.1, 2.8, 1.1),
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
  
  
  
  if (by_country %in% c("Y", "Yes")){       #plot the country by country
    
    for (ct in unique(substr(preds_hsu$nuts2, 1, 2))){
      
      
      for(crp in crps){
        
        if (crp %in% crps_over_sd){
          cuts1 <- cuts_1
          rbPal <- colorRampPalette(c('pink','red'))
          rbPal_1 <- rbPal
          col_neg <- "blue"
          
        }else{
          cuts1 <- cuts
          rbPal <- colorRampPalette(c('skyblue','darkblue'))
          rbPal_2 <- rbPal
          col_neg <- "red"
        }
        
        dt2plot <- preds_hsu
        
        dt2plot <- dt2plot[grepl(paste0("^", ct), dt2plot@data$nuts2), c(no_crps, crp)]
        
        dt2plot_negs <- dt2plot
        dt2plot_negs <- dt2plot_negs[which(dt2plot_negs@data[, crp] < 0), ]
        
        dt2plot$Col <- rbPal(n_cuts)[as.numeric(cut(dt2plot[[crp]], cuts1))]
        
        # labels for the legend
        #dt2plot$bin <- cut(dt2plot[[crp]], cuts1, include.lowest = TRUE, dig.lab = 4)
        #lev01 = levels(dt2plot$bin)
        
        #plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = 0.6)
        #plot(nuts23, lwd=0.5, col = "grey90", add=TRUE)
        
        plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(ct, ": ", crp), cex.main = cx)
        if(nrow(dt2plot_negs@data) > 0){
          exis_negs <- 1
          plot(dt2plot_negs, col = col_neg, border = col_neg, add = TRUE, lwd = 3)
          #print(paste0(ct, " / ", crp, " : number of negative values = ", nrow(dt2plot_negs@data)))
          mtext(text = paste0("number of negative values = ", nrow(dt2plot_negs@data)), 
                side = 3, cex = (cx - 1.0))     
          
        } 
        plot(nuts23, add=TRUE, lwd=0.5)
        #legend("right", fill = rbPal(6), legend = lev, cex = 1.1, title = paste0("LPIS - ", crop))
        box(which = "figure")
        
      }
      
    } 
    
  }else{       # plot all Europe (default) or NUTS2 by NUTS2
    for(crp in crps){
      if (crp %in% crps_over_sd){
        cuts1 <- cuts_1
        rbPal <- colorRampPalette(c('pink','red'))
        rbPal_1 <- rbPal
        col_neg <- "blue"
      }else{
        cuts1 <- cuts
        rbPal <- colorRampPalette(c('skyblue','darkblue'))
        rbPal_2 <- rbPal
        col_neg <- "red"
      }
      
      dt2plot <- preds_hsu
      dt2plot@data <- dt2plot@data[, c(no_crps, crp)]
      
      dt2plot_negs <- dt2plot
      dt2plot_negs <- dt2plot_negs[which(dt2plot_negs@data[, crp] < 0), ]
      
      dt2plot$Col <- rbPal(n_cuts)[as.numeric(cut(dt2plot[[crp]], cuts1))]
      
      # labels for the legend
      #dt2plot$bin <- cut(dt2plot[[crp]], cuts1, include.lowest = TRUE, dig.lab = 4)
      #lev01 = levels(dt2plot$bin)
      
      #plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = 0.6)
      #plot(nuts23, lwd=0.5, col = "grey90", add=TRUE)
      
      plot(dt2plot, col = dt2plot$Col, border=dt2plot$Col, main = paste0(crp), cex.main = cx)
      if(nrow(dt2plot_negs@data) > 0){
        exis_negs <- 1
        plot(dt2plot_negs, col = col_neg, border = col_neg, add = TRUE, lwd = 3)
        #print(paste0(ct, " / ", crp, " : number of negative values = ", nrow(dt2plot_negs@data)))
        mtext(text = paste0("number of negative values = ", nrow(dt2plot_negs@data)), 
              side = 3, cex = (cx - 1.0))     
        
      } 
      plot(nuts23, add=TRUE, lwd=0.5)
      #legend("right", fill = rbPal(6), legend = lev, cex = 1.1, title = paste0("LPIS - ", crop))
      box(which = "figure")
      
    }
    
    
    
  }
  
  
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
  sc_value <- round(((dt2plot@bbox[3] - dt2plot@bbox[1]) / 2), - 4)
  sc_b_x <- dt2plot@bbox[1, 1] + ((dt2plot@bbox[1, 2] - dt2plot@bbox[1, 1]) / 4)
  #sc_b_y <- dt2plot@bbox[2, 1] - 50000
  if(length(crps_over_sd) == 0){
    sc_b_y <- dt2plot@bbox[2, 1]
  }else{
    sc_b_y <- mean(dt2plot@bbox[2, ])
  }
  scalebar(sc_value, type = 'bar', divs = 4, cex = (cx - 0.1), below = "meters", xy = c(sc_b_x, sc_b_y))
  if(exis_negs > 0){
    legend(pos_leg1, fill = c(rbPal_2(n_cuts), "red"), legend = c(lev_0, "values < 0"), cex = cx, title = paste0("Legend 3"))
    if(length(crps_over_sd) != 0) legend("bottom", fill = c(rbPal_1(n_cuts), "blue"), legend = c(lev_1, "values < 0"), cex = cx, title = paste0("Legend 4"))
    
  }else{
    legend(pos_leg1, fill = rbPal_2(n_cuts), legend = lev_0, cex = cx, title = paste0("Legend 1"))
    if(length(crps_over_sd) != 0) legend("bottom", fill = rbPal_1(n_cuts), legend = lev_1, cex = cx, title = paste0("Legend 2"))
  }
  box(which = "figure")
  
  mtext(text = paste0("Header of Plot, etc, etc.  ", paste0("Regions: ", paste(curcountries, collapse = ", "))), 
        side = 3, outer = TRUE, line =  3.1, cex = (1.5 * cx + 0))   #header
  mtext(text = paste0("Sub-title here, etc, etc.  more info more info"), 
        side = 3, outer = TRUE, line =  1, cex = (1.0 * cx + 0))   #sub-title
  mtext(text = paste0("Footer of Plot: here all the information we want to include  ", format(Sys.time(), "%d-%b-%Y")), 
        side = 1, adj = 1, outer = TRUE, line = 2, cex = (cx - 0.5))     #footer
  
  rm(exis_negs)
  
  
  dev.off()
  
  
  
}
