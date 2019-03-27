f <- function(curfoc, filen='20190311_scenarioforum', nscens=24){
  paste0(cenv$resdir, "/capmod/", filen, "/", filen, "SSPs", 
         curfoc, "_", nscens, "scens.rdata")
}
roundO <- function(a){
  
  # Divide by 1000 for values between 10000 and 1000000
  div <- 10**(3*floor((log10(a)-1) /3))
  
  # Round to full numbers for 100 adn 1000
  divround <- 0
  if(floor(log10(a)) %% 3 == 1) divround <- 1
  
  n <- round(a/div, divround)
  return(list(div, divround))
  
}

plotrelem <- function(x = NULL,             # Passes data table. If NULL, the file filen is loaded
                      filen = "",
                      curfoc = "",
                      refrun = NULL,
                      filternuts0 = TRUE,
                      swapcols = FALSE,
                      sumcols = NULL,
                      keepruns = NULL,
                      par2plot="GCH4",
                      tit=NULL,
                      punit=NULL){
  
  library(RColorBrewer)
  require(ggplot2)
  filename <- f(curfoc = curfoc, filen = filen, nscens = nscens)
  cat("\n", filename)
  if(is.null(x)) load(filename)
  if(swapcols) setnames(x, c("cols", "rows"), c("rows", "cols"))
  #View(x)
  
  if(! is.null(sumcols)){
    xxn <- names(x)
    if(sumcols=='all'){
      xx <- x[, .(value = sum(value)), by=setdiff(xxn, c("cols", "value"))]
      xx$cols <- curfoc
    }else{
      xx <- x[grepl(sumcols, cols), 1:8]
      xx <- xx[, .(value = sum(value)), by=setdiff(xxn, c("cols", "value"))]
      xx$cols <- sumcols
    }
    x <- xx[, .SD, .SDcols = xxn]
  }
  save(list=objects(), file="x1.rdata")
  
  # See http://sape.inf.usi.ch/quick-reference/ggplot2/shape
  #     for using characters as shape
  # See https://stackoverflow.com/questions/31315112/add-points-to-grouped-bar-plot-ggplot2
  #     for adding points to barplots
  if(filternuts0){
    #y <- dcast.data.table(x[cols == par2plot & rall %in% s$nuts0[1:27]], rall ~ scen, value.var = "value")
    y <- x[cols == par2plot & rall %in% s$nuts0[1:27]]
  }else{
    y <- x[cols == par2plot]
  }
  y <- y[ , .(value = sum(value)), by = .(y, ssp, run)]
  
  # Adding columns for character and position of 'points'
  y[, value := round(value, 0)]
  #y[, valhalf := value/2]
  # The characters 50 ff indicate the numbers 1ff
  y[, sspn := sum(49, as.numeric(substr(ssp, 4, 4)), na.rm=TRUE), by=.(ssp)]
  
  
  # Problems with 2030 - SSP5
  y <- y[ssp != "SSP5" | y != 2030]
  
  #
  #jpgfile <- paste0(deparse(substitute(data2plot)), par2plot, ".jpg")
  jpgfile <- paste0(par2plot, ".jpg")
  cat("\n", jpgfile)
  jpeg(filename = jpgfile, width = 1150, height = 700, res = 130);
  
  
  allruns <- unique(y$run)
  if(! 'baseline' %in% allruns){
    
    if(is.null(refrun)){
      refrun <- allruns[1]
    }
    y[run == refrun]$run <- 'baseline'
  }
  
  
  save(list=objects(), file="x2.rdata")
  ybas <- y[run=="baseline", value]
  
  allyears <- unique(y$y)
  if(length(ybas) > 1){
    # Use different ybas values
    if(length(ybas) == length(allyears)){
      for(ay in 1:length(allyears)){
        y <- y[y==allyears[ay], valreg := value / ybas[ay]]
      }
    }
  }else{
    y[, valreg := value / ybas]
    y[run=="baseline", "valreg"] <- ybas
    valbaseline <- y[run=="baseline", valreg]
    
  }
  
  yrnd <- roundO(ybas)[[2]]
  ydiv <- roundO(ybas)[[1]]
  cat("\nyrnd=",yrnd," ydiv=",ydiv)
  if(ydiv > 1){
    yfac <- bquote(paste(10^.(log10(ydiv)), " "))
  }else(
    yfac <- bquote(paste(""))
  )
  ybas <- round(ybas/ydiv, yrnd) 
  write.csv(dcast.data.table(y, y ~ ssp + run, value.var = "valreg"),
            file = gsub("rdata", "csv", f(curfoc = curfoc, filen = filen, nscens = nscens)))
  
  #Correct the values to the order of magnitude
  y[, value := round(value/ydiv, yrnd)]
  
  
  # Exchange one color with black (baseline)
  #my.cols <- brewer.pal(length(unique(y$run)), "Blues")
  my.cols <- heat.colors(n = length(unique(y$run)), alpha = 0.8)
  cat("\nlenght(allruns)=", length(allruns))
  if(length(allruns)<=2){
    my.cols <- "blue"
  }else{
    my.cols <- rainbow(n=length(allruns), s=1, v = 1, start=0.5)
  }
  
  #my.cols[1] <- "#000000"
  
  #View(y)
  
  #y <- y[, ssp:= paste0(ssp, valbaseline)]
  #
  #y$run <- factor(y$run, levels = c("reference", "eatdiet", "eatmagpie", "envplus", "envuniform"))
  y <- y[run!="baseline"]
  
  if(! is.null(keepruns)){
    # Selects 'run's and/or orders them as wished in the argument
    #keepruns <- c("reference", "eatdiet", "eatmagpie", "envplus")
    y$run <- factor(y$run, levels = keepruns)
  }
  y <- y[! is.na(run)]
  save(list=objects(), file="x3.rdata")
  
  b<-ggplot(y, aes_string(x="y", fill = "run"))
  #b <- b + geom_bar(position = 'stack', stat='identity', width = 0.8) 
  b <- b + geom_bar(aes_string(y="valreg", fill = "run"),
                    position = position_dodge(), stat='identity', 
                    width = 0.8, alpha = 0.8, show.legend = TRUE) 
  b <- b + scale_fill_manual(values = my.cols)
  b <- b + ylab(expression("Value relative to reference [fraction]"))
  b <- b + xlab("")
  
  #b <- b + xlab("Countries") 
  
  if(is.null(punit)){
    punit <- ""
  }
  
  b <- b + ggtitle(bquote(paste("Relative Changes of total ", .(tit))), 
                   #expression("Baseline value = " ~ ybas ~ ""^3 ~ punit))
                   bquote(paste("Reference = ", .(ybas),
                                " [", .(yfac), .(punit), "]")))
  b <- b + theme(plot.title=element_text(hjust = 0.5), 
                 plot.subtitle=element_text(hjust = 0.5))
  
  
  #b <- b + facet_grid(. ~ ssp, scales = "free_y", space = "free")
  #b <- b + facet_wrap(. ~ ssp, scales = "free")
  b <- b + facet_wrap(. ~ ssp)
  
  #b + geom_point(y, mapping = aes_string(y="valhalf"),
  #               position=position_dodge(width = 0.8), shape = y$sspn)
  #b <- b + scale_y_continuous(limits = c(0,valregmax+0.2))
  valregmax <- max(1.5, y[run!="baseline", valreg])
  b <- b + scale_y_continuous(limits = c(0, valregmax), breaks = seq(0, valregmax, 0.2))
  b <- b + geom_hline(yintercept = c(0.5, 1), color = "red")
  b <- b + geom_text(
    mapping = aes_string(y=(valregmax-0.02), label="value"),
    angle = 90, vjust=0.5, hjust=1,
    size=3, color = "black",
    position=position_dodge(width=0.8))
  
  print(b)
  dev.off()
  
  return(b)
  
  
}
