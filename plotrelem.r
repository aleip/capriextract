## plotrelem ==> Plots total values of group of countries 
##               relative to Reference scenario.
##               The plots are per SSP and year,
##               with different scenarios as columns.
##               
##               Works only with summable variables! 
## 
## Required input:
## CAPRI data table with required columns:
## - rall: Regions
## - y: years
## - cols: cols  --> Note by default 
## - rows: rows 
## - ssp: SSP 
## - run: defining sub-scenario per ssp


f <- function(curfoc, filen='20190311_scenarioforum', nscens=24){
  paste0(cenv$resdir, "/capmod/", filen, "/", filen, "SSPs", 
         curfoc, "_", nscens, "scens.rdata")
}
roundO <- function(a){
  
  # Divide by 1000 for values between 10000 and 1000000
  div <- 10**(3*floor((log10(abs(a))-1) /3))
  
  # Round to full numbers for 100 adn 1000
  divround <- 0
  if(floor(log10(abs(a))) %% 3 == 1) divround <- 1
  
  n <- round(a/div, divround)
  return(list(div, divround))
  
}

plotrelem <- function(xorig = NULL,             # Passes data table. If NULL, the file filen is loaded
                      filen = "",
                      curfoc = "",
                      refrun = NULL,
                      filternuts0 = NULL,
                      swapcols = FALSE,
                      sumcols = NULL,
                      keepruns = NULL,
                      par2plot="GCH4",
                      currows = "",
                      curempty = "",
                      tit=NULL,
                      punit=NULL,
                      add2filename="",
                      plothere=FALSE){
  
  x <- copy(xorig)
  save(x, filen, curfoc, refrun, filternuts0, 
       swapcols, sumcols, keepruns, par2plot, 
       currows, curempty, tit, punit, add2filename, 
       plothere, file="plotrelem_args.rdata")
  #Default settings
  resetdefault <- FALSE
  if(resetdefault){
    curfoc = "";refrun = NULL;filternuts0 = TRUE;swapcols = FALSE;
    sumcols = NULL;keepruns = NULL;
    par2plot="GCH4";tit=NULL; punit=NULL;add2filename=""
  }
  library(RColorBrewer)
  
  #clean up 
  #if(exists("b")) {rm(b)}
  
  if(is.null(x)){
    filename <- f(curfoc = curfoc, filen = filen, nscens = nscens)
    cat("\n", filename)
    load(filename)
  }
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
  
  if (currows != ""){ x <- x[rows == currows] }
  x <- x[empty == curempty]
  save(x, sumcols, swapcols, curfoc, par2plot, file="x1.rdata")
  #return(list(x, currows))
  # See http://sape.inf.usi.ch/quick-reference/ggplot2/shape
  #     for using characters as shape
  # See https://stackoverflow.com/questions/31315112/add-points-to-grouped-bar-plot-ggplot2
  #     for adding points to barplots
  if(! is.null(filternuts0)){
    if(filternuts0=="NUTS0"){
      y <- x[cols == par2plot & rall %in% s$nuts0[1:27]]
    }else{
      y <- x[cols == par2plot & rall == filternuts0]
    }
  }else{
    y <- x[cols == par2plot]
  }
  y <- y[ , .(value = sum(value)), by = .(y, ssp, run)]
  
  # Adding columns for character and position of 'points'
  y[, value := signif(value, 3)]

  # The characters 50 ff indicate the numbers 1ff
  # The characters 98 ff indicate small alphabet charaters
  nonsspnames <- c("baseline")
  y[! ssp %in% nonsspnames , sspn := sum(49, as.numeric(substr(ssp, 4, 4)), na.rm=TRUE), by=.(ssp)]
  y[ssp == "baseline", sspn := 98] #b
  
  
  # Problems with 2030 - SSP5
  #y <- y[ssp != "SSP5" | y != 2030]
  #print(y)
  
  #
  allruns <- unique(y$run)
  if(! 'baseline' %in% allruns){
    
    #if(is.null(refrun)){
    #  refrun <- allruns[1]
    #}
    #y[run == refrun]$run <- 'baseline'
    ym <- max(y$value)
    y[value==ym, run:="baseline"]
  }
  
  
  save(x, y, allruns, sumcols, swapcols, curfoc, par2plot, file="x1.rdata")
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
  
  
  yrnd <- sapply(ybas, function(x) roundO(x)[[2]])[1]
  ydiv <- sapply(ybas, function(x) roundO(x)[[1]])[1]
  #cat("\nyrnd=",yrnd," ydiv=",ydiv)
  if(ydiv > 1){
    yfac <- bquote(paste(10^.(log10(ydiv)), " "))
  }else(
    yfac <- bquote(paste(""))
  )
  
  ybas <- round(ybas/ydiv, yrnd) 
  if(! exists("nscens")) nscens <- ""
  if(filen != ""){
    write.csv(dcast.data.table(y, y ~ ssp + run, value.var = "valreg"),
            file = gsub("rdata", "csv", f(curfoc = curfoc, filen = filen, nscens = nscens)))
  }
  #Correct the values to the order of magnitude
  y[, value := round(value/ydiv, yrnd)]
  #print(y)
  
  # Exchange one color with black (baseline)
  #my.cols <- brewer.pal(length(unique(y$run)), "Blues")
  my.cols <- heat.colors(n = length(unique(y$run)), alpha = 0.8)
  #cat("\nlenght(allruns)=", length(allruns))
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
  #print(y)
  save(list=objects(), file="x3.rdata")
  
  b<-ggplot(y, aes_string(x="y", fill = "run"))
  b <- b + geom_bar(aes_string(y="valreg", fill = "run"),
                    position = position_dodge(), stat='identity',
                    width = 0.8, alpha = 0.8, show.legend = TRUE)
  b <- b + scale_fill_manual(values = my.cols)
  b <- b + ylab(expression("Value relative to reference [fraction]"))
  b <- b + xlab("")

  #b <- b + xlab("Countries")

  if(is.null(punit)){
    punit <- ""
  }else{
    if(punit == "kt N yr-1") punit <- bquote(paste("kt N yr"^.(-1), " "))
    if(punit == "t N yr-1") punit <- bquote(paste("t N yr"^.(-1), " "))
    
  }
  refunittext <-  grepl("10", paste(yfac, collapse="")) | punit != "" 

  if(is.null(tit)){
    tit <- par2plot
    if(currows != "") tit <- paste0(tit, " - ", currows)
    if(curempty != "") tit <- paste0(tit, " - ", curempty)
    
  }
  
  if(refunittext) {
    b <- b + ggtitle(bquote(paste("Relative Changes of total ", .(tit))),
                     bquote(paste("Reference = ", .(ybas), " [", .(yfac), .(punit), "]")))
  }else{
    b <- b + ggtitle(bquote(paste("Relative Changes of total ", .(tit))),
                     bquote(paste("Reference = ", .(ybas))))
  }
  y[ssp == "baseline", ssp := "SSP0"]
  b <- b + theme(plot.title=element_text(hjust = 0.5),
                 plot.subtitle=element_text(hjust = 0.5))
  b <- b + facet_wrap(. ~ ssp, ncol = 2)

  valregmax <- max(1.5, y[run!="baseline", valreg])
  b <- b + scale_y_continuous(limits = c(0, valregmax), breaks = seq(0, valregmax, 0.2))
  b <- b + geom_hline(yintercept = c(0.5, 1), color = "red")
  b <- b + geom_text(
    mapping = aes_string(y=(valregmax-0.02), label="value"),
    angle = 90, vjust=0.5, hjust=1,
    size=2.5, color = "black",
    position=position_dodge(width=0.8))
  
  #jpgfile <- paste0(deparse(substitute(data2plot)), par2plot, ".jpg")
  parpart <- par2plot
  if(currows != "") parpart <- paste0(parpart, "_", currows)
  if(curempty != "") parpart <- paste0(parpart, "_", curempty)
  jpgfile <- paste0(add2filename, parpart, ".jpg")
  #cat("\n", jpgfile)
  jpeg(filename = jpgfile, width = 1150, height = 700, res = 130, quality = 500);
  print(b)
  dev.off()
  
  if(plothere){
    print(b)
  }
  
  #return(c)
  
  
}
