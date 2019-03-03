#' CAPRI barplot
#' 
#' @description This function makes a barplot based on CAPRI data.
#' By default, it uses a stacked plot with countries on x-axis.
#' Note that CAPRI data have to be loaded with the function opendata(), 
#' or with filteropen() or mutliplefilter() which use the opendata()
#' function recursively.
#' By default, the following columns are expected: 
#' - rall: units (countries, Nuts2, HSU, ...)
#' - cols: activities or variables for products
#' - rows: products or variables for activities
#' - y: years
#' - scen: identification of dataset if combined (e.g. for capmod)
#' - empty: additional dimensions used for CAPMOD with various content
#' - value: data
#' 
#' 
#' @param x CAPRI data frame or data table
#' @param perspective Defines if activity ('act') or 
#'                               commodity ('com') should be plotted
#' @param variable Defines which variable to use
#'        - levl = Levl (Area, Heads, ...)
#'        - grof = Gross production
#' @return data frame
#' @export

plotbars <- function(x = capri, emb = 'unique', 
                     plotdef = plotdef,
                     colbar = NULL   # If a vector with colors with the correct number of colors has been pre-defined
                     , colbar_args   # vector with colors to be passed to colbar
                     , col_map       # named vector with color map (with crops)
){
  
  if(!exists("datapath")) datapath <- cenv$resdir
  #print(plotdef$arr)
  #uniquex <- unique(x[ , .SD, .SDcols=plotdef$onx])
  uniquex <- as.vector(unique(x[[plotdef$onx]]))
  #uniquez <- unique(x[ , .SD, .SDcols=plotdef$onz])
  uniquez <- as.vector(unique(x[[plotdef$onz]]))
  #cat("\n uniquex\n", uniquex)
  #cat("\n uniquez\n", uniquez)
  if( length(uniquez) == 1 ) { 
    plotdef[ ,'curpanel'] <- uniquez}else{
      plotdef[ ,'curpanel'] <- uniquex  }
  #cat("\nthe current con\n", paste(plotdef$curpanel, collapse=""), "\n")
  #print(plotdef)
  
  plotdef<-checkinfo(x, plotdef)
  
  #cat("\n onx and ony", onx, ony, plotdef$curtit)
  
  numoflevles<-nrow(unique(x[,.SD, .SDcols=plotdef$ony]))
  save(x, plotdef, file=paste0(datapath, "x.rdata"))
  #a<-ggplot(x, aes_string(x=plotdef$onx, y="value", fill=plotdef$ony)) 
  a<-ggplot(x, aes_string(x=plotdef$onx, y="value", fill = factor(x[[plotdef$ony]], levels = (names(unlist(col_map)))))) 
  
  a<- a + geom_bar(position = 'stack', stat='identity', width = 0.8) +
    ylab(plotdef$curlab)
  if(emb=='unique') a <- a + xlab("Countries") + ggtitle(plotdef$curtit)
  if(emb!='unique') a <- a + xlab("")
  a <- a + scale_y_continuous(limits = c(0,plotdef[ , 'ymax']))
  a <- a + filltheme(a, plotdef)
  
  if(is.null(colbar)) {
    colbar <- colorRampPalette(c('skyblue','darkblue','pink','yellow','darkred'))
  }else{
    if(is.function(colbar)) colbar <- colbar
  }
  a <- a + fillscale(a, emb, plotdef, colbar, colbar_args, col_map)
  #a <- a + scale_fill_manual(values = colbar(33))
  return(a)
}

plotboxes <- function(x = capri, emb = 'unique', plotdef=plotdef){
  
  #x<-capri[cols%in%curcols & rows%in%mcact & rall %in% nuts0eu15]
  uniquex <- unique(x[ , .SD, .SDcols=plotdef$onx])
  uniquey <- unique(x[ , .SD, .SDcols=plotdef$ony])
  uniquez <- unique(x[ , .SD, .SDcols=plotdef$onz])
  #cat("\n uniquex", plotdef$onx, "\n"); print(uniquex)
  #cat("\n uniquey", plotdef$ony, "\n"); print(uniquey)
  #cat("\n uniquez", plotdef$onz, "\n"); print(uniquez)
  #if( length(uniquez) == 1 ) { 
  #  plotdef[ ,'curpanel'] <- uniquez}else{
  #    plotdef[ ,'curpanel'] <- uniquex  }
  plotdef[, 'curpanel'] <- plotdef$Plotname
  plotdef<-checkinfo(x, plotdef)
  a<-ggplot(x, aes_string(x="cols", y="value")) 
  a<- a + geom_boxplot(aes_string(fill="cols"))
  a <- a + xlab("Crops") + ggtitle(plotdef$curtit)
  a <- a + ylab(plotdef$curlab)
  a <- a + filltheme(a)
  if(plotdef$Plotname%in%c("cropyild", "scropyild", "cpri")){
    a<-a+guides(fill=FALSE)
  }else{
    a<- a + fillscale(a, emb)
  }
}


filltheme <- function(a, plotdef){
  t <- theme(axis.text.x = element_text(face="bold", color="#993333", 
                                        size=9, angle=90, 
                                        vjust = 0.5, hjust=0),
             axis.text.y = element_text(face="bold", color="#993333", 
                                        size=9, angle=0, 
                                        vjust = 0.5, hjust=1
                                        #margin = c(0, r=10, 0, 0, unit='pt')
             ),
             axis.title = element_text(size=10),
             legend.text = element_text(size=9)
  )
  return(t)
}

fillscale <- function(a, emb, plotdef, colbar=NULL, colbar_args, col_map){
  if(emb!='unique'){legcols<-2}else{legcols<-NULL}
  cat(plotdef$curleg)
  if(is.null(colbar)) {
    colbar<-colorRampPalette(c('skyblue','darkblue','pink','yellow','darkred'))   
  }else{
    colbar <- colorRampPalette(unlist(colbar_args))
  }
  t <- discrete_scale(aesthetics='fill', 
                      scale_name='',
                      palette = colbar,
                      name = plotdef$curleg,
                      limits = names(unlist(col_map)),
                      guide=guide_legend(ncol=legcols,
                                         override.aes = list(size = 1)))
  
  if(plotdef$Plotname=='box'){
    t <- discrete_scale(aesthetics='fill', 
                        scale_name='',
                        #palette = colorRampPalette(c('skyblue','darkblue','pink','yellow','darkred')),
                        name = plotdef$curleg,
                        guide=guide_legend(row=1,
                                           override.aes = list(size = 1)))
    
  }
  return(t)
}

exportdata <- function(x = NULL, refdesc = 'ref', flag=''){
  
  cat("\nExporting data ...")
  # refdesc: required if one certain run shall be used as the reference.
  #            if none is given then the run with the scenario description 'ref' is used as reference
  #            if none is given and 'ref' is not in the scenario descriptions, the first is used
  
  if(is.null(x)){
    message("Please provide a dataset to export.")
    invisible()
  }
  commonname <- ''
  # if(! exists("commonname")){
  #   commonname <- substr(info[[2]][1], 1, 20)
  # }
  nscen <- ncol(x)
  nelem <-unlist(lapply(1:nscen, function(y) length(unlist(unique(x[, .SD, .SDcols=names(x)[y]])))))
  
  xcols <- unique(x$cols)
  xrows <- unique(x$rows)
  if(plotdef$Plotname=='gwpt') plotdef$arr <- 'rcw'
  
  if(as.character(plotdef$arr) == 'rwc') {
    xextra <- xcols
    xcomm <- xrows
    xset <- plotdef$rows
  }else if(plotdef$arr == 'rcw') {
    xextra <- xrows
    xcomm <- xcols
    xset <- plotdef$cols
  }else if(plotdef$arr == 'crw') {
    if(plotdef$Plotname == 'cropyild') { 
      xextra <- "YILD"
    }else{
      xextra <- xrows
    }
    xcomm <- xcols
    xset <- plotdef$cols
  }else{
    stop(paste0("Not defined for plotdef$arr = ", plotdef$arr))
  }
  
  nrows <- length(xcomm)
  if(nrows > 1) xcomm<-xset
  
  ## Check reference scenario
  existref <- refdesc %in% unique(x$scen)
  
  scenshort <- unique(x$scen)
  filenames  <- unique(x$run)
  if(!existref) {
    refdesc <- scenshort[1]
  }
  #cat("\n",as.character(scenshort[1]), "\n")
  xy <- x[scen == refdesc, scen:='ref'] 
  xy <- xy[! duplicated(xy[,1:6])]
  scenshort <- scenshort[scenshort != refdesc]
  cat("existref=", existref, refdesc)
  #View(xy)
  #cat("\nscenshort=\n", paste(as.character(scenshort), collapse="\n"))
  save(x, xy, refdesc, scenshort, xcomm, file="xy.rdata")
  for(c in 1:length(xextra)){
    
    curcol <- xextra[c]
    
    anames <- paste0("A", c(1:length(scenshort)))
    
    #View(xy)
    # Year included in scen
    form1 <- formula("rall + cols + rows ~ scen")
    form2rwc <- formula("rall + cols ~ rows + scen")
    form2rcw <- formula("rall + rows ~ cols + scen")
    form2yild <- formula("rall + cols ~ scen")
    
    if(plotdef$arr == 'rwc') z <- dcast.data.table(xy[cols==curcol], form1, value.var = "value", sum)
    if(plotdef$arr == 'rcw') z <- dcast.data.table(xy[rows==curcol], form1, value.var = "value")
    if(curcol == 'YILD') z <- dcast.data.table(xy, form1, value.var="value")
    setnames(z, scenshort, anames)
    z <- z[, as.vector(anames) := .SD / ref, .SDcols = anames]
    #cat("\n",paste(c(refdesc, "anames=", anames), collapse="\n"))
    #View(z)
    z1 <- melt.data.table(z, measure.vars = c('ref', anames), 
                          variable.name = "scen", 
                          value.name = "value")
    if(plotdef$arr == 'rwc') z2 <- dcast.data.table(z1, form2rwc, value.var = "value") 
    if(plotdef$arr == 'rcw') z2 <- dcast.data.table(z1, form2rcw, value.var = "value") 
    if(curcol == 'YILD') z2 <- dcast.data.table(z1, form2yild, value.var="value")
    
    if(! grepl("//$", cenv$datapath)) datapath <- paste0(cenv$datapath, "/")
    con <- file(paste0(datapath, flag, "SSPs", plotdef$Plotname, 
                       substr(commonname,1,10), "_", xcomm, "-",curcol, ".csv"), open = "wt")
    cat("# ", file = con)
    cat("# Full name: ", commonname, file = con)
    cat("\n# cols = ", as.character(curcol), file = con)
    cat("\n# Data source:", file = con)
    
    #info$nrow <- "# "
    #write.csv(info, row.names = FALSE, quote = FALSE, file=con)
    #cat("\n# - ", paste(names(info), collapse = " - "), file = con)
    for(i in 1:length(filenames)){
      cat("\n", paste(scenshort[i], ": ", filenames[i], collapse = " - "), file = con)
    }
    
    cat("\n# ref = Reference scenario - absolute values", file = con)
    for(i in 1:length(scenshort)){
      cat("\n# ", anames[i], " = ", scenshort[i], file = con)
      cat(" - relative to REF", file=con)
      
    }
    cat("\n#\n", file = con)
    write.csv(z2, row.names = FALSE, quote = FALSE, file=con)
    close(con)
  }
  
  
}


calcstats<-function(x = capri, plotname){
  
  a<-desc_statby(as.data.frame(x), measure.var = "value", grps = "rows")
  a<-a[,c("rows", "length", "mean", "min", "max", "median", "sd", "range", "cv")]
  #print(a)
  a.p <- ggtexttable(t(format(a, digits = 2 ,scientific = FALSE)), 
                     rows = gsub("rows", "CROP", row.names(t(a))), 
                     theme = ttheme("default", base_size = 8 , 
                                    padding = unit(c(2, 2), "mm"))) 
  #a.p<-table_cell_font(a.p,size=8)
  
  
}

setuppage <- function(x, plotname='', filen=NULL, ddebug=0,   
                      omitplots = FALSE){
  
  # Include required functions
  source("capriplotcolors.r")
  source("capriplottexts.r")
  #xlsok <- require(xlsx, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(ggpubr)
  
  # Read plot characteristics
  plotdef<-read.table("capriplotdefaults.txt", header = TRUE)
  #ddebug <<- ddebug
  datapath <- cenv$datapath
  if(is.null(datapath)) datapath <- cenv$resdir
  if(is.null(datapath)) datapath <- "."
  datapath <- paste0(datapath, "/")
  #al201901 - don't put hard-wired paths! they always run into errors on other PCs
  #xavi 20190121: Yes, that's true. But then we should put a copy of capriplotdefaults.txt in the working directory. Or adding a 
  #               path in capri_dirs.r to where the file is
  #               If we keep the working directory in ~/capriextract, then all the plots are saved there. Alternatively, we could 
  #               include a path to where we want the plots to be saved
  #xavi 20190121: Finally, I have used 'datapath' from capri_dirs.r to define where the plots are saved, and kept the working 
  #               directory to ~/capriextract
  #plotdef<-read.table("E:\\capriextract/capriplotdefaults.txt", header = TRUE)
  plotdef<-plotdef[plotdef$Plotname==plotname,]
  if(ddebug==1) print(names(attributes(x)))
  if(substr(plotdef$arr,1,1)=='r') plotdef$onx <- 'rall'
  if(substr(plotdef$arr,1,1)=='c') plotdef$onx <- 'cols'
  if(substr(plotdef$arr,1,1)=='w') plotdef$onx <- 'rows'
  if(substr(plotdef$arr,2,2)=='r') plotdef$ony <- 'rall'
  if(substr(plotdef$arr,2,2)=='c') plotdef$ony <- 'cols'
  if(substr(plotdef$arr,2,2)=='w') plotdef$ony <- 'rows'
  if(substr(plotdef$arr,3,3)=='r') plotdef$onz <- 'rall'
  if(substr(plotdef$arr,3,3)=='c') plotdef$onz <- 'cols'
  if(substr(plotdef$arr,3,3)=='w') plotdef$onz <- 'rows'
  
  ### Filter data - the sets are given under 'rall' (rall), 'acts' (cols), and 'prod' (rows) 
  print(plotdef)
  plotdefrall<-plotdef[,'rall']
  if(! is.null(plotdefrall)){
    plotdefrall<-as.character(plotdefrall)
    if(! grepl(",",plotdefrall)){
      plotdefrall <- s[[plotdefrall]]
    }else{
      plotdefrall <- strsplit(as.character(plotdefrall),",")[[1]]
    }
    x<-x[rall %in% plotdefrall]
  }
  save(x, plotdef, file=paste0(datapath, 'test0.rdata'))
  plotdefcols<-plotdef[,'cols']
  if(! is.na(plotdefcols)){
    plotdefcols<-as.character(plotdefcols)
    if(! grepl(",",plotdefcols)){
      plotdefcols <- s[[plotdefcols]]
    }else{
      plotdefcols <- toupper(strsplit(as.character(plotdefcols),",")[[1]])
      x$cols <- toupper(x$cols)
    }
    if(ddebug==1) cat("\nplotdefcols=", plotdefcols, "\n")
    x<-x[cols %in% plotdefcols]
  }
  save(x, file=paste0(datapath, 'test1.rdata'))
  plotdefrows<-plotdef[,'rows']
  cat("\n", plotdefrows)
  if(plotdefrows=="ALL"){
    plotdefrows <- as.character(unique(x[cols%in%plotdefcols]$rows))
  }else if(! is.na(plotdefrows)){
    plotdefrows<-as.character(plotdefrows)
    #cat("\nFilter for ",plotdefrows," using ", grepl(",",plotdefrows))
    if(! grepl(",",plotdefrows)){
      plotdefrows <- s[[plotdefrows]]
    }else{
      #cat("\nFilter for ",plotdefrows, "using strsplit")
      plotdefrows <- strsplit(as.character(plotdefrows),",")[[1]]
    }
    x<-x[rows %in% as.character(plotdefrows)]
  }
  # In case that the selection of plotdefrows restricts what is available for plotdefcols
  plotdefcols <- unique(x$cols)
  save(x, file=paste0(datapath, 'test2.rdata'))
  
  ### Check arrangement of plots
  ### The third character in 'arr' determines the variable that changes with the plots
  loopover<-substr(plotdef[,'arr'],3,3)
  if(plotname%in%c('cropyild')) loopover<-"x" #No loop
  v2plot<-"x"
  if(loopover=='c') v2plot<-plotdefcols
  if(loopover=='w') v2plot<-plotdefrows
  if(loopover=='r') v2plot<-plotdefrall
  nplots<-length(v2plot)
  
  ### Check if multiple scenarios are stored - in this case loop over the scenarios
  cat(names(x))
  if("scen" %in% names(x)){
    if(ddebug==1) cat("\n#Scenario description availalbe")
    numscen<-length(unique(x$scen))
    scendesc<-unique(x$scen)
  }else{
    numscen<-1
    scendesc<-''
  }
  w<-11.7
  w<-20
  save(list=objects(), file=paste0(datapath, 'test3.rdata'))
  cat("\n", pdfname)
  cat("\n", rname)
  if(ddebug==1) cat("numscen=", numscen, scendesc)
  if (numscen > 0){
    # Adjust scales
    ymax <- vector()
    if(ddebug==1) print(v2plot)
    for (i in 1:nplots){
      if(loopover=='c') ymax[i]<-max(x[cols==v2plot[i], lapply(.SD, sum, na.rm=TRUE), .SDcols='value', by=c(eval(plotdef$onx), "scen")]$value)
      if(loopover=='w') ymax[i]<-max(x[rows==v2plot[i], lapply(.SD, sum, na.rm=TRUE), .SDcols='value', by=c(eval(plotdef$onx), "scen")]$value)
      if(loopover=='r') ymax[i]<-max(x[rall==v2plot[i], lapply(.SD, sum, na.rm=TRUE), .SDcols='value', by=c(eval(plotdef$onx), "scen")]$value)
    }
  }
  
  ####### temporary because Java does not work ###### 
  if(plotdef[,'t2plot']=='omit') omitplots <- TRUE
  cat("\nNow make unique")
  x <- x[! duplicated(x[,1:6])]
  xdcast <- dcast.data.table(x, rall + y + scen ~ cols + rows, value.var="value")
  write.csv(xdcast, file = gsub("rdata", "csv", rname))
  save(x, xdcast, file=rname)
  
  #if (! omitplots) 
  colbar<-setcolors(x, plotdef$ony)
  
  #colbar[1] is the function
  #colbar[2] is the vector with colors to be passed as arguments to the function
  #colbar[3] is a named vector with color map (with crops)
  if(! omitplots){
    #nplots<-5;ncol<-1
    pdfname <- datapath
    pdfname <- paste0(pdfname, filen, "SSPs", plotname,"_")
    scenname <- paste(scendesc, collapse="-")
    if(nchar(scenname)>20){scenname <- paste0(numscen, "scens")}  
    pdfname <- paste0(pdfname, scenname)
    pdfname <- paste0(pdfname, ".pdf")
    rname <- gsub("pdf", "rdata", pdfname)
    pdf(file = pdfname, 
        onefile = TRUE,
        width = w, 
        height = w *plotdef[,'hwratio']
    )
    for(scens in 1:numscen){
      plottit<-paste0(plotdef[,'title'])
      if(ddebug==1) cat("\n", scens, scendesc)
      if(scendesc[scens]!='')plottit<-paste0(plottit,": ",scendesc[scens], "\n")
      xscen<-x
      if("scen" %in% names(x)) xscen<-x[scen==scendesc[scens]]
      
      p<-list()
      if(ddebug==1) if(length(v2plot)==0) cat("\n No plots to do: ", as.character(v2plot))
      cat("\nScen", scens, "/", numscen, " for ",plotname, ". Plots to do: ", v2plot)
      
      j<-1
      for (i in 1:nplots){
        
        # Select data
        y<-xscen
        plotdef$ymax <- ymax[i]
        
        if(loopover=='c') y<-xscen[cols==v2plot[i]]
        if(loopover=='w') y<-xscen[rows==v2plot[i]]
        if(loopover=='r') y<-xscen[cols==v2plot[i]]
        if(nrow(y)>0 & omitplots!=1){
          #cat("\nPreparing plot ", i)
          if(plotdef[,'t2plot']=='bar') {p[[j]]<-plotbars(y, emb='multiple', plotdef, colbar=colbar[1], colbar_args = colbar[2], col_map = colbar[3])}
          if(plotdef[,'t2plot']=='box') {p[[j]]<-plotboxes(y, emb='multiple', plotdef)}
          if(plotdef[,'t2plot']=='box') {p[[j+1]]<-calcstats(y); j<-j+1}
        }
        j <- j +1
      }
      j<-j-1
      nrall<-length(unique(x$rall))
      ncol<-plotdef[,'ncol']
      nrow<-plotdef[,'nrow']
      if(is.na(nrow)) nrow<-ceiling(j/ncol)
      if(ddebug==1) cat("\n j=",j," ncol=",ncol," nrow=",nrow)
      
      cat("\nLength of p=", length(p))
      if(omitplots!=1){
        fig<-ggarrange(plotlist=p, 
                       common.legend=TRUE, 
                       #common.legend=FALSE, 
                       legend='right',
                       #labels = "AUTO",
                       nrow = nrow, ncol = ncol, align = 'v'
                       #widths = c(1,1)
        )
        figa<-annotate_figure(fig, 
                              top=text_grob(plottit,
                                            face='bold',
                                            color='darkgrey',
                                            size=20,
                                            lineheight=2))
        print(figa)
      }
      #ggsave(filename = paste0(v2plot,".pdf"), 
      #   plot = figa, device="pdf", 
      #        scale = 1, 
      #        width = w, 
      #        height = w * plotdef[,'hwratio'] * nplots / ncol
      #        units = "cm",
      #        dpi = 300
      #        )
      if(ddebug==1) cat("\nPrint ", scendesc[scens])
    }
    dev.off()
  }else{
    p <- x
  }
  
  ## Give 'x' back to global environment for further checks
  lastdataplotted <<- x
  plotdef <<- plotdef
  exportdata(lastdataplotted, flag=filen)
  return(p)
}

