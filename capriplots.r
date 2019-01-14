#' CAPRI barplot
#' 
#' @description This function makes a barplot based on CAPRI data.
#' By default, it uses a stacked plot with countries on x-axis.
#' Note that CAPRI data have to be loaded with the function opendata(), 
#' or with filteropen() or mutliplefilter() which use the opendata()
#' function recursively.
#' By default, the following columns are expected: 
#' - RALL: units (countries, Nuts2, HSU, ...)
#' - COLS: activities or variables for products
#' - ROWS: products or variables for activities
#' - Y: years
#' - SCEN: identification of dataset if combined (e.g. for capmod)
#' - EMPTY: additional dimensions used for CAPMOD with various content
#' - VALUE: data
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
  
  print(plotdef$arr)
  #uniquex <- unique(x[ , .SD, .SDcols=plotdef$onx])
  uniquex <- as.vector(unique(x[[plotdef$onx]]))
  #uniquez <- unique(x[ , .SD, .SDcols=plotdef$onz])
  uniquez <- as.vector(unique(x[[plotdef$onz]]))
  cat("\n uniquex\n", uniquex)
  cat("\n uniquez\n", uniquez)
  if( length(uniquez) == 1 ) { 
    plotdef[ ,'curpanel'] <- uniquez}else{
      plotdef[ ,'curpanel'] <- uniquex  }
  cat("\nthe current con\n", paste(plotdef$curpanel, collapse=""), "\n")
  print(plotdef)
  
  plotdef<-checkinfo(x, plotdef)
  
  #cat("\n onx and ony", onx, ony, plotdef$curtit)
  
  numoflevles<-nrow(unique(x[,.SD, .SDcols=plotdef$ony]))
  save(x, plotdef, file="x.rdata")
  #a<-ggplot(x, aes_string(x=plotdef$onx, y="VALUE", fill=plotdef$ony)) 
  a<-ggplot(x, aes_string(x=plotdef$onx, y="VALUE", fill = factor(x[[plotdef$ony]], levels = (names(unlist(col_map)))))) 
  
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
  
  #x<-capri[COLS%in%curcols & ROWS%in%mcact & RALL %in% nuts0eu15]
  uniquex <- unique(x[ , .SD, .SDcols=plotdef$onx])
  uniquey <- unique(x[ , .SD, .SDcols=plotdef$ony])
  uniquez <- unique(x[ , .SD, .SDcols=plotdef$onz])
  cat("\n uniquex", plotdef$onx, "\n"); print(uniquex)
  cat("\n uniquey", plotdef$ony, "\n"); print(uniquey)
  cat("\n uniquez", plotdef$onz, "\n"); print(uniquez)
  if( length(uniquez) == 1 ) { 
    plotdef[ ,'curpanel'] <- uniquez}else{
      plotdef[ ,'curpanel'] <- uniquex  }
  plotdef<-checkinfo(x, plotdef)
  a<-ggplot(x, aes_string(x="COLS", y="VALUE")) 
  a<- a + geom_boxplot(aes_string(fill="COLS"))
  a<- a + filltheme(a)
  if(plotdef$Plotname%in%c("cropyild", "scropyild")){
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


calcstats<-function(x = capri, plotname){
  
  a<-desc_statby(as.data.frame(x), measure.var = "VALUE", grps = "ROWS")
  a<-a[,c("ROWS", "length", "mean", "min", "max", "median", "sd", "range", "cv")]
  #print(a)
  a.p <- ggtexttable(t(format(a, digits = 2 ,scientific = FALSE)), 
                     rows = gsub("ROWS", "CROP", row.names(t(a))), 
                     theme = ttheme("default", base_size = 8 , 
                                    padding = unit(c(2, 2), "mm"))) 
  #a.p<-table_cell_font(a.p,size=8)
  
  
}

setuppage <- function(x, plotname='', info=info){
  
  # Read plot characteristics
  #plotdef<-read.table("capriplotdefaults.txt", header = TRUE)
  plotdef<-read.table("E:\\capriextract/capriplotdefaults.txt", header = TRUE)
  plotdef<-plotdef[plotdef$Plotname==plotname,]
  print(names(attributes(x)))
  if(substr(plotdef$arr,1,1)=='r') plotdef$onx <- 'RALL'
  if(substr(plotdef$arr,1,1)=='c') plotdef$onx <- 'COLS'
  if(substr(plotdef$arr,1,1)=='w') plotdef$onx <- 'ROWS'
  if(substr(plotdef$arr,2,2)=='r') plotdef$ony <- 'RALL'
  if(substr(plotdef$arr,2,2)=='c') plotdef$ony <- 'COLS'
  if(substr(plotdef$arr,2,2)=='w') plotdef$ony <- 'ROWS'
  if(substr(plotdef$arr,3,3)=='r') plotdef$onz <- 'RALL'
  if(substr(plotdef$arr,3,3)=='c') plotdef$onz <- 'COLS'
  if(substr(plotdef$arr,3,3)=='w') plotdef$onz <- 'ROWS'
  
  ### Filter data - the sets are given under 'rall' (RALL), 'acts' (COLS), and 'prod' (ROWS) 
  plotdefrall<-plotdef[,'rall']
  if(! is.na(plotdefrall)){
    plotdefrall<-as.character(plotdefrall)
    if(! grepl(",",plotdefrall)){
      plotdefrall <- get(plotdefrall)
    }else{
      plotdefrall <- strsplit(as.character(plotdefrall),",")[[1]]
    }
    x<-x[RALL %in% plotdefrall]
  }
  save(x, file='test0.rdata')
  plotdefcols<-plotdef[,'cols']
  if(! is.na(plotdefcols)){
    plotdefcols<-as.character(plotdefcols)
    if(! grepl(",",plotdefcols)){
      plotdefcols <- get(plotdefcols)
    }else{
      plotdefcols <- toupper(strsplit(as.character(plotdefcols),",")[[1]])
    }
    x<-x[COLS %in% plotdefcols]
  }
  save(x, file='test1.rdata')
  plotdefrows<-plotdef[,'rows']
  if(! is.na(plotdefrows)){
    plotdefrows<-as.character(plotdefrows)
    #cat("\nFilter for ",plotdefrows," using ", grepl(",",plotdefrows))
    if(! grepl(",",plotdefrows)){
      plotdefrows <- get(plotdefrows)
    }else{
      #cat("\nFilter for ",plotdefrows, "using strsplit")
      plotdefrows <- strsplit(as.character(plotdefrows),",")[[1]]
    }
    x<-x[ROWS %in% as.character(plotdefrows)]
  }
  save(x, file='test.rdata')
  
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
  if("SCEN" %in% names(x)){
    #Scenario description availalbe
    numscen<-length(unique(x$SCEN))
    scendesc<-unique(x$SCEN)
  }else{
    numscen<-1
    scendesc<-''
  }
  w<-11.7
  w<-20
  #nplots<-5;ncol<-1
  print(gsub(".gdx","",info$filename))
  pdf(file = paste0(gsub(".gdx","",info$filename), 
                    plotname,"_",paste(scendesc, collapse="-"),".pdf"), 
      onefile = TRUE,
      width = w, 
      height = w *plotdef[,'hwratio']
  )
  save(x, file='test.rdata')
  cat(numscen, scendesc)
  if (numscen > 0){
    # Adjust scales
    ymax <- vector()
    print(v2plot)
    for (i in 1:nplots){
      if(loopover=='c') ymax[i]<-max(x[COLS==v2plot[i], lapply(.SD, sum, na.rm=TRUE), .SDcols='VALUE', by=c(eval(plotdef$onx), "SCEN")]$VALUE)
      if(loopover=='w') ymax[i]<-max(x[ROWS==v2plot[i], lapply(.SD, sum, na.rm=TRUE), .SDcols='VALUE', by=c(eval(plotdef$onx), "SCEN")]$VALUE)
      if(loopover=='r') ymax[i]<-max(x[RALL==v2plot[i], lapply(.SD, sum, na.rm=TRUE), .SDcols='VALUE', by=c(eval(plotdef$onx), "SCEN")]$VALUE)
    }
  }
  
  colbar<-setcolors(x, plotdef$ony)
  #colbar[1] is the function
  #colbar[2] is the vector with colors to be passed as arguments to the function
  #colbar[3] is a named vector with color map (with crops)
  
  for(scens in 1:numscen){
    plottit<-paste0(plotdef[,'title'])
    if(scendesc[scens]!='')plottit<-paste0(plottit,": ",scendesc[scens], "\n")
    xscen<-x
    if("SCEN" %in% names(x)) xscen<-x[SCEN==scendesc[scens]]
    
    p<-list()
    if(length(v2plot)==0) cat("\n No plots to do: ", v2plot)
    cat("\n Plots to do: ", v2plot)
    
    j<-1
    for (i in 1:nplots){
      
      # Select data
      y<-xscen
      plotdef$ymax <- ymax[i]
      
      if(loopover=='c') y<-xscen[COLS==v2plot[i]]
      if(loopover=='w') y<-xscen[ROWS==v2plot[i]]
      if(loopover=='r') y<-xscen[COLS==v2plot[i]]
      if(nrow(y)>0){
        cat("\nPreparing plot ", i)
        if(plotdef[,'t2plot']=='bar') {p[[j]]<-plotbars(y, emb='multiple', plotdef, colbar=colbar[1], colbar_args = colbar[2], col_map = colbar[3])}
        if(plotdef[,'t2plot']=='box') {p[[j]]<-plotboxes(y, emb='multiple', plotdef)}
        if(plotdef[,'t2plot']=='box') {p[[j+1]]<-calcstats(y); j<-j+1}
      }
      j <- j +1
    }
    j<-j-1
    nrall<-length(unique(x$RALL))
    ncol<-plotdef[,'ncol']
    nrow<-plotdef[,'nrow']
    if(is.na(nrow)) nrow<-ceiling(j/ncol)
    cat("\n j=",j," ncol=",ncol," nrow=",nrow)
    
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
    
    #ggsave(filename = paste0(v2plot,".pdf"), 
     #   plot = figa, device="pdf", 
#        scale = 1, 
#        width = w, 
#        height = w * plotdef[,'hwratio'] * nplots / ncol
#        units = "cm",
#        dpi = 300
#        )
    cat("\nPrint ", scendesc[scens])
    print(figa)
  }
  dev.off()
  return(p)
}


