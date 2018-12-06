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

plotbars <- function(x = capri, emb = 'unique', plotdef=plotdef
){
  
  info<-checkinfo(x)
  if(info[[1]]=='act'){
    onx<-'RALL'
    ony<-'COLS'
  }else if(info[[1]]=='com'){
    onx<-'RALL'
    ony<-'ROWS'
  }
  numoflevles<-nrow(unique(x[,.SD, .SDcols=ony]))
  
  a<-ggplot(x, aes_string(x=onx, y="VALUE", fill=ony)) 
  
  a<- a + geom_bar(position = 'stack', stat='identity', width = 0.8) +
    ylab(info[[2]])
  if(emb=='unique') a <- a + xlab("Countries") + ggtitle(info[[3]])
  if(emb!='unique') a <- a + xlab("")
  a <- a + scale_y_continuous(limits = c(0,plotdef[ , 'ymax']))
  a <- a + filltheme(a, plotdef)
  a <- a + fillscale(a, emb, plotdef)
  return(a)
}

filltheme <- function(a, plotdef){
  t <- theme(axis.text.x = element_text(face="bold", color="#993333", 
                                        size=9, angle=90, 
                                        vjust = 0.5, hjust=0),
             axis.text.y = element_text(face="bold", color="#993333", 
                                        size=9, angle=0, 
                                        vjust = 0.5, hjust=1),
             axis.title = element_text(size=10),
             legend.text = element_text(size=9)
  )
  return(t)
}

fillscale <- function(a, emb, plotdef){
  if(emb!='unique'){legcols<-2}else{legcols<-NULL}
  t <- discrete_scale(aesthetics='fill', 
                      scale_name='',
                      palette = colorRampPalette(c('skyblue','darkblue','pink','yellow','darkred')),
                      name = info[[4]],
                      guide=guide_legend(ncol=legcols,
                                         override.aes = list(size = 1)))
  if(plotname=='box'){
    t <- discrete_scale(aesthetics='fill', 
                        scale_name='',
                        palette = colorRampPalette(c('skyblue','darkblue','pink','yellow','darkred')),
                        name = info[[4]],
                        guide=guide_legend(row=1,
                                           override.aes = list(size = 1)))
    
  }
}

plotboxes <- function(x = capri, emb = 'unique', plotname){
  
  #x<-capri[COLS%in%curcols & ROWS%in%mcact & RALL %in% nuts0eu15]
  info<-checkinfo(x)
  a<-ggplot(x, aes_string(x="COLS", y="VALUE")) 
  a<- a + geom_boxplot(aes_string(fill="COLS"))
  a<- a + filltheme(a)
  if(plotname%in%c("cropyild")){
    a<-a+guides(fill=FALSE)
  }else{
    a<- a + fillscale(a, emb)
  }
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

setuppage <- function(x, plotname=''){
  
  # Read plot characteristics
  plotdef<-read.table("capriplotdefaults.txt", header = TRUE)
  plotdef<-plotdef[plotdef$Plotname==plotname,]
  
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
  #nplots<-5;ncol<-1
  pdf(file = paste0(plotname,"_",paste(scendesc, collapse="-"),".pdf"), 
      onefile = TRUE,
      width = w, 
      height = w *plotdef[,'hwratio']
  )
  save(x, file='test.rdata')
  cat(scendesc)
  if (numscen > 1){
    # Adjust scales
    ymax <- vector()
    print(v2plot)
    for (i in 1:nplots){
      if(loopover=='c') ymax[i]<-max(x[COLS==v2plot[i], 'VALUE'])
      if(loopover=='w') ymax[i]<-max(x[ROWS==v2plot[i], 'VALUE'])
      if(loopover=='r') ymax[i]<-max(x[COLS==v2plot[i], 'VALUE'])
    }
  }
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
        if(plotdef[,'t2plot']=='bar') {p[[j]]<-plotbars(y, emb='multiple', plotdef)}
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
  return(a)
}


checkinfo<-function(x){
  
  #' Checks CAPRI information in data frame/table
  #' 
  #' @description Analyses data on content and returns
  #' information as a string to be plotted as label  
  
  perspective<-''
  curtit<-''
  legtit<-''
  
  curcols<-as.character(unique(x$COLS))
  numcols<-length(curcols)
  
  currows<-as.character(unique(x$ROWS))
  numrows<-length(currows)
  
  curequal<-sum(x$ROWS==x$COLS)
  print(currows)
  print(curcols)
  curlab<-"not yet defined in fuction checkinfo()"
  if(numrows==1 & numcols>1){
    perspective<-'act'
    if(currows=='LEVL'){
      if(sum(! curcols %in% mcact) == 0 ){
        # All activities are crops
        curlab<-"Area [1000 ha]"
        curtit<-"Total agricultural area by crop"
        legtit<-"CROPS"
      }
    }
    if(currows%in%c("N_CAL", "N_FAT", "N_PRO")){
      print("nutrients")
      if(currows=="N_CAL") balterm<-"Calorie intake\n[kcal cap-1 day-1]"
      if(currows=="N_FAT") balterm<-"Fat intake\n[g fat cap-1 day-1]"
      if(currows=="N_PRO") balterm<-"Protein intake\n[g proteins cap-1 day-1]"
      #cat("\n",perspective, balterm)
      curlab<-balterm
      curtit<-"Nutrient intake"
      legtit<-"Food primary commodities"
    }
    
  }else if(numrows>1 & numcols==1){
    perspective<-'com'
    if(curcols%in%c("GROF", "FEDM", "HCOM", "IMPT", "EXPT")){
      if(curcols=="GROF") balterm<-"Gross production"
      if(curcols=="IMPT") balterm<-"Import"
      if(curcols=="EXPT") balterm<-"Export"
      if(curcols=="FEDM") balterm<-"Feed use"
      if(curcols=="HCOM") balterm<-"Human consumption"
      #cat("\n",perspective, balterm)
      if(sum(! currows %in% mcact) == 0 ){
        # All products are crops
        curlab<-paste0(balterm, "\n [1000 t]")
        curtit<-"Total quantity by crop"
        legtit<-"CROPS"
      }  
    }
    if(curcols%in%c("N_CAL", "N_FAT", "N_PRO")){
      print("nutrients")
      if(curcols=="N_CAL") balterm<-"Calorie intake\n[Mcal yr-1]"
      if(curcols=="N_FAT") balterm<-"Fat intake\n[kg fat yr-1]"
      if(curcols=="N_PRO") balterm<-"Protein intake\n[kg proteins yr-1]"
      #cat("\n",perspective, balterm)
      curlab<-balterm
      curtit<-"Nutrient intake"
      legtit<-"Food primary commodities"
    }
  }else if(curequal>0){
    #This happens only for yield
    perspective<-'act'
    curlab<-"Crop yield \n[kg ha-1 yr-1]"
    curtit<-"Crop yield"
    legtit<-"CROPS"
  }
  
  # Check regional level
  curregs<-as.character(unique(x$RALL))
  nregs<-length(curregs)
  nnuts0<-length(grepl("000000",curregs))
  if(nregs==nnuts0){
    # Plot at country level - use 2 digits
    x<-x[, RALL := substr(RALL,1,2) ]
    
  }
  
  if(perspective=='') {cat("\nperspective missing\ncurrows=", currows,
                           "\n curcols=", curcols)}
  return(list(perspective, curlab, curtit, legtit))
}
