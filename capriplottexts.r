checkinfo<-function(x, plotdef=plotdef){
  
  #' Checks CAPRI information in data frame/table
  #' 
  #' @description Analyses data on content and returns
  #' information as a string to be plotted as label  
  
  perspective<-''
  curtit<-''
  legtit<-''
  
  curcols<-as.character(unique(x$cols))
  numcols<-length(curcols)
  
  currows<-as.character(unique(x$rows))
  numrows<-length(currows)
  
  save(list=objects(), file="capriplottexts.rdata")
  
  curequal<-sum(x$rows==x$cols)
  #print(currows)
  #print(curcols)
  curlab<-"not yet defined in fuction checkinfo()"
  if(numrows==1 & numcols>1){
    perspective<-'act'
    if(currows=='LEVL'){
      if(sum(! curcols %in% mcact) == 0 ){
        # All activities are crops
        plotdef$curlab<-"Area [1000 ha]"
        plotdef$curtit<-"Total agricultural area by crop"
        plotdef$legtit<-"CROPS"
      }
    }
  }else if(numrows>1 & numcols==1){
    perspective<-'com'
    if(curcols%in%c("N_CAL", "N_FAT", "N_PRO")){
      #print("nutrients")
      if(curcols=="N_CAL") balterm<-"Calorie intake\n[Mcal yr-1]"
      if(curcols=="N_FAT") balterm<-"Fat intake\n[kg fat yr-1]"
      if(curcols=="N_PRO") balterm<-"Protein intake\n[kg proteins yr-1]"
      #cat("\n",perspective, balterm)
      plotdef$curlab<-balterm
      plotdef$curtit<-"Nutrient intake"
      plotdef$legtit<-"Food primary commodities"
    }
  }  
  # Check regional level
  curregs<-as.character(unique(x$rall))
  nregs<-length(curregs)
  nnuts0<-length(grepl("000000",curregs))
  if(nregs==nnuts0){
    # Plot at country level - use 2 digits
    x<-x[, RALL := substr(rall,1,2) ]
    
  }
  
  #cat("\n check if curpanel is there:\n")
  #print(plotdef)
  if(plotdef$Plotname %in% c('ncalinha')) plotdef$perspective<-'nutrition'
  if(plotdef$Plotname %in% c('ncalinha')) plotdef$curtit<-'Nutrient intake per Inhabitant'
  if(plotdef$Plotname %in% c('ncalinha')) plotdef$legtit<-'Food'
  if(plotdef$Plotname %in% c('cpri')) plotdef$perspective<-'prices'
  if(plotdef$Plotname %in% c('cpri')) plotdef$curtit<-'Consumer prices'
  if(plotdef$Plotname %in% c('cpri')) plotdef$legtit<-'Food'
  if(plotdef$Plotname %in% c('cpri')) plotdef$curlab<-expression("Prices [?? t"^-1 ~ "]")
  if(plotdef$Plotname %in% c('cropyild')) plotdef$perspective<-'act'
  if(plotdef$Plotname %in% c('cropyild')) plotdef$curlab<-expression("Crop yield [kg ha"^-1 ~ "yr"^-1 ~ "]")
  if(plotdef$Plotname %in% c('cropyild')) plotdef$curtit<-"Crop yield"
  if(plotdef$Plotname %in% c('cropyild')) plotdef$legtit<-"CROPS"
  
  if(plotdef$curpanel%in%c("GROF", "FEDM", "HCOM", "IMPT", "EXPT", "INDM", "PRCM", "INHA")){
    if(plotdef$curpanel=="GROF") balterm<-"Gross production"
    if(plotdef$curpanel=="IMPT") balterm<-"Import"
    if(plotdef$curpanel=="EXPT") balterm<-"Export"
    if(plotdef$curpanel=="FEDM") balterm<-"Feed use"
    if(plotdef$curpanel=="HCOM") balterm<-"Human consumption"
    if(plotdef$curpanel=="INDM") balterm<-"Use in industry"
    if(plotdef$curpanel=="PRCM") balterm<-"Processing for secondary products"
    if(plotdef$curpanel=="INHA") balterm<-"Human consumption per capita"
    #cat("\n",perspective, balterm)
    if(sum(! currows %in% s$mcact) == 0 ){
      # All products are crops
    }  
    plotdef$curtit<-"Total quantity by crop"
    plotdef$legtit<-"CROPS"
    plotdef$curlab<-paste0(balterm, "\n [1000 t]")
    if(plotdef$curpanel=="INHA") plotdef$curlab<-paste0(balterm, "\n [kg / cap / year]")
  }
  
  if(plotdef$curpanel == "N_CAL") 
    plotdef$curlab<-expression("Energy intake per person [kcal cap"^-1 ~ "day"^-1 ~ "]")
  if(plotdef$curpanel == "N_PRO") 
    plotdef$curlab<-expression("Protein intake per person [g  cap"^-1 ~ "day"^-1 ~ "]")
  if(plotdef$curpanel == "N_FAT") 
    plotdef$curlab<-expression("Fat intake per person [g cap"^-1 ~ "day"^-1 ~ "]")
  #return(plotdef, list(perspective, curlab, curtit, legtit))
  return(plotdef)
}
