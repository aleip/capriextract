checkinfo<-function(x, plotdef=plotdef){
  
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
  
  save(list=objects(), file="capriplottexts.rdata")
  
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
  }else if(numrows>1 & numcols==1){
    perspective<-'com'
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
  
  cat("\n check if curpanel is there:\n")
  print(plotdef)
  if(plotdef$Plotname %in% c('ncalinha')) plotdef$perspective<-'nutrition'
  if(plotdef$Plotname %in% c('ncalinha')) plotdef$curtit<-'Nutrient intake per Inhabitant'
  if(plotdef$Plotname %in% c('ncalinha')) plotdef$legtit<-'Food'

  if(plotdef$curpanel%in%c("GROF", "FEDM", "HCOM", "IMPT", "EXPT", "INHA")){
    if(plotdef$curpanel=="GROF") balterm<-"Gross production"
    if(plotdef$curpanel=="IMPT") balterm<-"Import"
    if(plotdef$curpanel=="EXPT") balterm<-"Export"
    if(plotdef$curpanel=="FEDM") balterm<-"Feed use"
    if(plotdef$curpanel=="HCOM") balterm<-"Human consumption"
    if(plotdef$curpanel=="INHA") balterm<-"Human consumption per capita"
    #cat("\n",perspective, balterm)
    if(sum(! currows %in% mcact) == 0 ){
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
