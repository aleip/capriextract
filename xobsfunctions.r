selecthsu<-function(reload=0, capridat=capridat, cols=curcols,
                    rows=currows, curcountry, curyear){
  if(reload==1){
    capridat<-opendata(scope,curcountry,curyear)
  }
  
  #COLS (activities, variables for products)
  if(!is.null(cols)) capridat<-capridat[capridat$COLS%in%cols,]
  
  #ROWS (products, variables for activities)
  if(!is.null(rows)) capridat<-capridat[capridat$ROWS%in%rows,]
  
  capridat$Y <- curyear
  
  return(capridat)
}

openxobstimeseries<-function(cols=curcols,
                             rows=currows, curcountries, 
                             baseyear, curyears){
  
  for(curcountry in curcountries){
    
    capridat<-opendata(scope = "capdiscapreg",curcountry = curcountry, curyear = baseyear)
    xobscountry<-selecthsu(reload = 0, capridat, curcols, currows, curcountry, baseyear)
    
    for(y in curyears){
      capridat<-opendata(scope = "capdistimes",curcountry = curcountry, curyear = y)
      xobscountry<-rbind(xobscountry, selecthsu(reload = 0, capridat, curcols, currows, curcountry, y))
    }
    
    xobscountry$NUTS2<-curcountry
    xobscountry<-select(xobscountry, NUTS2, RALL, COLS, ROWS, Y, VALUE)  
    
    if (which(curcountries==curcountry)==1){xobsa <- xobscountry}else{
      xobsa<-rbind(xobsa, xobscountry)
    }
  }
  
  xobsa<-dcast(xobsa, NUTS2 + RALL + COLS ~ Y, value.var = "VALUE")
  xobs<-filter(xobsa, grepl("^U", RALL))
  xobsnuts2<-filter(xobsa, ! grepl("^U", RALL))
  return(list(xobs, xobsnuts2))
  
}

xobsscatter<-function(xobs, mcact, curyears){
  
  crop<-"BARL"
  crop<-"SWHE"
  curc<-"IT20"
  
  graphics.off()
  jpeg(filename=paste0(datapath, "plots/", curc, ".jpg"), 
       width=2000, height=2000, res=300)
       
       
  par(mfrow=c(4, 5), ann=FALSE, mar=c(1.5,1.8,2,0))
  j<-1; k<-1
#  for(crop in c("SWHE", "BARL", "SUGB", "OFAR")){
  while(j <= 20){
    crop<-mcact[k]
    x<-unlist(select(filter(xobs, NUTS2==curc, COLS==crop), as.character(baseyear)))
    if(length(x) > 0){
      for(i in 1:length(curyears)){
        y<-unlist(select(filter(xobs, NUTS2==curc, COLS==crop), as.character(curyears[i])))
        #print(curyears[i])
        if(i==1){
          plot(x, y, col=blues9[4+i], type="p", pch=16, bg=blues9[4+i], cex=1)
          title(main=paste0(curc,": ",crop))
        }else{
          points(x, y, col=blues9[4+i], type="p", pch=16, bg=blues9[4+i], cex=1)
        }
      }
      j<-j+1
    }else{
      cat("no ", crop)
    }
    k<-k+1
    cat("\n",j, k, crop)
  }
  dev.off()
}


iniplot<-function(figname,nplots){

  # from eugirp_funnirplots.r
  
    graphics.off()
  # Define metrics of the plot ####
  # Note: default values have been set for a plot of the size 27.94 cm x 15.24 cm
  # Note: default values have been set for a plot of the size 11 in x 6 in
  
  # Settings that come from curplot.r in EU-GIRP
  plotresolution<-300
  plotformat<-'pdf'
  plotsdir<-paste0(datapath, "plots/")
  runfocus<-'xobsscatter'
  
  pwidth=27.94
  pwidth=16
  pheight=pwidth/1.833
  if(runfocus=="compare"){heightmult<-1.3}else{heightmult<-1}
  pheight=pheight*heightmult
  pconv<-pwidth/27.94
  plotresolution<-plotresolution
  bspace=0.1
  # Plotting barplot, first the bars in shading, then the left-and righthand pattern
  #df.bar<-barplot(eu28fin,yaxp=c(0,8000,2000),col=mycols)
  
  #cat("\nfigname: ",figname)
  if(plotformat=="pdf") pdf(file=figname,width=pwidth,height=pheight)
  if(plotformat=="png") png(file=gsub("pdf","png",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
  if(plotformat=="jpg") jpeg(file=gsub("pdf","jpg",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
  cat(gsub(plotsdir,"",figname),": ")
  # Parameters must be set afte defining graphic (?)
  par(mfrow = c(nplots,1))
  if(runfocus=="compare") par(mfrow=c(1,3))
  par(xpd=FALSE)
  
  #outer margin area
  #see http://rgraphics.limnology.wisc.edu/rmargins_sf.php
  if(runfocus=="range"){haslegend<-0}else{haslegend<-1}
  hasfootnote<-1
  hastitle<-1
  xstt=0.15
  if(haslegend==1){xleg=0.7}else{xleg=0.95}
  if (hasfootnote==1){ystt=0.10}else{ystt=0.05}
  if (hastitle==1){yhea=1-0.1/heightmult} else {yhea=1.0}
  
  # omd: outer margin as fraction of device region (in contrast: oma in lines of text)
  paromd<-c(xstt,xleg,ystt,yhea)
  if(runfocus=="compare") paromd<-c(0.05,1,0.15,yhea)
  par(omd=paromd)
  return(list(hastitle,haslegend,hasfootnote,pconv,paromd))
}
