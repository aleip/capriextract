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

xobsscatter<-function(xobs, curcact, curyears, curcountries, add2name=""){
    for(i in 1:(length(curyears)-1)){if(curyears[1+i]=="_") curyears[1+i]<-"capdis"}
        
    for(curc in curcountries){
        
        cols <- c('red','gray40','blue','plum','#f6e8c3','#c7eae5','#5ab4ac','#01665e')
        graphics.off()
        jpeg(filename=paste0(datapath, "plots/", scope, curc, paste(curyears, collapse="_vs_"), add2name, ".jpg"), 
             width=2000, height=2000, res=300)
        
        par(mfrow=c(4, 5), ann=FALSE, mar=c(1.7,1.8,2,0), cex.axis=1, 
            mgp=c(1, 0.3, 0.3) # distances between plot and axis lables
            ) 
        if(length(curcact)<10)par(mfrow=c(3, 4))
        if(length(curcact)<4)par(mfrow=c(2, 2))
        j<-1; k<-1
        while(j <= min(19,length(curcact))){
            crop<-curcact[k]
            all<-select(filter(xobs, NUTS2==curc, COLS==crop), gsub("_","",curyears))
            x<-unlist(select(filter(xobs, NUTS2==curc, COLS==crop), as.character(gsub("_","",curyears[1]))))
            if(length(x) > 0){
                axmax<-max(all)
                for(i in 1:(length(curyears)-1)){
                    y<-unlist(select(filter(xobs, NUTS2==curc, COLS==crop), as.character(gsub("_","",curyears[1+i]))))
                    #print(curyears[i])
                    if(i==1){
                        plot(x, y, col=cols[1], type="p", pch=1, bg=cols[1], cex=0.6, xlim=c(0,axmax), ylim=c(0,axmax))
                        points(x[x==0], y[x==0], col=blues9[9], type="p", pch=1, bg=blues9[9], cex=0.3)
                        points(x[y==0], y[y==0], col="darkred", type="p", pch=1, bg="darkred", cex=0.3)
                        title(main=paste0(curc,": ",crop))
                    }else{
                        points(x, y, col=cols[i], type="p", pch=1, bg=cols[i], cex=0.6)
                        
                    }
                    if(length(y)>0) legend(x=-axmax*0.13,y=axmax*(1.12-i*0.07), cex=0.6,  bty="n", 
                               legend=paste(as.character(gsub("_","",curyears[1+i])),"=",format(summary(lm( y ~ x))$adj.r.squared, digits=4)),
                               bg="white"
                               )
                }
                j<-j+1
                cat("\n", curc, crop, " ok - ")
            }else{
                cat("\n", curc, crop, " NO - ")
            }
            k<-k+1
            if(k>30)j<-100
            cat(j, k, crop)
        }
        plot(0, 0)
        legend(x="topright", legend=curyears[1:length(curyears)],  pch=rep(16,length(curyears)),
               col=c("white", cols[1:(length(curyears)-1)]))

        dev.off()
    }
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

lapm2fssact<-function(capridat, sel, inv=0){
    
    #LGRAS	  .  	(GRAI,GRAE,FALLOSET)
    #LMAIZ	  .  	(MAIZ,MAIF)
    #LOCRO	  .  	(OCRONECR,OOIL)
    #LOLIV	  .  	(OLIV,TABO)
    #LRAPE	  .  	RAPEVSET
    #LVINY	  .  	(TAGR,TWIN)
    #OFRU	  .  	APPLOFRU
    #OVEG	  .  	TOMAOVEG
    
    capridat$COLS <- as.character(capridat$COLS)
    
    newact<-list(c("LGRAS","GRAI"),
                 c("LGRAS","GRAE"),
                 c("LGRAS","FALLOSET"),
                 c("LMAIZ","MAIZ"),
                 c("LMAIZ","MAIF"),
                 c("LOCRO","OCRONECR"),
                 c("LOCRO","OOIL"),
                 c("LOLIV","OLIV"),
                 c("LOLIV","TABO"),
                 c("LRAPE","RAPEVSET"),
                 c("LVINY","TAGR"),
                 c("LVINY","TWIN"),
                 c("OFRU","APPLOFRU"),
                 c("OVEG","TOMAOVEG"))
    
    for(i in 1:length(newact)){
        
        from<-1; to<-2
        if(inv==1){from<-2; to<-1}
        #cat("\n", newact[[i]][from], " to ", newact[[i]][to])
        capridat$COLS[sel & capridat$COLS==newact[[i]][from]] <- newact[[i]][to]
        
    }
    
    return(capridat)
    
}

renamecrops<-function(dkd){
    dkd$COLS <- as.character(dkd$COLS)
    dke<-dkd
    # crops in ...  LAMP   LPIS
    inv<-1 #Replace LPIS data
    newact<-list(c("LGRAS","GRAI"),
                 c("LGRAS","GRAE"),
                 c("LGRAS","GRAS"),
                 c("LGRAS","FALLOSET"),
                 c("LMAIZ","MAIZ"),
                 c("LMAIZ","MAIF"),
                 c("LRAPE","RAPEVSET"),
                 c("LOCRO","OCRONECR"),
                 c("LOCRO","OOIL"),
                 c("LOCRO","SOYA"),
                 c("LOCRO","TOBA"),
                 c("LOCRO","TEXT"),
                 c("LOCRO","SUNF"),
                 c("LOLIV","OLIV"),
                 c("LOLIV","TABO"),
                 c("LVINY","TAGR"),
                 c("LVINY","TWIN"),
                 c("APPLOFRU","OFRU"),
                 c("APPLOFRU","APPL"),
                 c("OVEG", "POTA"),
                 c("OVEG", "FLOW"),
                 c("OVEG", "TOMAOVEG"),
                 c("OCER", "OATS"),
                 c("OCER", "RYEM"),
                 c("OCER", "PARI"),
                 c("WHEA", "SWHE"),
                 c("WHEA", "DWHE"),
                 c("PERM", "NUTS"),
                 c("PERM", "NURS"),
                 c("PERM", "LOLIV"),
                 c("PERM", "LVINY"),
                 c("PERM", "CITR")
    )
    for(i in 1:length(newact)){dke$COLS[dke$COLS==newact[[i]][2]] <- newact[[i]][1]}
    return(dke)
}

