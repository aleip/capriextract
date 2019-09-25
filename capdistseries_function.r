startextract <- function(){
  
  #' Extraction of Nitrogen surplus data at HSU level for the KIP-INCA project
  #'
  #' Extracts relevant elements for the Gross Nitrogen Surplus by crop at the 
  #' spatial levle of Spatial Homogeneous Units (HSU) and writes them (i) into 
  #' rdata-files and (ii) into csv files for distribution.
  #' 
  #' @author Adrian Leip \email{adrian.leip@ec.europa.eu}
  #' @references Leip, A., Koeble, R., 2018 The CAPRI disaggregation. Report in preparation
  #' @referemces Leip, A., Koeble, R., Reuter, H.I., Lamboni, M., Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284, Unpublished data archive
  #' @references Lamboni, M., Koeble, R., Leip, A., 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82, 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028
  #' @references Leip, A., Marchi, G., Koeble, R., Kempen, M., Britz, W., Li, C., 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5, 73-94. https://doi.org/10.5194/bg-5-73-2008
  #' 
  #' @return saves rdata and csv files with relevant data
  #' 
  scope<<-"capdistimes"                 # Hard-coded elements embedded as a function of (different) scopes
  rm(list=setdiff(objects(), "capri")); setwd(gsub("logfiles", "capriextract", getwd())); source("R/initializecapri.R"); InitCapriEnv(scope = 'capmod')
  #source("capri_packages.r")           # Data libararies require
  #' CAPRI-EPNF branch used on 
  svnpath <<- "https://svn1.agp.uni-bonn.de/svn/capri/branches/epnf"
  
  #' @source 
  capriversion <- as.data.frame(matrix(nrow=1, ncol=6))
  colnames(capriversion) = c("CAPRI_task", "Date", "Revision", "FilesOutput","Branch", "Note")
  capriversion[1,] <- c("CAPREG-timeseriesGHG", "20180711", "7247", "res_%BAS%%MS%.gdx", "ecampa3", "BAS=Base year, MS=Member State") 
  capriversion[2,] <- c("CAPREG-12", "20181106", "7503", "res_time_series_GHG_%MS%.gdx'", "epnf", "MS=Member State") 
  capriversion[3,] <- c("CAPDIS-1212", "20190918", "8173", "xobs_2_%MS%_%BAS%%BAS%", "epnf", "BAS=Base year, MS=Member State") 
  capriversion[4,] <- c("CAPDIS-12-2xxx", "20190918", "8173", "xobs_2_%MS%_%BAS%%Y%", "epnf", "BAS=Base year 2 digits, Y=Simulation year 4 digits") 
  capriversion <<- capriversion
  
  #source("capri_dirs.r")               # Defines paths to data depending machine
  #source("capri_sets.r")               # Loads all relevant sets from a CAPRI dump-file
  source("capriextract_functions.r")
  source("xobsfunctions.r")
  #source("R/initializecapri.R"); 
  #source(".init_leipadrD01RI1600881")
  d5space<<-"\\\\ies-ud01.jrc.it/D5_agrienv/Data/"
  savepath<<-paste0(d5space, "/capdis_results/20181122_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20190116_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20190125_kipinca")
  savepath<<-paste0(d5space, "/capdis_results/20190919_kipinca")
  n2o<<-c("N2OAPP", "N2OGRA", "N2OSYN", "N2OHOU")
  
  curcountries <<- substr(s$nuts0, 1, 2)
  cntnodisagg <<- c("AL", "MK", "CH", "CS", "MO", "BA", "KO", "NO", "TU", "HR")
  eu28<<-setdiff(curcountries, cntnodisagg)
  misregOK <<- c("AT130000", # Wien
                 "BL100000", # Region de Bruxelles-Capitale
                 "DE300000", # Berlin
                 "DE500000", # Bremen
                 "DE600000", # Hamburg
                 "ES630000", # Ciudad Autonoma de Ceuta (ES)
                 "ES640000", # Ciudad Autonoma de Melilla (ES)
                 "ES700000", # Canarias
                 "PT200000", # Acores
                 "PT300000" # Madeira
  )
  curyears<<-as.character(seq(2010, 2012, 2))
  curyears<<-as.character(seq(2000, 2012, 2))
  
  
  mcactuaar <<- c(s$mcact, "UAAR")
  datapath <<- paste0(cenv$capri, "epnfresults/")
  
}
test <- function(){
  a <- c("scope", "reginame", "cols", "ydim", "regi", "curcountries", "rows",
         "curyears", "baseyear", "curscens", "curscenshort", 
         "savepath", "datapath", "svnpath", "capriversion", "mcactuaar",
         "s", "cenv", "sursoi", "soilemissions", "mmsemissions", 
         "levlyild", "xobsname", "reginame")
  b <- 12:20
  return(b)
}

ccclean <- function(){
  #all <- objects()
  kp <- c("scope", "reginame", "cols", "ydim", "regi", "curcountries", "rows",
              "curyears", "baseyear", "curscens", "curscenshort", 
              "savepath", "datapath", "svnpath", "capriversion", "mcactuaar",
              "s", "cenv", "sursoi", "soilemissions", "mmsemissions", 
              "levlyild", "xobsname", "reginame")
  #torem <- as.vector(setdiff(all, toleav))
  torem <<- setdiff(ls(), kp)
  print(setdiff(ls(), kp))
  rm(list=setdiff(ls(), kp), inherits=TRUE)
}

loadcurfile<-function(scope=scope, xobsname="xobs", reginame="EU27",
                      cols=mcact,
                      rows="LEVL", ydim=NULL, regi=NULL,
                      curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  curfile<-paste0(savepath, "/xobs_", reginame, "_2000-2012_", toupper(xobsname), ".rdata")
  if(file.exists(curfile)){
    cat("nLoading existing file ", curfile)
    load(curfile)
  }else{
    xobshsu<-filtermultiple(scope=scope, 
                            cols=cols, 
                            rows=rows, 
                            ydim=ydim, #curdim5=NULL, 
                            #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                            regi=regi, 
                            curcountries=curcountries, 
                            curyears=curyears, 
                            baseyear=baseyear, 
                            curscens=curscens, 
                            curscensshort=curscensshort
    )
    #capriinfo <- capridat[[2]]
    #capridata <- capridat[[1]]
    # xobsnuts2<-capridat[!grepl("^U[1-9]", capridat$RALL),]
    # xobshsu<-capridat[grepl("^U[1-9]", capridat$RALL),]
    # assign(paste0(xobsname,"hsu"), xobshsu)
    # assign(paste0(xobsname,"nuts2"), xobshsu)
    # save(xobsnuts2, xobshsu, file=paste0(curfile))
    xobs <- xobshsu[[1]]
    xobsinfo<-xobshsu[[2]]
    xobsnuts2<-xobs[!grepl("^F[1-9]", xobs$rall),]
    xobshsu<-xobs[grepl("^F[1-9]", xobs$rall),]
    
    assign(paste0(xobsname,"fsu"), xobshsu)
    assign(paste0(xobsname,"nuts2"), xobsnuts2)
    
    save(list=c(paste0(xobsname,"fsu"), paste0(xobsname,"nuts2")), file=paste0(curfile))
  }
  return(xobshsu)
}

extractall <- function(scope=scope, reginame="EU27",
                       cols=s$mcact,
                       ydim=NULL, regi=NULL,
                       curcountries="AT", curyears="2012", baseyear='12', curscens='', curscensshort=''){
  
  if(reginame=="EU27") { curcountries <- curcountries }
  sursoi<-c("SURSOI","NinSOI","NMANAP","NMINSL","ATMOSD","CRESID","NRET","YILD","LEVL","BIOFIX","NMANGR")
  # Note: N2OCRO and N2OHIS not included here
  soilemissions<-c("N2OAPP", "N2OGRA", "N2OSYN", "NH3APP", "NH3GRA", "NH3SYN", "NOXAPP", "NOXGRA", "NOXSYN", "RUNMIN", "RUNSUR")
  mmsemissions<-c("LU", "EXCRET", "N2OHOU", "N2OSTO", "NH3HOU", "NH3STO", "NOXHOU", "NOXSTO", "N2STO", "RUNHOU")
  levlyild <- c("LEVL", "YILD")
  
  
  rows <- sursoi
  xobsname <-"sursoi"
  reginame <- paste(reginame, collapse = "")
  cleanup()
  xobshsu<-loadcurfile(scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  
  rows <- soilemissions
  xobsname="soilemissions"
  cleanup()
  xobshsu<-loadcurfile(scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  
  rows <- mmsemissions
  xobsname="mmsemissions"
  cleanup()
  xobshsu<-loadcurfile(scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  
  rows <- levlyild
  xobsname="levlyild"
  cleanup()
  xobshsu<-loadcurfile(scope="capdistimes", xobsname=xobsname, reginame=reginame,
                       cols=mcactuaar, rows=rows, ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=curcountries, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  
  xobsname="livestock"
  
  cleanup()
  xobshsu<-loadcurfile(scope="capdistimesLU", xobsname=xobsname, reginame="EU27",
                       cols=NULL, rows=NULL, ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=eu28, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  
  xobshsu<-loadcurfile(scope="capdistimes", xobsname="maactlevl", reginame="EU27",
                       cols=s$maact, rows="LEVL", ydim=NULL, #curdim5=NULL, 
                       #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                       regi=NULL, curcountries=eu28, curyears=curyears, baseyear='12', curscens='', curscensshort='')
  
  
}

replaceEUfilesbynewcountry <- function(reginame="FI", lastdate=""){
  
  load("//ies/d5/agrienv/Data/FSU/fsu_delimdata.rdata")
  fsudelim <- fsu_delimdata[, .(rall=fsuID, CNTR_CODE)]
  fsudelim <- fsudelim[, fsuID_nr := as.numeric(gsub("F", "", rall))]
  nuts2del <- fsu_delimdata[, .(CAPRINUTS2, FSUADM2_ID)]
  replacePar <- function(reginame, xobsname, lastdate){
    
    if(lastdate !=""){lastdate <- paste0("_", lastdate)}
    
    toload <- paste0(savepath, "/xobs_", "EU27", "_2000-2012_", toupper(xobsname), lastdate, ".rdata")
    cat("\nLoading", toload)
    load(toload)
    assign("eu", get(paste0(xobsname, "fsu")))
    assign("eu2", get(paste0(xobsname, "nuts2")))
    rm(list=c(paste0(xobsname, "fsu"), paste0(xobsname, "nuts2")))
    
    toload <- paste0(savepath, "/xobs_", reginame, "_2000-2012_", toupper(xobsname), ".rdata")
    cat("\nLoading", toload)
    load(toload)
    assign("reg", get(paste0(xobsname, "fsu")))
    assign("reg2", get(paste0(xobsname, "nuts2")))
    rm(list=c(paste0(xobsname, "fsu"), paste0(xobsname, "nuts2")))
    
    #fsuinreg <- unique(reg$rall)
    #eu <- eu[! rall %in% fsuinreg]
    eu <- merge(eu, fsudelim, by="rall")
    reg <- merge(reg, fsudelim, by="rall")
    eu <- eu[!grepl(reginame, CNTR_CODE)]
    eu2 <- eu2[! grepl(reginame, rall)]
    
    eufsu <- rbind(eu, reg)
    rm(list=c("eu", "reg"))
    eufsu <- eufsu[order(fsuID_nr)]
    eufsu <- eufsu[, .(rall, cols, rows, y, value, n)]
    assign(paste0(xobsname, "fsu"), eufsu)
    rm(eufsu)
    
    eunuts2 <- rbind(eu2, reg2)
    eunuts2 <- eunuts2[order(rall)]
    assign(paste0(xobsname, "nuts2"), eunuts2)
    rm(eunuts2, eu2, reg2)
    
    
    save(list=c(paste0(xobsname,"fsu"), paste0(xobsname,"nuts2")), 
         file=paste0(savepath, "/xobs_", "EU27", "_2000-2012_", toupper(xobsname), "_",format(Sys.time(), "%Y%m%d"), ".rdata"))
    rm(list=c(paste0(xobsname, "fsu"), paste0(xobsname, "nuts2")))
    
  }
  
  replacePar(reginame, xobsname="mmsemissions", lastdate=lastdate)
  replacePar(reginame, xobsname="levlyild", lastdate=lastdate)
  replacePar(reginame, xobsname="soilemissions", lastdate=lastdate)
  replacePar(reginame, xobsname="sursoi", lastdate=lastdate)
  #replacePar(reginame, xobsname="maactlevl", lastdate=lastdate)
  #replacePar(reginame, xobsname="livestock", lastdate=lastdate)
  
}

writemeta<-function(){
  con<-file(paste0(savepath, "/README_capridisagg_",format(Sys.time(), "%Y%m%d"),".txt"),open = "wt")
  fsets <- paste0(savepath, "/README_caprisets_",format(Sys.time(), "%Y%m%d"),".txt")
  
  cat("\n Write meta information ", format(Sys.time(), "%Y%M%d %H:%M"))
  writeLines(paste0("# README file for the disaggregatgion of Nitrogen surplus data at FSU level for the KIP-INCA project",
                    "\n#",
                    "\n# Disaggregated data from CAPRI time series: relevant elements for the Gross Nitrogen Surplus by crop at the",
                    "\n# spatial levle of Farmstructure Soil Units (FSU).",
                    "\n\n# Content of folders:",
                    "\n# Data are organized in 1 folder per parameter. Each row correspond to 1 FSU. Columns are fsuID, fsuID_nr, activities.",
                    "\n#     - For a shapefile with the geometry of the FSU see @RENATE pleaseX.",
                    "\n#     - For a description of crop and livestock activities see file ", fsets,
                    "\n# Variables are as follows:",
                    "\n#        Nitrogen flows relevant for SURSOI",
                    "\n#        Soil budgets terms [kg N / ha / yr] with ha referring to crop activities",
                    "\n#             - NinSOI N inputs to soil as the farmer applies emissions from application have yet to occur",
                    "\n#                      NinSOI = BIOFIX + NMINSL + NMANAP + NMANGR + ATMOSD + CRESID - ",
                    "\n#             - BIOFIX Biological fixation",
                    "\n#             - NMINSL Mineral fertilizer N input net of gaseous losses and run-off",
                    "\n#             - NMANAP Manure input net of all surface losses. Part applied intentionally to agricultural land",
                    "\n#             - NMANGR Manure input net of all surface losses. Part deposited by grazing animals",
                    "\n#             - ATMOSD Atmospheric deposition",
                    "\n#             - CRESID Crop residues",
                    "\n#             - SURSOI Soil surface surplus: all gaseous emissions from manure and mineral fertilizer as well as runoff already subtracted",
                    "\n#                      SURSOI = NinSOI - NRET",
                    "\n#                      SURSOI = Leaching plus denitrification (N2)",
                    "\n#             - NRET   Crop retention",
                    "\n#             - MINLOSSES N losses from mineral fertilizer (NH3, N2O, NOX, run-off)",
                    "\n#             - MANLOSSES N losses from manure after application (NH3, N2O, NOX, run-off)",
                    "\n#             - MMSLOSSES N losses from manure in manure management system",
                    "\n#                      Total excretion of N in manure is obtained from:",
                    "\n#                      EXCRET = NMANAP + NMANGR + MANLOSSES + MMSLOSSES",
                    "\n#",
                    "\n#        Activities",
                    "\n#             - LEVL Cultivation of crops [1000 ha]",
                    "\n#             - LEVLLIVESTOCK Number of animals [1000 head] or [1000000 head for poultry]",
                    "\n#             - 1000Head Number of heads of livestock groups differentiated for grazing and non-grazing animals",
                    "\n#                    Attention that the individual categories for Dairy and Nondairy Cattle are the sum of animals with different LU per head!",
                    "\n#                    The same shares is applied to all livestock activities - see LEVLLIVESTOCK and LU converstion factors:",
                    "\n#                    Livestock activities 'LU' [100 Livestock Units, see https://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Livestock_unit_(LSU)]",
                    "\n#             - 1000Ha Area to which the livestock density refers. This area for grazing animals is based on shares of variour Corine Land Cover Classes. The area for non-grazing animals is UAAR.",
                    "\n#             - HeadperHa Livestock density - calculated from 1000Head and 1000Ha",
                    "\n# ",
                    "\n# "
  ), con)
  
  #                      "\n# rdata-files and (ii) into csv files for distribution.",
  
  
  writeLines(paste0("# @Data source: CAPRI results",svnpath),con)
  writeLines(paste0("# @Repository: ",svnpath),con)
  writeLines(paste(c("#", names(capriversion)), collapse=","), con)
  write.table(capriversion, quote=FALSE, col.names=FALSE, row.names=rep("#", nrow(capriversion)), sep=",", con)
  
  writeLines(paste0("#\n# @Data processing: https://github.com/aleip/capriextract"),con)
  writeLines(paste0("#    file: capdistseries_functions.r, functions extractall and reload_and_write and other functions called therein."),con)
  writeLines("#\n# @author Adrian Leip adrian.leip@ec.europa.eu", con)
  
  writeLines(paste0("#\n# @references Leip A. Koeble R. 2018 The CAPRI disaggregation. Report in preparation"), con)
  writeLines(paste0("# @references Leip A. Koeble R. Reuter H.I. Lamboni M. Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284 Unpublished data archive"), con)
  writeLines(paste0("# @references Lamboni M. Koeble R. Leip A. 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028"), con)
  writeLines(paste0("# @references Leip A. Marchi G. Koeble R. Kempen M. Britz W. Li C. 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5 73-94. https://doi.org/10.5194/bg-5-73-2008"), con)
  writeLines(paste0("#\n# @documentation see XZXXX"), con)
  writeLines("#\n", con)
  
  close(con)
  
  con<-file(fsets,open = "wt")
  curset <- sdesc[grepl("activities", set)]
  write.table(curset, quote=FALSE, row.names=FALSE, sep=",", con)
  close(con)
}

reload_and_write <- function(reginame="EU27"){
  
  wrapoverwrite <- function(x){
    
    # Write out data
    params<-setdiff(names(x), c("rall", "cols", "y"))
    yrs <- unique(x$y)
    y <- copy(x)
    for(p in params){
      for(yr in yrs){
        cat("\n Extract", as.character(p), "for year", yr, "from data (",which(p %in% params),"/",length(params),") ", format(Sys.time(), "%Y%M%d %H:%M"))
        wdata <- y[, c("rall", "cols", "y", p), with=FALSE]
        setnames(wdata, p, "value")
        wdata$rows <- p
        wdata <- wdata[, .(fsuID=rall, fsuID_nr = as.numeric(gsub("F", "", rall)), cols, rows, y, value)]
        wdata <- wdata[abs(value) < 1e-6, value := 0]
        cat("dcast for crops ")
        wdata <- dcast.data.table(wdata, fsuID + fsuID_nr ~ cols, drop=TRUE, value.var="value", fun=sum)
        wdata <- wdata[order(fsuID_nr)]
        #writefilenow(p, yr)
        #Add first line with zeros to indicate to ArcGIS that these are numeric data
        firsline <- wdata[1]
        firsline <- firsline[, `:=` (fsuID = "F0", fsuID_nr = 0)]
        r <- setdiff(names(firsline), c("fsuID", "fsuID_nr"))
        v <- rep(1E-6, length(r))
        firsline <- firsline[, as.vector(r) := as.list(v)]
        
        if(! dir.exists(paste0(savepath, "/", p))){
          dir.create(paste0(savepath, "/", p))
        }
        
        wdata <- rbind(firsline, wdata)
        
        con<-file(paste0(savepath, "/", p, "/capridisagg_", reginame, "_", p, "_", yr, "_",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
        cat("\n Write data ", format(Sys.time(), "%Y%M%d %H:%M"))
        write.csv(wdata, quote=FALSE, con,row.names=FALSE, na="")
        close(con)
        
      }
      #zip(paste0(savepath, "/", "capridisagg4kipinca_", p, "_",format(Sys.time(), "%Y%m%d"),".zip"), 
      #    files=list.files(paste0(savepath, "/", p), pattern="*", full.names=TRUE)
      #)
    }
    
  }
  
  # Note: N2OCRO and N2OHIS not included here
  soilemissions<-c("N2OAPP", "N2OGRA", "N2OSYN", "NH3APP", "NH3GRA", "NH3SYN", "NOXAPP", "NOXGRA", "NOXSYN", "RUNMIN", "RUNSUR")
  mmsemissions<-c("LU", "EXCRET", "N2OHOU", "N2OSTO", "NH3HOU", "NH3STO", "NOXHOU", "NOXSTO", "N2STO", "RUNHOU")
  levlyild <- c("LEVL", "YILD")
  
  reginame <- paste(reginame, collapse="")
  curfile<-paste0(savepath, "/xobs_", reginame, "_2000-2012_")
  
  # Write LEVLYILD - data included in sursoi data - not needed separately
  #cat("\nLoad",paste0(curfile, "LEVLYILD", ".rdata"))
  #load(paste0(curfile, "LEVLYILD", ".rdata"),verbose=FALSE)
  #wrapoverwrite(levlyildfsu)
  #rm(list=c("levlyildfsu", "levlyildnuts2"))
  
  # Calculate total for syn and manemissions
  cat("\nLoad",paste0(curfile, "SOILEMISSIONS", ".rdata"))
  load(paste0(curfile, "SOILEMISSIONS", ".rdata"))
  
  synemissions<-soilemissionsfsu[grepl("SYN|MIN", rows)]
  synemissionstot<-synemissions[, sum(value),by=list(rall, cols, y)]
  synemissionstot$rows <- 'MINLOSSES'
  manemissions<-soilemissionsfsu[grepl("APP|GRA", rows)]
  manemissionstot<-manemissions[, sum(value),by=list(rall, cols, y)]
  manemissionstot$rows <- 'MANLOSSES'
  setnames(manemissionstot, "V1", "value")
  setnames(synemissionstot, "V1", "value")
  manemissionstot <- manemissionstot[,.(rall, cols, rows, y, value)]
  synemissionstot <- synemissionstot[,.(rall, cols, rows, y, value)]
  symmanemissionstot <- rbind(synemissionstot, manemissionstot)
  save(symmanemissionstot, file=paste0(curfile, "SYNMANLOSTOT", ".rdata"))
  rm(list=objects()[grepl("synemissions|manemissions|soilemissions", objects())])
  
  
  # Write SURSOI
  cat("\nLoad",paste0(curfile, "SURSOI", ".rdata"))
  sursoi<-c("SURSOI","NinSOI","NMANAP","NMINSL","ATMOSD","CRESID","NRET","YILD","LEVL","BIOFIX","NMANGR")
  load(paste0(curfile, "SURSOI", ".rdata"))
  
  # clean data for high SURSOI
  clean4sursoi <- function(x, limit = 400){
    
    ms <- as.character(unique(x$rows))
    xs <- x[rows=="SURSOI" & value > limit, .(rall, cols, y, value)]
    
    xhigh <- merge(xs[, .(rall, cols, y)], x, by = c("rall", "cols", "y"))
    if(nrow(xhigh)>0){
      write.csv(dcast.data.table(xhigh, rall + cols + y ~ rows, value.var="value"),
                paste0(savepath, "/" , "capridisagg4kipinca_SURSOI_GT_", limit, "~", format(Sys.time(), "%Y%m%d"),".csv"),
                row.names=FALSE, quote=FALSE, na="")
      xd <- x[rows=="SURSOI" & value > limit, value := 400]
    }else{
      xd <- x
    }
    
    return(xd)
  }
  sursoifsu <- clean4sursoi(x=sursoifsu, limit=400)
  sursoifsu <- sursoifsu[rows %in% sursoi]
  
  # Eliminate data with very low level
  sursoismall <- sursoifsu[rows=="LEVL" & value < 0.0001] 
  combis2rem <- sursoismall[, .(rall, cols, y)]
  save(sursoismall, file=paste0(curfile, "LEVL_LT_0.0001", ".rdata"))
  save(sursoifsu, file=paste0(curfile, "SURSOI_clean400", ".rdata"))
  
  sursoifsu <- dcast.data.table(sursoifsu, rall + cols + y ~ rows, value.var="value")
  sursoifsu <- sursoifsu[ LEVL >= 0.0001 ]
  # 
  wrapoverwrite(sursoifsu)
  rm(list=objects()[grepl("sursoi", objects())])
  
  
  load(paste0(curfile, "SYNMANLOSTOT", ".rdata"))
  symmanemissionstot <- dcast.data.table(symmanemissionstot, rall + cols + y ~ rows, value.var="value")
  combis2rem$rem <- 1
  symmanemissionstot <- merge(symmanemissionstot, combis2rem, by=c("rall", "cols", "y"), all.x=TRUE)
  symmanemissionstot <- symmanemissionstot[is.na(rem), .(rall, cols, y, MANLOSSES, MINLOSSES)]
  wrapoverwrite(symmanemissionstot)
  rm(list=objects()[grepl("symmanemissionstot", objects())])
  
  cat("\nLoad",paste0(curfile, "MMSEMISSIONS", ".rdata"))
  load(paste0(curfile, "MMSEMISSIONS", ".rdata"))
  mmsrows <- unique(mmsemissionsfsu$rows)
  mmsrows <- mmsrows[grepl("HOU|STO", mmsrows)]
  mmsfsutot <-mmsemissionsfsu[rows %in% mmsrows, sum(value),by=list(rall, cols, y)]
  mmsfsutot$rows <- 'MMSLOSSES'
  setnames(mmsfsutot, "V1", "value")
  mmsfsutot <- mmsfsutot[,.(rall, cols, rows, y, value)]
  synmanmmstot <- rbind(symmanemissionstot, mmsfsutot)
  save(synmanmmstot, file=paste0(curfile, "SYNMANMMSLOSTOT_20190925", ".rdata"))
  
  mmsfsutot <- dcast.data.table(mmsfsutot, rall + cols + y ~ rows, value.var="value")
  mmsfsutot <- merge(mmsfsutot, combis2rem, by=c("rall", "cols", "y"), all.x=TRUE)
  mmsfsutot <- mmsfsutot[is.na(rem), .(rall, cols, y, MMSLOSSES)]
  wrapoverwrite(mmsfsutot)
  rm(list=objects()[grepl("mmsfsutot", objects())])
  
  cat("\nLoad",paste0(curfile, "LIVESTOCK", ".rdata"))
  load(paste0(curfile, "LIVESTOCK", ".rdata"))
  livestockfsu <- livestockfsu[! grepl("LU", rows)]
  livestockfsu2 <- dcast.data.table(livestockfsu, rall + rows + y ~ cols + type, value.var="value")
  #livestockfsu <- livestockfsu[`1000Head_graz` + `1000Head_ngra` > 0]
  livestockfsu2 <- melt.data.table(livestockfsu2, id.vars=1:3, measure.vars=4:length(livestockfsu2), value.name='value', variable.name='cols')
  livestockfsu2 <- livestockfsu2[!is.na(value), .(rall, cols, rows, y, value)]
  #livestockfsu <- rbind(livestockfsu, LEVLfsu[, .(rall, cols, rows, y, value)])
  livestockfsu2 <- dcast.data.table(livestockfsu2, rall + cols + y ~ rows, value.var="value", fill=0)
  livestockfsu2 <- livestockfsu2[HeadperHa > 0]
  wrapoverwrite(livestockfsu2)
  
  load(paste0(curfile, "MAACTLEVL", ".rdata"))
  
  livestock <- dcast.data.table(LEVLfsu, rall + cols + y ~ rows, value.var="value")
  setnames(livestock, "LEVL", "LEVLLIVESTOCK")
  wrapoverwrite(livestock)
  
  # Need to write out sets and metadata!!
  #fsets <- paste0("set_activities",format(Sys.time(), "%Y%m%d"),".csv")
  #write.csv(rbind(mcactexp, uaar, maactexp),paste0(savepath, "/" , fsets),row.names=FALSE)
  writefilenow()
  
}

regionaldata <- function(){
  load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20190919_kipinca/xobs_EU27_2000-2012_SURSOI_20190925.rdata")
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  xnuts <- dcast.data.table(sursoinuts2[cols=="UAAR"], rall + cols + y ~ rows, value.var="value")
  setnames(xnuts, "rall", "CAPRINUTS2")
  allnuts <- merge(unique(fsu_delimdata[, .(CAPRINUTS2)]), xnuts, by="CAPRINUTS2", all.x=TRUE)
  
  miscntOK <- c("AL", "BA", "CH", "CS", "HR", "MK", "NO", "TR")
  miscntOK <- paste0("^", miscntOK)
  miscntOK <- paste(miscntOK, collapse="|")
  misnuts <- allnuts[is.na(cols)]
  misnuts <- misnuts[! grepl(miscntOK, CAPRINUTS2)]
  
  
  misnames <- unique(merge(misnuts[! CAPRINUTS2 %in% misregOK, .(CAPRINUTS2)], sdesc[, .(CAPRINUTS2=element, description)], by="CAPRINUTS2"))
  xnuts[CAPRINUTS2 %in% misnames$CAPRINUTS2]
  
  
  sursoifsu <- merge(sursoifsu, fsu_delimdata[, .(rall = fsuID, CNTR_CODE)], by="rall")
  dsursoifsu <- dcast.data.table(sursoifsu, CNTR_CODE + rall + cols + y ~ rows, value.var="value")
  save(dsursoifsu, file=paste0(savepath, "/xobs_EU27_2000-2012_SURSOI_20190925_dcast.rdata"))
  stst <- sursoifsu[cols=="UAAR", .( min = min(value),
                                     max = max(value),
                                     mean = mean(value),
                                     median = median(value)), by=c("CNTR_CODE", "rows", "y")]
  #stst <- dcast.data.table(stst, CNTR_CODE + y ~ rows, value.var=c("min", "max", "mean", "median"))
  write.csv(stst, file=paste0(savepath, "/sursoi_statistics.csv" ))
  stst2 <- sursoifsu[cols!="UAAR", .( min = min(value),
                                     max = max(value),
                                     mean = mean(value),
                                     median = median(value)), by=c("CNTR_CODE", "rows", "y")]
  #stst <- dcast.data.table(stst, CNTR_CODE + y ~ rows, value.var=c("min", "max", "mean", "median"))
  write.csv(stst2, file=paste0(savepath, "/sursoi_statistics_notUAAR.csv" ))
 
  highNinSOI <- dsursoifsu[NinSOI > 2000]
  highNinSOI_noyild <- highNinSOI[is.na(YILD)]
  highNinSOI <- highNinSOI[! is.na(YILD)]
  
  highNinSOI_LEVL.gt.0.1 <- highNinSOI[LEVL >= 0.1] 
  highNinSOI_LEVL.gt.0.1[rall=="F214603"]
  highNinSOI_LEVL.gt.0.1[cols !="UAAR"]
}


