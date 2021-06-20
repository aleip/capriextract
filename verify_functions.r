gisenvironment <- function(){
  source("capriextract_functions_4mapping.r")
  ok <- loadGISenv(loadFSU = TRUE)
  
  grid025 <- readOGR(dsn = "\\\\ies\\d5\\agrienv\\Data\\uscie\\hsu2_database_update_2016_02orig", layer = "meteo_025deg_from_HSU")
  
  #Test
  load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20210107_kipinca/xobs_TestScript_2000-2018_GHGNtot_20210110.rdata")
  
  # Get geometry of 0.25 deg grid and FSUs (if needed later, not necessary for export to netCDF)
  if(! exists("verifygrid")){
    require(rgdal)
    fsu_dir <- "\\\\ies\\d5\\agrienv\\Data\\FSU\\"
    #verifygrid <- readOGR(dsn = paste0(fsu_dir, "fsu_025deg_meteogrid"), layer = "FSU_intersect_meteo_025deg")
    verifygrid <- readOGR(dsn = paste0(fsu_dir, "fsu_025deg_meteogrid"), layer = "meteo_025deg_1km_etrslaea")
    fsugeom <- readOGR(dsn = paste0(fsu_dir, "FSU_delimdata_layer_only_fsuID"), layer = "FSU_delimdata_layer_only_fsuID")
  }  
}

doverify <- TRUE
export2netCDF <- function(
  # data2map = a data.table of the form data.table[, .(rall, y, variable)], with rall=fsuID, y = year, variable = name of variable to map.
  data2map){
  require(ncdf4)
  
  
  fsu_dir <- "\\\\ies\\d5\\agrienv\\Data\\FSU\\"
  verifydir <- gsub("kipinca", "verify", savepath)
  if(! dir.exists(verifydir)){dir.create(verifydir)}
  meteogrid_lonlat <- fread(paste0(fsu_dir, "fsu_025deg_meteogrid/", "meteo_025deg_xy_dd.csv"))[, .(codemeteo, lat=y, lon=x)]
  fsu_meteogrid <- fread(paste0(fsu_dir, "fsu_025deg_meteogrid/", "FSU_intersect_meteo_025deg_table.csv"))[, .(fsuID, codemeteo, fsupartha, fsutotha, mettotha, fsu_share, met_share)]
  
  
  datanames <- names(data2map)
  #check that data table is OK
  if(length(datanames) != 3) {stop("You should provide a data.table with three columns: rall, y, variable")}
  if(datanames[1] != "rall") {stop("First column expected to be 'rall' with fsuID. Please check.")}
  if(datanames[2] != "y") {stop("Second column expected to be 'y' with the year. Please check.")}
  variablename <- datanames[3]
  if(variablename=="N2O"){
    #Convert from N2O-N to N2O
    data2map[, N2O := N2O / 28 * 44]
  }
  verifyfile <- paste0(verifydir, "/", basename(verifydir), "_grid_025deg_", variablename, ".nc")
  
  # Merge data with grid
  # --> without data.table the geographic info would be maintained, data as data.frame.
  #     Because of preference to work with data.tables we do another merge later
  datamapped <- merge(
    data2map[, .(fsuID=rall, y, value=get(datanames[3]))], 
    fsu_meteogrid[, .(fsuID, codemeteo, met_share, fsu_share)], 
    by="fsuID", allow.cartesian = TRUE)
  # ---> If the value is area-based, calculate weighted average of FSU value (using the share of the grid the FSU take)
  #      If the value is absolute, calculate the sum of the shares of the FSU value within each grid (using the share the FSU each grid has)
  gridadata <- datamapped[, sum(value * fsu_share, na.rm=TRUE), by=.(codemeteo, y)]
  gridadata <- merge(gridadata[, .(codemeteo, y, value=V1)], meteogrid_lonlat, by="codemeteo")
  gridadata[is.na(value), value := nav]
  griddatad <- dcast.data.table(gridadata, codemeteo + lat + lon ~ y, value.var = 'value')
  # Write 3-dim array
  cat("\nverifyfile=", verifyfile)
  wdat <- copy(gridadata)
  setnames(wdat, "value", variablename)
  write.csv(wdat[, .SD, .SDcols=c("lon", "lat", "y", variablename)], file = gsub("nc", "csv", verifyfile), row.names = FALSE)
  
  # make matrix with each combination of lon-lat & add data
  vlon <- sort(unique(gridadata$lon), decreasing = FALSE)
  vlat <- sort(unique(gridadata$lat), decreasing = FALSE)
  
  # The number of lon entries is the length of lat
  #vlotn <- sort(unique(data2mapg$lon), decreasing = FALSE)
  #vlon <- sort(unique(data2mapg$lat), decreasing = FALSE)
  vtim <- sort(unique(gridadata$y), decreasing = FALSE)
  lonlat <- as.data.table(expand.grid(vlat, vlon, vtim))[, .(lat=Var1, lon=Var2, y=Var3)]
  
  data2mapgrid <- merge(lonlat, gridadata, by=c("lat", "lon", "y"), all.x=TRUE)

  
  # dtest <- rbind(data2mapg[1:3], data2mapg[1:3], data2mapg[81:83])
  # vlon <- sort(unique(dtest$lon), decreasing = FALSE)
  # vlat <- sort(unique(dtest$lat), decreasing = FALSE)
  # vtim <- sort(unique(dtest$y), decreasing = FALSE)
  # lonlat <- as.data.table(expand.grid(vlat, vlon, vtim))[, .(lat=Var1, lon=Var2, y=Var3)]
  # data2mapgrid <- unique(merge(lonlat, dtest[, .(y, lon, lat, value)], by=c("y", "lon", "lat"), all=TRUE))
  
  setkey(data2mapgrid, y, lon, lat)
  setorderv(data2mapgrid, key(data2mapgrid), c(1, 1, 1))
  nav <-  -9.e+33
  data2mapgrid[is.na(value), value := nav]
  data2mapgrid
  
  ####################################################################################################
  #   Exporting to netCDF
  ####################################################################################################
  
  # Defining dimensions
  lon <- ncdim_def( "Lon", "0.25 degree", vlon, unlim=TRUE) 
  lat <- ncdim_def( "Lat", "0.25 degree", vlat, unlim=TRUE) 
  t <- ncdim_def( "Time", "years", vtim, unlim=TRUE) 	
  
  # Creating the main variable and the .nc file
  ncdata <- ncvar_def(name = variablename, 
                      longname = paste0("Gridded total emissions of agricultural ", variablename, " as estimated by CAPRI - CAPDIS"), 
                      units = paste0("1000 kg ", variablename, "/yr"), 
                      dim = list(lat, lon, t), missval = nav)
  ncfile <- nc_create(filename = verifyfile, vars = ncdata)#, force_v4 = TRUE)
  
  # writing the data
  ncvar_put(ncfile, varid = ncdata, vals = data2mapgrid$value, start = c(1, 1, 1), 
            count = c(length(vlat), length(vlon), length(vtim)))
  
  # Adding attributes to the file
  ncatt_put(ncfile, varid = 0, attname = "coord_ref", attval = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  ncatt_put(ncfile, varid = 0, attname = "author", attval = "Adrian Leip (JRC) - adrian.leip@ec.europa.eu")
  if(variablename=="CH4"){
    ncatt_put(ncfile, varid = 0, attname = "Emissions_considered", attval = "CH4ENT (CH4 emissions from enteric fermentation), CH4MAN (CH4 emissions from manure management), CH4RIC (CH4 emissions from rice cultivation)")
    
  }else if(variablename=="N2O"){
    ncatt_put(ncfile, varid = 0, attname = "Emissions_considered", 
              attval = paste0("N2OSYN: application of mineral fertiliser; N2OAPP: application of manure; ", 
                              "N2OGRA: manure deposition by grazing animals; ", 
                              "N2OHOU: livestock housing and manure management; ",
                              "N2OCRO: crop residues; ", 
                              "N2OHIS: mineralisation of organic matter in histosols; ",
                              "N2ODEP: emissions from atmospheric deposition - attention: this is direct emissions on the field. Indirect emissions from re-deposited volatilisation of NH3 are not included."))
    
  }
  cv <- as.data.table(capriversion)
  ncatt_put(ncfile, varid = 0, attname = "Model_name_Version", 
            attval =  paste0("CAPREG-timeseries_GHG (", 
                             cv[CAPRI_task=="CAPDIS-12-2xxx", Date], 
                             "; Revision: ", cv[CAPRI_task=="CAPDIS-12-2xxx", `Code-Revision`]))
  
  nc_close(ncfile)
  
  check_netCDFdata(verifyfile)
  
  return(verifyfile) 
}


check_netCDFdata <- function(ncfile){
  
  # See email Rona 01/08/2019
  
  require(ncdf4)
  require(fields)
  require(maps)
  nc <- nc_open(filename = ncfile, write = TRUE)
  Longitude=ncvar_get(nc, "Lon")
  Latitude=ncvar_get(nc, "Lat")
  gettime=ncvar_get(nc, "Time")
  gas = names(nc$var)
  unit <- nc$var[[gas]]$units
  emis= ncvar_get(nc, names(nc$var))
  
  ppath <- paste0(dirname(ncfile), "/verify_plots/")
  ppathl <- paste0(dirname(ncfile), "/verify_plotslog/")
  if(! dir.exists(ppath)){dir.create(ppath)}
  if(! dir.exists(ppathl)){dir.create(ppathl)}
  
  pngw <- 512
  
  for (yi in 1:length(gettime)){
    selemis <- emis[,,yi]
    
    png(filename = paste0(ppath, gsub(".nc", paste0("_", gettime[yi], ".png"), basename(ncfile))), 
        width=pngw, 
        height = 0.9*pngw*length(Longitude)/length(Latitude))
    image.plot(Longitude, Latitude, t(selemis)
               #, zlim=c(0, 10e6)
               )
    map('world', add=T)
    mtext(side=3, line=0, cex=1.0, adj = 0, paste0(gas, " emissions [", unit, "]"))
    mtext(side=3, line=0, cex=2.0, adj = 1, gettime[yi])
    dev.off()
  }
  for (yi in 1:length(gettime)){
    selemis <- emis[,,yi]
    selemis[selemis < 10e-4]<- 10e-4
    png(filename = paste0(ppathl, gsub(".nc", paste0("_", gettime[yi], "_log.png"), basename(ncfile))), 
        width=1024, 
        height = 0.9*1024*length(Longitude)/length(Latitude))
    image.plot(Longitude, Latitude, t(log10(selemis))
               #, zlim=c(-3, 6)
    )
    map('world', add=T)
    mtext(side=3, line=0, cex=1.0, adj = 0, paste0(gas, " emissions [", unit, "] (log)"))
    mtext(side=3, line=0, cex=2.0, adj = 1, gettime[yi])
    dev.off()
  }
  
  nc_close(nc)
}



extractGHGs <- function(data2load = "", reginame="EU27", yr){
  
  load(data2load)
  
  fixcodes <- c("rall", "cols", "CAPRINUTS2", "CNTR_CODE")
  selemissions <- c("N2OSYN", "N2OAPP", "N2OGRA", "N2OHOU", "N2OCRO", "N2ODEP", "N2OHIS", "N2OAMM", "N2OLEA", "CH4ENT", "CH4MAN", "CH4RIC")
  
  tempadd <- c("ATMOSD", "CRESID")
  
  selhead <- intersect(names(nbudget), c(fixcodes, selemissions, tempadd, "LEVL"))
  
  ghgs <- nbudget[, selhead, with=FALSE]
  ghgs <- ghgs[cols=="UAAR"]
  ghgsabs <- copy(ghgs)
  if(! "N2OCRO" %in% names(ghgsabs)){  ghgsabs[, N2OCRO := 0.01 * CRESID] }
  if(! "N2ODEP" %in% names(ghgsabs)){  ghgsabs[, N2ODEP := 0.01 * ATMOSD] }
  if(! "N2OHIS" %in% names(ghgsabs)){  selemissions <- selemissions[selemissions!="N2OHIS"] }
  if(! "N2OAMM" %in% names(ghgsabs)){  selemissions <- selemissions[selemissions!="N2OAMM"] }
  if(! "N2OLEA" %in% names(ghgsabs)){  selemissions <- selemissions[selemissions!="N2OLEA"] }
  ghgsabs[, (selemissions) := LEVL * .SD, .SDcols=selemissions]
  
  ghgsabs <- ghgsabs[, c(fixcodes, selemissions, "LEVL"), with=FALSE]
  # Eliminate upper 1% of outliers 
  for (iem in selemissions){
    vem <- ghgsabs[, iem, with=FALSE]
    setnames(vem, iem, "var")
    q99 <- quantile(vem$var, probs = 0.99)
    l99 <- vem$var<q99
    ghgsabs[!l99, (iem) := q99]
  }
  
  ghgsabsm <- melt.data.table(ghgsabs, id.vars = fixcodes, variable.name = "rows")
  
  tmp <- copy(ghgsabsm)  
  tmp[, gas := substr(rows, 1, 3)]
  ghgtot <- tmp[, .(value = sum(value)), by=.(rall, gas)]
  ghgtot[, y := yr]
  # ghgtime <- dcast.data.table(ghgtot, rall + gas ~ y, value.var = "value")
  # 
  # ghgtimec <- copy(ghgtime) 
  # ghgtimec[, `2016` := 0.5 * (`2014`+`2018`)]
  return(ghgtot)
}

firstderivate <- function(xx, curyears){
  
  for(t in 2:length(curyears)){
    slope <- paste0(curyears[t], "incr")
    t1 <- as.character(curyears[t-1])
    t2 <- as.character(curyears[t])
    xx[, (slope) := get(t2) / get(t1)]
  }
  xx1 <- melt.data.table(xx, id.vars = c("rall", "gas"), variable.name = "y")
  xx1 <- xx1[grepl("incr", y)]
  # Filter those with a relative change above factor 2 
  xx2 <- xx1[(value > 2 | value < 0.5)]
  hist(xx2$value)
}

tables4report <- function(){
  load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20210122_verify/xobs_Verify_verifydata_time.rdata")
  # Convert unit from 1000 kg gas --> Mt gas
  # Convert unit from ha ---> Mkm2 (10/1000000=1/100000)
  totPyr <- verifydata[, lapply(.SD, sum, na.rm=TRUE), by=y, .SDcols=c("CH4", "N2O", "LEV")]
  totPyr[, `:=` (CH4=CH4/1000000, N2O=N2O*44/28/1000000, LEV=LEV*10/1000000)]
  totPyr[, `:=` (CH4rel = CH4/totPyr[y==2000, CH4], N2Orel = N2O/totPyr[y==2000, N2O],LEVrel = LEV/totPyr[y==2000, LEV])]

  load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20210122_verify/xobs_Verify_2018-2018_GHGNtot_20210116_dcastclean.rdata")
  totSources <- nbudget[cols=="UAAR"]
  totSources <- totSources[cols=="UAAR", `:=` (CH4ENT=CH4ENT*LEVL, CH4MAN=CH4MAN*LEVL, CH4RIC=CH4RIC*LEVL, 
                                            N2OAPP=N2OAPP*LEVL, N2OGRA=N2OGRA*LEVL, N2OHOU=N2OHOU*LEVL, 
                                            N2OSYN=N2OSYN*LEVL, N2OCRO=0.01*CRESID*LEVL, N2ODEP=0.01*ATMOSD*LEVL)]
  selemissions <- c("N2OSYN", "N2OAPP", "N2OGRA", "N2OHOU", "N2OCRO", "N2ODEP", #"N2OHIS", "N2OAMM", "N2OLEA", 
                    "CH4ENT", "CH4MAN", "CH4RIC")
  totSources4nuts <- totSources[, lapply(.SD, sum, na.rm=TRUE), by=CAPRINUTS2, .SDcols=selemissions]
  tot4nuts <- melt.data.table(totSources4nuts, id.vars = "CAPRINUTS2", variable.name = "source", value.name = "value")
  tot4nuts[, gas := substr(source, 1, 3)]
  tot4nuts[, cnt := substr(CAPRINUTS2, 1, 2)]
  tot4cnt <- tot4nuts[, sum(value), by=c("gas", "cnt")]
  tot4gas <- tot4nuts[, sum(value), by=c("gas")]
  totSources<- totSources[, lapply(.SD, sum, na.rm=TRUE), .SDcols=selemissions]
  
  write.csv(totSources, file="totsources.csv")
  
}

discussiontext <- function(){
  require(openxlsx)
  load("//ies-ud01.jrc.it/D5_agrienv/Data/FSU/fsu_delimdata.rdata")
  load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20210122_verify_for_D4.2/xobs_Verify_verifydata_time.rdata")
  
  verify <- merge(verifydata, fsu_delimdata[, .(rall=fsuID, CAPRINUTS2, CNTR_NAME)], by=c("rall"))
  t1 <- melt.data.table(verify[, lapply(.SD, sum, na.rm=TRUE), by=.(CNTR_NAME, y), .SDcols=c("CH4", "N2O", "LEV")], 
                  id.vars = c("CNTR_NAME", "y"), variable.name = "par", value.name = "value")
  verifycnt <- dcast.data.table(t1,par + CNTR_NAME ~ y, value.var = "value")
  write.xlsx(verifycnt, file="verify_cnt.xlsx")
}
