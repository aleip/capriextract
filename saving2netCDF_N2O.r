
# This is a scritp to save data (results) from CAPRI as NetCDF files after aggregate it at 0.25 deg
# Step1: Retrieving CAPRI data to be exported and spatial data (marsgrid - hsu)
# Step2: Calculating XY coordinates and plotting a map for checking (only if needed)
# Step3: 




## Retrieving data ####

source("E://capriextract/capri_packages.r")
source("E://capriextract/capriextract_functions_4mapping.r")
loadGISenv() #"E:\capriextract\capriextract_functions_4mapping.r"

load("//ies-ud01.jrc.it/D5_agrienv/Data/capdis_results/20181130_verify/xobs_EU27_2000-2012_n2o.rdata", verbose = TRUE)

#head(n2otot)  #emissions
#head(n2ouaar) #emissions/categ
#head(n2o)


#load("\\\\ies\\d5\\agrienv\\Data\\uscie\\hsu2_database_update_2016_02orig\\uscie_hsu2_nuts_marsgrid.rdata", verbose = TRUE)

HSU2_DEFPARAM <- data.table::fread("\\\\ies\\d5\\agrienv\\Data\\uscie\\hsu2_database_update_2016_02orig\\HSU2_DEFPARAM.csv", header = TRUE)

hsu2_grid025 <- HSU2_DEFPARAM[, c("HSU2_IDRUN", "HSU2_CD_ME")]
hsu2_grid025$HSU2_IDRUN <- paste0("U", hsu2_grid025$HSU2_IDRUN)

n2otot_grid025 <- merge(n2otot, hsu2_grid025, by.x = "RALL", by.y = "HSU2_IDRUN", all.x = TRUE)
setnames(n2otot_grid025, "HSU2_CD_ME", "grid025")

n2otot_grid025_2 <- as.data.frame(n2otot_grid025 %>% group_by(Y, grid025) %>% summarise_at(.vars = c("TOTEM", "UAAR"), .funs = sum, na.rm = TRUE))
n2otot_grid025_2$EMperUAAR <- n2otot_grid025_2$TOTEM / n2otot_grid025_2$UAAR

n2otot_grid025_2 <- n2otot_grid025_2[, c(2, 1, 3, 5, 4)]


#calculating centroid of meteogrid 0.25 deg

grid025 <- readOGR(dsn = "\\\\ies\\d5\\agrienv\\Data\\uscie\\hsu2_database_update_2016_02orig", layer = "meteo_025deg_from_HSU")

grid025_centr <- rgeos::gCentroid(grid025, byid = TRUE, id = grid025$codemeteo)
grid025@data <- cbind(grid025@data[, 1], as.data.frame(grid025_centr@coords))
names(grid025@data) <- c("codemeteo", "lon", "lat")



## plotting map ####

data4map <- merge(grid025, n2otot_grid025_2[n2otot_grid025_2$Y == 2012, ], by.y = "grid025", by.x = "codemeteo", all.y = TRUE)
names(data4map@data)[1] <- "grid025"
data4map <- data4map[!is.na(data4map@data$TOTEM), ]


jpeg("~/kkplot.jpg", width = 21, height = 29.7, units = "cm", res = 300, quality = 100, pointsize = 8)

plot(grid025)
#plot(data4map[!is.na(data4map@data$TOTEM), ])
plot(data4map, add=T, col="red")

dev.off()



## save to netcdf ####
library(ncdf4)
library(RNetCDF)


# Filling in the gaps 

n2otot_grid025_3 <- merge(n2otot_grid025_2, grid025@data, by.x = "grid025", by.y = "codemeteo", all.x = TRUE)
n2otot_grid025_3_unique <- unique(n2otot_grid025_3[, c("grid025", "lon", "lat")])
n2otot_grid025_3 <- n2otot_grid025_3[order(n2otot_grid025_3$Y, n2otot_grid025_3$lon, n2otot_grid025_3$lat), ]
n2otot_grid025_4 <- n2otot_grid025_3[, c("lon", "lat")]
n2otot_grid025_4 <- unique(n2otot_grid025_4)

n2otot_grid025_6 <- as.data.frame(matrix(ncol = 0, nrow = 0)) 

for(i in unique(n2otot_grid025_3[, "Y"])){
  n2otot_grid025_5 <- n2otot_grid025_3[n2otot_grid025_3$Y %in% i, ]
  n2otot_grid025_5 <- merge(n2otot_grid025_5, n2otot_grid025_4, by = c("lon", "lat"), all = TRUE)
  n2otot_grid025_5$Y <- i
  n2otot_grid025_5 <- merge(n2otot_grid025_5[, -c(3)], n2otot_grid025_3_unique, by = c("lon", "lat"), all.x = TRUE)

  n2otot_grid025_6 <- rbind(n2otot_grid025_6, n2otot_grid025_5)
  
}

# creating a 3-dim array
n2otot_grid025_6 <- n2otot_grid025_6[order(n2otot_grid025_6$Y, n2otot_grid025_6$lon, n2otot_grid025_6$lat), ]
n2otot_grid025_7 <- n2otot_grid025_6[, 1:4]

n2otot_grid025_7_3dim <- array(n2otot_grid025_7$TOTEM, c(10164, 1, 7))


#Defining variables 
lon <- n2otot_grid025_6[n2otot_grid025_6$Y == 2002, "lon"]
lat <- n2otot_grid025_6[n2otot_grid025_6$Y == 2002, "lat"]
year <- unique(n2otot_grid025_6[, "Y"])
x <- length(lon)
y <- length(lat)
z <- length(year)

# defining dimensions
dimX <- ncdim_def("longitude", "meters", lon)
dimY <- ncdim_def("latitude", "meters", lat)
dimT  <- ncdim_def("years","years 2000 - 2012", as.numeric(year), unlim = TRUE)

# set missing value
missval  <- -9.e+33

# Creating the main variable and the .nc file
varbl <- ncvar_def(name = "TotalEM", " 1000 kg N2O-N/yr", list(dimX, dimY, dimT), missval = missval)
ncfile <- nc_create(filename = paste0("~/", "VERIFY_capri_n2o_025DEG_20181130_20190530_2.nc"), vars = varbl)


# opening .nc file for writing data
nc2 <- open.nc(paste0("~/", "VERIFY_capri_n2o_025DEG_20181130_20190530_2.nc"), write = TRUE)

# writing the data
var.put.nc(ncfile = nc2, variable = "TotalEM", data = n2otot_grid025_7_3dim, start=NA, count=NA, na.mode=0, pack=FALSE)

# adding attributes to the file
att.put.nc(ncfile = nc2, variable = "NC_GLOBAL", name = "coord_ref", type = "NC_CHAR", value = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m")
att.put.nc(ncfile = nc2, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Adrian Leip (JRC) - adrian.leip@ec.europa.eu")
att.put.nc(ncfile = nc2, variable = "NC_GLOBAL", name = "Emissions_considered", type = "NC_CHAR", value = "N2OAPP (N2O emissions from application of manure to agricultural soils), N2OGRA (N2O emissions from deposition of manure by grazing animals), N2OSYN (N2O emissions from application of synthetic fertilizers), N2OHOU (N2O emissions from animal housing and manure management systems)")
att.put.nc(ncfile = nc2, variable = "NC_GLOBAL", name = "Model_name_Version", type = "NC_CHAR", value = " CAPREG-timeseriesGHG (20180711, Revision: 7247,  res_%BAS%%MS%.gdx,  Branch: ecampa3 ), CAPREG-12 (20181106, Revision: 7503,  res_time_series_GHG_%MS%.gdx',  Branch: epnf),  CAPDIS-1212 (20181107, Revision: 7503,  xobs_2_%MS%_%BAS%%BAS%,  Branch: epnf),  CAPDIS-12-2xxx (20181121, Revision: 7538,  xobs_2_%MS%_%BAS%%Y%,  Branch: epnf)")

close.nc(nc2)


# opening again the connection for checking
nc <- nc_open(filename = paste0("~/", "VERIFY_capri_n2o_025DEG_20181130_20190530_2.nc"), write = TRUE)

str(nc)
nc

nc_close(nc)






