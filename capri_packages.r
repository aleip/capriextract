#Used Packages
rm(list=objects()[!objects()%in%c("scope", "xobshsu", "hsu", "nuts23", 
                                  "curcols", "curyears", "curcountries", "curregi", "currows")])
library(data.table)
library(plyr)
#library(devtools)
#install_github("hadley/dplyr@master")   #to install the last version of dplyr
library(dplyr)    #installed the dev version from https://github.com/hadley/dplyr
#library(reshape)
library(reshape2)
library(stats)
library(gdxrrw)
#library(xlsx)
library(tidyr)

#if(grepl("map",scope)){
    library(sp)
    library(rgdal)
    library(raster)
    #library(graphics)
    library(rgeos)
    #library(ggplot2)
    library(GISTools) 
    library(maps)
#}

