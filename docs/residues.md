---
title: "Capri extract"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---



[TOC]

# Introduction 
Reading data on agricultural residues from the capri model, using the capriextract R functions.


# Requirements

## Installing R packages

External packages used by the capriextract functions: 

```
cat capri_packages.r
#Used Packages
rm(list=objects())
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
```

The `gdxrrw` package is needed for the `rgdx.param()` function which reads gdx data. 
Package `gdxrrw` is not available on CRAN:
```
Warning in install.packages :
  package ‘gdxrrw’ is not available (for R version 3.5.1)
```
It is available from support.gams.com
[GDXRRW: Interfacing GAMS and R](https://support.gams.com/gdxrrw:interfacing_gams_and_r)


```r
install.packages("~/downloads/gdxrrw_1.0.4.tar.gz", repos = NULL, type = "source")
```

## Installing the GAMS system and related GDX libraries
Error loading the GDX API: use igdx() to diagnose and solve the problem

```r
igdx() 
# The GDX library has not been loaded
```

Note from support.gams.com
[GDXRRW: Interfacing GAMS and R](https://support.gams.com/gdxrrw:interfacing_gams_and_r)

> For GDXRRW to work it must load shared libraries from the GAMS system directory. One common problem is a failure to find the GAMS system directory and these shared libraries. The igdx command can be used to show what GAMS system directory GDXRRW has found (igdx()) and to point GDXRRW to a particular GAMS system directory if so

I ran the following to install gams:
```
mkdir /opt/gams
cd /opt/gams/
chmod u+x /home/user/downloads/linux_x64_64_sfx.exe 
ll /home/user/downloads/linux_x64_64_sfx.exe 
/home/user/downloads/linux_x64_64_sfx.exe 

# Move to the gams folder
cd gams25.1_linux_x64_64_sfx/
./gamsinst 
```

Now in R, the `igdx()` can load the gdx library by specifying the GAMS directory as a parameter: 

```r
igdx(gamsSysDir = "/opt/gams/gams25.1_linux_x64_64_sfx/")
```

```
## GDX API loaded from gamsSysDir=/opt/gams/gams25.1_linux_x64_64_sfx/
## The GDX library has been loaded
## GDX library load path: /opt/gams/gams25.1_linux_x64_64_sfx/
```



# use `opendata()` from `capriextract_functions.r`

## Run the `opendata` function directly on spanish data
The opendata function has 3 arguments,
but global variables are also needed to make the `opendata`  function work. 
Pass the arguments the opendata function wants to have in the capdis context.

```r
getwd()
```

```
## [1] "/home/rougipa/R/capriextract"
```

```r
source("capriextract_functions.r")
# Global variables needed by the opendata function in the "capdis" context
datapath <- "/Bioeconomy/BIOMASS/capri/"
baseyear <- 1212
data3dim<-c("RALL","COLS","ROWS","VALUE")
# Load the gdx library by specifying the GAMS directory as a parameter
igdx(gamsSysDir = "/opt/gams/gams25.1_linux_x64_64_sfx/")
```

```
## Reloading GDX API
## GDX API loaded from gamsSysDir=/opt/gams/gams25.1_linux_x64_64_sfx/
## The GDX library has been loaded
## GDX library load path: /opt/gams/gams25.1_linux_x64_64_sfx/
```

```r
# use the function
es <- opendata("capdis", "ES", "2010")
```

```
## 
##   /Bioeconomy/BIOMASS/capri/capdis/xobs_2_ES_1212.gdx
```

```r
knitr::kable(head(es, 10), format = "markdown")
```



|RALL     |COLS    |ROWS     |       VALUE|
|:--------|:-------|:--------|-----------:|
|ES300000 |OTHER   |LEVL     | 336.0196126|
|ES300000 |AREA    |LEVL     | 729.4231014|
|ES300000 |AREAcon |LEVL     | 729.4231014|
|ES300000 |SWHE    |RELIRRI  |   1.0552882|
|ES300000 |SWHE    |YSCALING |   0.7772080|
|ES300000 |SWHE    |N2OAPP   |   0.0009732|
|ES300000 |SWHE    |N2OSYN   |   1.2082809|
|ES300000 |SWHE    |NOXAPP   |   0.0002607|
|ES300000 |SWHE    |NH3APP   |   0.0172118|
|ES300000 |SWHE    |NOXSYN   |   0.3534031|

```r
if(FALSE){
    write.csv(es, "~/downloads/capdis_ES_2010.csv", row.names = FALSE)
    saveRDS(es, "~/downloads/capdis_es_2010.rds")
    capdis_spain <- readRDS("~/downloads/capdis_es_2010.rds")
}
```




## Missing objects while runing the opendata function

To load data under the capdis scope, the following objects have to be present
in the global environement: 

 * datapath, 
 * baseyear 
 * and data3dim.

For example here is an error returned when data3dim is not defined:
> Error in opendata("capdis", "ES", "2010") : object 'data3dim' not found

Where is data3dim defined?
```
grep -n data3dim *.r
capriextract_functions.r:42:    datanames<-data3dim
capri_sets.r:4:data3dim<-c("RALL","COLS","ROWS","VALUE")
```

## Note on the data paths
The `capri_dirs.r` script defines the data paths depending on which machine is runing.
It uses the system name to adapt path variables to each particular system. 
We could use the following conditional switch:

```r
Sys.info()[4] == "d01ri1603346.ies.jrc.it" 
```

```
## nodename 
##     TRUE
```

But for the moment we can also simply overwrite the `datapath` variable.


# Use `gdxrrw::rgdx.param()` directly to open the metadata dump

Open the metadata dump file using the gdxrrw package directly:

```r
datafile <- "/Bioeconomy/BIOMASS/capri/capdis/dumpcapdis_EndOfCapdis.gdx"
dataparm <- "xobs"
dumpcapdis <- rgdx.param(datafile, dataparm)
knitr::kable(head(dumpcapdis, 10), format = "markdown")
```



|.i       |COLS    |ROWS     |         XOBS|
|:--------|:-------|:--------|------------:|
|RO070000 |OTHER   |LEVL     | 1751.4378979|
|RO070000 |AREA    |LEVL     | 3486.2055815|
|RO070000 |AREAcon |LEVL     | 3486.2055815|
|RO070000 |SWHE    |RELIRRI  |    1.2992839|
|RO070000 |SWHE    |YSCALING |    0.9869032|
|RO070000 |SWHE    |N2OAPP   |    0.2274606|
|RO070000 |SWHE    |N2OSYN   |    0.3538747|
|RO070000 |SWHE    |NOXAPP   |    0.0672335|
|RO070000 |SWHE    |NH3APP   |    4.1470952|
|RO070000 |SWHE    |NOXSYN   |    0.1030497|



