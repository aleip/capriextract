---
title: "Capri extract"
output: 
  html_document: 
    keep_md: yes
---





# Introduction 
Reading data on agricultural residues from the capri model, using the capriextract R functions.


## Path to data 
The `capri_dirs.r` script defines the path to the data depending on which machine is runing.
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


## Packages

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


# Load `capriextract_functions.r`

```r
getwd()
```

```
## [1] "/home/rougipa/R/capriextract"
```

```r
source("capriextract_functions.r")
```

# use `opendata` 

## Run the `opendata` function directly
The opendata function has 3 arguments,
but global variables are also needed to make the `opendata`  function work. 
Pass the arguments the opendata function wants to have in the capdis context.

```r
# Global variables needed by the opendata function in the "capdis" context
datapath <- "/Bioeconomy/BIOMASS/capri/"
baseyear <- 1212
data3dim<-c("RALL","COLS","ROWS","VALUE")
# Load the gdx library by specifying the GAMS directory as a parameter
igdx(gamsSysDir = "/opt/gams/gams25.1_linux_x64_64_sfx/")
```

```
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
knitr::kable(head(es), format = "markdown")
```



|RALL     |COLS    |ROWS     |       VALUE|
|:--------|:-------|:--------|-----------:|
|ES300000 |OTHER   |LEVL     | 336.0196126|
|ES300000 |AREA    |LEVL     | 729.4231014|
|ES300000 |AREAcon |LEVL     | 729.4231014|
|ES300000 |SWHE    |RELIRRI  |   1.0552882|
|ES300000 |SWHE    |YSCALING |   0.7772080|
|ES300000 |SWHE    |N2OAPP   |   0.0009732|

To fix the following error:
> Error in opendata("capdis", "ES", "2010") : object 'data3dim' not found

Where is data3dim defined?
```
grep -n data3dim *.r
capriextract_functions.r:42:    datanames<-data3dim
capri_sets.r:4:data3dim<-c("RALL","COLS","ROWS","VALUE")
```

## Copy code from inside the `opendata` function 


```r
# opendata function parameters
scope <- "capdis"
curcountry <- "ES"
curyear <- "1212"
# Global variables needed by the opendata function in the "capdis" context
datapath <- "/Bioeconomy/BIOMASS/capri/"
baseyear <- 1212
data3dim<-c("RALL","COLS","ROWS","VALUE")
# Copy-paste the part concerning the "capdis" scope
datafile<-paste0("capdis/xobs_2_",curcountry,"_",baseyear)
datafile<-paste0(datapath,datafile,".gdx")
dataparm<-"xobs"
ydim<-""
datanames<-data3dim
if(file.exists(datafile)){
    cat("\n ",datafile)
    d<-list(datafile,dataparm,datanames,ydim)
    capridat<-rgdx.param(datafile,dataparm)
    names(capridat)<-datanames
    if(grepl("lapm", scope)){
        capridat$Y<-gsub("_","",curyear)
        if(curyear=="")capridat$Y<-"capdis"
        capridat$NUTS2<-curcountry
        if(curyear=="preds"){
            capridat$ROWS<-"LEVL"
        }
        capridat<-select(capridat, c("NUTS2", data4dim))
    }
}
```


# RGDX

## installing the package
`capridat<-rgdx.param(datafile,dataparm)` returned an error:
> Error in rgdx.param(datafile, dataparm) : 
  could not find function "rgdx.param"
 
Where is this function defined? 
See the package section above the function is defined in the `gdxrrw` package.  

# Installing the GAMS system to render the GDW libraries available
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
## Reloading GDX API
## GDX API loaded from gamsSysDir=/opt/gams/gams25.1_linux_x64_64_sfx/
## The GDX library has been loaded
## GDX library load path: /opt/gams/gams25.1_linux_x64_64_sfx/
```

