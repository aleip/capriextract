#' @title Spatial disaggregation results - scatter plots
#' @description Generates scatter plots based on LPIS data and LAPM / CAPDIS results
#' @param date 30/10/2018
#' @author Adrian Leip
#' @export


# Retrieve Matching data with LPIS data; FSS2010 and LAPM predictions are already joined
# --> eliminate fields that are not required
# --> rename field to fit for functions
dkdata<-"\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\lpis_and_other_agricultural_data_at_high_spatial_resolution\\denmark\\maps\\grid10km_nuts23_hsu_DK_ALL_2010.csv"
dkdata<-fread(dkdata,header=TRUE)
dk<-select(dkdata, -c(OBJECTID, Shape, USCIE_GRID10_NUTS2_3_HSU2IDRUN_nr, 
                      HSU2_CD_NG, HSU2_CD_NG, HSU2_IDRUN,
                      Shape_Length, Shape_Area))
setnames(dk, old=c("USCIE_GRID10_NUTS2_3_HSU2IDRUN", "ADMIN_EEZ"), new=c("RALL", "NUTS2"))

# Melt data and split field with combined crop_datasource information
# --> Keep only LPIS, LAPMpredictions and FSS2010 data for comparison
dkd<-melt(dk, id.vars=1:2)
dkd$COLS<-gsub("_.*","",dkd$variable)
dkd$ROWS<-"LEVL"
dkd$variable<-gsub(".*_","",dkd$variable)
#unique(dkd$variable)
#[1] "FSS2010"     "LAPM2010"    "CAPRI2012BJ" "LPIS2010"    "LAPMp2010"  
#keep only LAPMp2010 and LPIS2010
dkd<-filter(dkd, variable %in% c("LPIS2010", "LAPMp2010", "FSS2010"))
curcountries<-"DK"
curyears<-c("LPIS2010", "LAPMp2010", "FSS2010")

# Rename and group crops to matching and aggregate classes
dke<-renamecrops(dkd)
dkt<-dcast(dke, NUTS2 + RALL + COLS + ROWS ~ variable, value.var="value", sum)
dkt$LPIS2010[is.na(dkt$LPIS2010)]<-0
dkt$LAPMp2010[is.na(dkt$LAPMp2010)]<-0
dkt<-dkt[grepl("^DK",dkt$NUTS2),]
dkt$LAPMp2010<-dkt$LAPMp2010/10
dkt$NUTS2<-substr(dkt$NUTS2,1,2)

xobsscatter(xobs=dkt, curcact = unique(dkt$COLS)[1:length(unique(dkt$COLS))], curyears, curcountries, add2name="")

