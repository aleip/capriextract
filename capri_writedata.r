write.csv(rbind(mcactexp),paste0("set_cropactivities",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(maactexp,daactexp),paste0("set_animalactivities",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(feed_rowsexp,paste0("set_feedingstuff",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(feed_to_o,paste0("set_feedingstuffcomposition",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rows,paste0("set_rows",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
#write.csv(rbind(frows,ico),paste0("set_crop_and_animalproducts",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(frmbal_cols,mrkbal_cols),paste0("set_products_marketbalance",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(rall),paste0("set_rall",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)
write.csv(rbind(fert_distexp),paste0("set_fert_dist",format(Sys.time(), "%Y%m%d"),".csv"),row.names=FALSE)

metafile<-paste0("data_",scope,"_meta",format(Sys.time(), "%Y%m%d"),".csv")
con<-file(paste0("data_",scope,format(Sys.time(), "%Y%m%d"),".csv"),open = "wt")
if(scope%in%c("feed_marketbal","activities")|grepl("baseyear",scope)) 
  writeLines(paste0("# Data Source: CAPREG - 21.03.2018"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Data Source: CAPREG-time series - 06.04.2018"),con)
writeLines(paste0("# Repository: ",svnpath),con)
if(scope%in%c("feed_marketbal","activities")|grepl("baseyear",scope))
  writeLines(paste0("# Files used: res_%Y%%RALL%.gdx"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Files used: %MSALL%_12.gdx"),con)
if(scope%in%c("feed_marketbal","activities")|grepl("baseyear",scope))
  writeLines(paste0("# Parameter: DATA2"),con)
if(scope=="tseries_marketbal")writeLines(paste0("# Parameter: DATA"),con)
writeLines(paste0("# Suggested quotation:"),con)
writeLines(paste0("#                      Wolfgang Britz and Peter Witzke (editors): CAPRI model documentation 2015. Available at https://svn1.agp.uni-bonn.de/svn/capri/trunk/doc. [This is the latest version. A public (older) version is available at the CAPRI model website at https://www.capri-model.org/dokuwiki/doku.php?]"),con)
writeLines(paste0("#                      Markus Kempen and Peter Witzke (2018). Improvement of the stable release of the CAPRI model: Fertilizer and Feed allocation routines. Deliverable 3: Revised feed module for CAPRI. Specific contract No. Joint Research Centre 154208.X39"),con)
if(scope=="tseries_marketbal")writeLines(paste0("#                      Responsible for (integrating feed data into) the time series: Alexander Gocht"),con)
if(scope%in%c("feed_marketbal","activities")|grepl("baseyear",scope)) 
  writeLines(paste0("# Definition (livestock activities) of columns and rows (feed stuff)"),con)
if(scope%in%c("feed_marketbal","tseries_marketbal"))writeLines(paste0("# Definition columns: market balance - rows: crop and animal product."),con)
if(scope%in%c("feed_marketbal","tseries_marketbal"))writeLines(paste0("# For definition of sets see files set_*"),con)
writeLines(paste0("#\n# Detailed metainformation see file ",metafile),con)
if(scope%in%c("activities")) writeLines(paste0("#Units: crop activities in [1000 ha]; livestock activities in [1000 head]; poultry activities [1000000 heads]"),con)
write.csv(capridat,con,row.names=FALSE)
close(con)


con<-file(metafile,open = "wt")
write.csv(caprimeta[caprimeta$.i4%in%meta2keep,],con,row.names=FALSE)
close(con)
