
# This script is to read in the gdx from CAPRI (LAPM + constraining), once the NUTS2 gdx are "collected"
# and exported together in a csv file



# loading needed variables (avoiding running modelling steps)

fle <- "C:\\Users\\rotllxa\\Documents\\ludm_new\\lapm0_settings.r"

settings.lines <- scan(fle, what = character(), nlines = 236, sep = '\n')
settings.lines.collapsed <- paste(settings.lines, collapse = '\n')
source(textConnection(settings.lines.collapsed))



# The function

data2csv <- function(Country=NULL, FSS=NULL){
  
  print(paste("start exporting to csv ","at",Sys.time()))
  
  curcountries2<-as.data.table(read.table("countrynames.txt",sep=" ",header=TRUE))
  
  if(!is.null(Country)){
    #curcountries<- paste0(Country, "000000")
    curcountries<- Country
  }else{
    curcountries <- as.vector(curcountries2$CAPRI_NUTS0)
    curcountries <- substr(curcountries, 1, 2)
  }
  curcountries <- unique(curcountries)
  
  lap_results <- as.data.table(matrix(ncol = 0, nrow = 0))  # to store final results (predictions per crop per HSU) for all countries
  
  for(ct in 1:length(curcountries)){
    
    #if (grepl("NO", curcountries[ct])) {cat("\nPlease, fix the problem with NO!!!","\n "); next}
    
    cat(curcountries[ct]," ")
    
    ls_fls <- list.files(paste0(capripath, "/dat/capdishsu/fssdata/"), pattern = paste0("capdis_", curcountries[ct], "_10GRID.gdx"))
    if (length(ls_fls) == 0) {cat("\nNo data for",curcountries[ct],"\n "); next}
    
    
    if(FSS=="FSS_nuts3"){
      capdis_res <- rgdx.param(paste0(capripath, "/dat/capdishsu/fssdata/capdis_", curcountries[ct],"_10N3.gdx"), "p_capdis")
    }else if (FSS=="FSS_grid10km"){
      capdis_res <- rgdx.param(paste0(capripath, "/dat/capdishsu/fssdata/capdis_", curcountries[ct],"_10GRID.gdx"), "p_capdis")
    }
    
    if (any(names(capdis_res)== ".i")){
      capdis_res <- capdis_res[capdis_res$.j == "LEVL", ]
      capdis_res <- dcast(capdis_res, .i ~ .k, drop=TRUE, value.var="p_capdis")  #to split the variables in columns containing area/sd info
      capdis_res <- as.data.table(capdis_res)
      setnames(capdis_res, c(".i"), c("hsu"))
    }else{
      capdis_res <- capdis_res[capdis_res$j == "LEVL", ]
      capdis_res <- dcast(capdis_res, i ~ k, drop=TRUE, value.var="p_capdis")  #to split the variables in columns containing area/sd info
      capdis_res <- as.data.table(capdis_res)
      setnames(capdis_res, c("i"), c("hsu"))
    }
    
    
    lap_results <- rbind(lap_results, capdis_res, fill = TRUE)
    
  }
  
  lap_results_kk <- copy(lap_results)
  #lap_results <- lap_results_kk
  
  ## There are repeated HSU but with different values. If they are summed, te result is almost equal to AREA
  setkeyv(lap_results_kk, "hsu")
  key(lap_results_kk)
  nms2sum <- names(lap_results_kk)[!names(lap_results_kk) %in% c("hsu", "AREA")]
  allna <- function(x){
    a <- all(is.na(x))
    return(a)
  }
  lap_results_kk_1 <- lap_results_kk %>% group_by(hsu) %>% summarise_at(vars(nms2sum), funs(sum(.,na.rm=TRUE)))
  lap_results_kk_2 <- lap_results_kk %>% group_by(hsu) %>% summarise_at(vars(nms2sum), funs(allna))
  lap_results_kk_2[lap_results_kk_2 == TRUE] <- NA
  lap_results_kk_2[lap_results_kk_2 == FALSE] <- 0
  lap_results_kk_1[, nms2sum] <- as.data.table(lap_results_kk_1[, nms2sum]) + as.data.table(lap_results_kk_2[, nms2sum])
  rm(lap_results_kk_2)
  
  nuts_hsu <- as.data.table(rgdx.param(paste0(capripath, "/dat/capdishsu/p_hsu_srnuts2.gdx"), "p_hsu_srnuts2"))[,1:2] 
  
  if (any(names(nuts_hsu) == ".i")){
    setnames(nuts_hsu, c(".i", ".j"), c("hsu", "nuts2"))
  }else{
    setnames(nuts_hsu, c("i", "j"), c("hsu", "nuts2"))
  }
  
  lap_results_kk_1 <- merge(lap_results_kk_1, lap_results_kk[, c("hsu", "AREA")], by = "hsu", all.x = TRUE)
  lap_results <- merge(lap_results_kk_1, nuts_hsu, all.x = TRUE, by = "hsu")
  
  lap_results <- lap_results[!substr(lap_results$hsu, 1, 2) %in% curcountries, ] 
  #lap_results <-  lap_results[!lap_results$crop %in% c("HSU2_TOTAR", "FACTOR", "ENVCORR", "SUM")] 
  lap_results <- lap_results[!is.na(lap_results$nuts2), ] 
  
  lap_results[is.na(lap_results)] <- -9999
  lap_results<- lap_results[!duplicated(lap_results),]
  
  write.csv(lap_results, paste0(path, "/results/", currunid, "/lap_final_predictions_", FSS, ".csv"), row.names = FALSE)
  write.csv(lap_results, paste0(capripath, "/dat/capdishsu/fssdata", "/lap_final_predictions_", FSS, ".csv"), row.names = FALSE)
  
  cat("\nEnd of Step 4b at", format(Sys.time()),"\n ")  
  
  
}


# Running it:
step1done <- data2csv(Country = Country, FSS = FSS)



