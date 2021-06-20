if(format(Sys.time(), "%Y%m%d")=="20210503"){
  
  # Extracting Nuts2 data
  
  rm(list=objects())
  source("R/initializecapri.R")
  
  currun <- "cur_ru_1_20210122.gms"
  curdate <- format(Sys.time(), "%Y%m%d")
  folderdate <- '20210503'
  project <- 'CAPREG_cropresidues'
  baseyear <- '12'
  InitCapriEnv(capri.runfile = currun, addcomment = "Extraction of base year for bruna")
  source("capdistseries_function.r")
  startextractcapdis(folderdate = '20210503')
  
  curcountrieslist <- list.files(path = paste0(cenv$resout, "/capreg/"), pattern = paste0("res_", baseyear, ".*gdx"), full.names = FALSE)
  curcountrieslist <- unique(substr(curcountrieslist, 7, 8))
  curyears <- 12
  
  nutrients <- c(#"MANN", "MANK", "MANP", 
                 #"KMAN", "KMIN", "KRET", "KCRD", "KAPL", "KMIL,
                 "PMAN", "PMIN", "PRET", "PCRD", "PAPL", "PMIL",
                 "NMAN", "NMIN", "NRET", "NCRD", "NAPL", "NMIL", "BIOFIX",
                 #"NITF", "PHOF", "POTF",
                 "LEVL", "SURSOI"
  )

  # For crop residues
  currows <- c("N2OCRO", "LEVL", "YILD", "MANN", "NCRD")
  
  caprix <- filtermultiple(scope="capreg",
                           #cols=c(s$mpact, "UAAR"),
                           cols=c(s$mcact, "UAAR"),
                           rows=currows,
                           ydim=NULL, #curdim5=NULL,
                           #regi="HSU", curcountries=nuts2[grepl(a, nuts2)], curyears=tser, baseyear='12', curscens='', curscensshort='')
                           regi=s$srnuts2,
                           curcountries=curcountries,
                           #curcountries="BL",
                           curyears=curyears,
                           #curyears=curyear,
                           baseyear=baseyear,
                           curscens="",
                           curscensshort="")
  save(caprix, file = paste0("caprix.rdata"))

  capri <- caprix[[1]][, value := as.numeric(value)]
  capri <- dcast.data.table(capri, rall + y + cols ~ rows, value.var = "value", fill=0)
  capri <- capri[N2OCRO != 0]
  capri[,`:=` (N2OCROabs = N2OCRO * LEVL, PROD = YILD * LEVL, MANNtot = LEVL*MANN, contry = substr(rall, 1, 2))]
  cropres1 <- capri[, lapply(.SD, sum), .SDcols=c("N2OCROabs"), by=.(contry, cols, y)]
  cropres1 <- dcast.data.table(cropres, cols ~ contry, value.var = "N2OCROabs")
  cropres2 <- capri[, lapply(.SD, sum), .SDcols=c("N2OCROabs"), by=.(contry, cols, y)]
  cropres2 <- dcast.data.table(cropres, cols ~ contry, value.var = "PROD")
  cropres3 <- capri[, lapply(.SD, sum), .SDcols=c("N2OCROabs"), by=.(contry, cols, y)]
  cropres3 <- dcast.data.table(cropres, cols ~ contry, value.var = "MANNtot")

  wb <- createWorkbook()
  ws <- addWorksheet(wb, "N2OCRO")
  ws <- writeData(wb, "N2OCRO", cropres1)
  ws <- addWorksheet(wb, "PROD")
  ws <- writeData(wb, "PROD", cropres2)
  ws <- addWorksheet(wb, "MANNtot")
  ws <- writeData(wb, "MANNtot", cropres3)

  saveWorkbook(wb, file="N2OCRO_new_mostcountries~20210503.xlsx")
} 