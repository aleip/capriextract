#' Initialize CAPRI settings to perform CAPRI data extractions and processing
#' @description The function \code{initializecapri()} is a function that should always be run before performing CAPRI extractino tasks as it sets the relevant CAPRI 'environment'
#' @param caprirunfile Path to a file with most relevant information such as paths to gams, data or result folders etc.
#' @param retrievesettings options {0,1} default 1. 1 checks if information is already available depending on computer and user. This will be actiate only of no caprirunfile is given. 
#' @return updates the list caprisettings with all relevant information.
#' @examples \dontrun{
#' caprirunfile <- "x:\\dev\\epnf\\gams\\fortran.gms"
#' InitCapriEnv(caprirunfile)
#' str(c)
#' }
#' @export
require(data.table)
InitCapriEnv <- function(capri.runfile = NULL,
                         scope = NULL){
  
  require(gdxrrw)
  if(! is.null(capri.runfile)){
    
    # Read the caprirunfile and retrieve relevant information
    # In CAPRI, the relevant parameter are given as $SETGLOBAL 
    # therefore here all lines with $SETGLOBAL will be retrieved.
    # However, some are removed as they are irrelevant for most CAPRI data extraction tasks.
    # Note that the spelling has to follow CAPRI customs...
    con <- file(caprirunfile, open = "r")
    setglobal <- readLines(con)
    setglobal <- setglobal[grepl("setglobal|time and date", setglobal, ignore.case=TRUE)]
    setglobal <- setglobal[! grepl("Rexe|Trollexe|gamsArg|procSpeed|JAVA|CMD", setglobal)]
    setglobal <- setglobal[! grepl("regcge_scenario|countries|result_type_underScores|lst2|fst[24]", setglobal)]
    setglobal <- setglobal[! grepl("regLevel|initialLUfile_|tradeMatrixInputFileName_", setglobal)]
    setglobal <- setglobal[! grepl("policy_blocks|modArmington|explicit_NTM|tagg_module|yani_m|REGCGE", setglobal)]
    setglobal <- setglobal[! grepl("altLicense|NET_MIGR|FIX_BUDGET_FAC_SUBS|Supply|abMob|closure_|solpringSupply|limrow|limcol", setglobal)]
    setglobal <- gsub("\\*   Time and date   :", "$SETGLOBAL Time", setglobal)
    close(con)
    
    # For GAMS syntax spaces in the parameter terms are allowed - in R this creates problems
    # Therefore go through them and remove spaces and perform a few other string operations.
    setglobals <- as.data.table(Reduce(rbind, lapply(1:length(setglobal), function(y) {
      a <- strsplit(setglobal[y], " ")[[1]]
      a <- c(a[2], paste(a[3:length(a)], collapse=" "))
      a <- gsub("NA ", "", a)
      a <- gsub("\\\\", "/", a)
      a <- gsub("'", "", a)
      a <- gsub(" ", "", a)
      return(a)
      
    })))
    
    if(! exists("cenv")){
      cenv <- list()
    }
    
    # Main gams-directory from which the gams program has to be called
    # All gams scripts are in underfolders of curdir
    cenv$curdir <- setglobals[V1 == "curDir", V2]
    
    # Folder of results-data. As sometimes runs build on results,
    # but don't want to overwrite, a separate result-folder can 
    # be defined for the storing the results of the current CAPRI run.
    cenv$resin <- setglobals[V1 == "results_in", V2]
    cenv$resout <- setglobals[V1 == "results_out", V2]
    if(cenv$resin == '') cenv$resin <- setglobals[V1 == "Resdir", V2]
    if(cenv$resout == '') cenv$resout <- setglobals[V1 == "Resdir", V2]
    
    # Folder where input data for CAPRI runs are stored
    cenv$datdir <- setglobals[V1 == "Datdir", V2]
    
    # Scratch dir - directory for temporary files or for debugging files
    cenv$scrdir <- setglobals[V1 == "scrdir", V2]
    
    # Additional suffix for the result files
    cenv$resid <- setglobals[V1 == "ResId", V2]
    if(cenv$resid == 'ResId') cenv$resid <- ''
  }else{
    RetrieveCapriInit()
  }
  
  startextract(scope=scope)  
  
  # Push updated c into the global environnment
  cenv <<- cenv
  StoreCapriInit()
  invisible()
  
}

#' Initialize CAPRI settings to perform CAPRI data extractions and processing
#' @description The function \code{getcapri()} retrieves CAPRI environent parameter stored in the list cenv
#' @param x parameter that should be returned
#' @return current value for x
#' @examples \dontrun{
#' tobedone
#' }
#' @export
require(data.table)
GetCenv <- function(x=NULL){
  
  y <- cenv[[x]]
  return(y)
  
}

#' Store CAPRI settings in a list
#' @description The function \code{storecapri()} stores settings by user (user) and computer (nodename) used
#' @param x parameter that should be returned
#' @return current value for x
#' @examples \dontrun{
#' tobedone
#' }
#' @export
require(data.table)

StoreCapriInit <- function(){
  
  me <- Sys.info()[7]
  pc <- Sys.info()[4]
  
  if(file.exists(".caprisettings.Rdata")) load(file=".caprisettings.Rdata")
  if(!exists("caprisettingarchive")) caprisettingarchive <- list()
  caprisettingarchive[[paste0(me, pc)]] <- cenv
  save(caprisettingarchive, file=".caprisettings.Rdata")
  invisible()
  
}

#' Update CAPRI environemnt setting
#' @description The function \code{UpdateCapriInit()} updates settings stored in the 
#'              current session and stores settings by user (user) and computer (nodename) 
#'              using the function \code{StoreCapriInit()}
#' @param x parameter that should be returned
#' @return current value for x
#' @examples \dontrun{
#' tobedone
#' }
#' @export
UpdateCapriInit <- function(x = NULL){
                            # List of vectors of lenght 2 c(variablename, value) 
                            #      eg. list(c(c("curdir", "X:/dev/epnf/gams")),c("datdir", "X:/dev/epnf/dat"))
                            # OR vector of length 2
  failure <- 0
  if(is.null(x)){
    message("Exit - no parameter provided")
    invisible()
  }
  if(! exists("cenv")){
    RetrieveCapriInit()
  }
  if(is.list(x)){
    nx <- length(x)
    for (i in 1 : nx){
      if(length(x[[i]]) < 2){
        message("Exit - vector",i," has less than 2 elements (parameter name, parameter value)")
      }else{
        cenv[[x[[i]][1]]] <- x[[i]][2]
      }
    }
  }else{
    if(is.vector(x)){
      cenv[[x[1]]] <- x[2]
    }
  }
  StoreCapriInit()
  cenv <<- cenv
  
  y <- unlist(lapply(1:length(x), function(z) cenv[[x[[z]][1]]]))
  return(y)
}

#' Update CAPRI environemnt setting
#' @description The function \code{UpdateCapriInit()} updates settings stored in the 
#'              current session and stores settings by user (user) and computer (nodename) 
#'              using the function \code{StoreCapriInit()}
#' @param x parameter that should be returned
#' @return current value for x
#' @examples \dontrun{
#' tobedone
#' }
#' @export
RemoveCapriEnv <- function(x = NULL){
                           # Vector of parameters to be removed
  if (is.null(x)){
    message("Exit - no parameter provided")
  }else{
    
    nx <- length(x)
    if(! exists("cenv")){
      RetrieveCapriInit()
    }
    for (i in 1:nx){
      cenv[x[i]] <- NULL
    }
  }
  StoreCapriInit()
  cenv <<- cenv
}

#' Retrive CAPRI settings from archived settings
#' @description The function \code{retrievecapri()} retrieves settings by user (user) and computer (nodename) used
#' @return current value for x
#' @examples \dontrun{
#' tobedone
#' }
#' @export
require(data.table)

RetrieveCapriInit <- function(){
  
  me <- Sys.info()[7]
  pc <- Sys.info()[4]
  
  if(file.exists(".caprisettings.Rdata")) load(file=".caprisettings.Rdata")
  if(!exists("caprisettingarchive")) caprisettingarchive <- list()
  x <- names(caprisettingarchive)
  if(paste0(me,pc) %in% x){
    
    c <- caprisettingarchive[[paste0(me, pc)]] 

  }else{
    
    message("The user/computer combination has not yet been archived.")
    message("These are the combinations stored")
    print(names(caprisettingarchive))
    choice <- readline(prompt = "Please enter 0 to abort and choose another method\n or select the nunber of the settings you want to retrieve.")
    if(choice !=0){
      if(choice <= length(caprisettingarchive)){
        cat("\nUsing settings from ", paste0(me,pc))
        c <- caprisettingarchive[[as.numeric(choice)]]
      }
    }else{
      message(paste0("Please run InitCapriEnv(capri.runfile) if you have a CAPRI runfile at hand.\n",
                     "Otherwise Choose the option .... (to be developed)."))
      c <- NULL
    }
  }
  
  if(file.exists(".caprisets.Rdata")) {
    GetCapriSets() 
  }else{
    message(paste0("CAPRI sets have not been initialized.\n",
                   "Please run UpdateCapriSets(setfile) if you have gdx file with relevant CAPRI sets."))
  }
  
  cenv <<- c
  invisible()
}


#' Retrive default CAPRI sets (check out meanig of sets in gams)
#' @description The function \code{GetCapriSets()} retrieves settings by user (user) and computer (nodename) used
#' @return current value for x
#' @examples \dontrun{
#' tobedone
#' }
#' @export
GetCapriSets <- function() {
  
  load(".caprisets.Rdata")
  s <<- s
  sdesc <<- sdesc
  
  
}

#' Retrive default CAPRI sets (check out meanig of sets in gams)
#' @description The function \code{GetCapriSets()} retrieves settings by user (user) and computer (nodename) used
#' @return current value for x
#' @examples \dontrun{
#' setfile <- "x:\\dev\\leipadr\\results\\sets\\dumpcapdis_EndOfCapdis.gdx"
#' UpdateCapriSets(setfile)
#' tobedone
#' }
#' @export
UpdateCapriSets <- function(setfile = NULL  # gdx file with required sets
                            ) {
  
  require(gdxrrw)
  if(gdxrrw::igdx() == FALSE){
    gdxrrw::igdx(dirname(Sys.which('gams.exe')))
  }
  
  # Reset sets2retrieve
  sets2get.eles <- vector()
  sets2get.name <- vector()
  maps2get.eles <- vector()
  maps2get.name <- vector()
  if(! exists("s")){
    # s stored the sets as vectors
    s <- list()
  }
  if(! exists("sdesc")){
    # sdesc stores the set elements descriptions as vectors by set
    sdesc <- list()
  }
  
  # in CAPRI activities are stored in 'cols'
  #          products are stored in 'rows'
  # cols and rows are used to store parameter for products and activities, respectively
  
  # 1. Activities
  
  sets2get.name <- c(sets2get.name, "All columns in the CAPRI model")
  sets2get.eles <- c(sets2get.eles, "cols")
  
  sets2get.name <- c(sets2get.name, "Crop activities")
  sets2get.eles <- c(sets2get.eles, "mcact")
  
  sets2get.name <- c(sets2get.name, "Animal production activities")
  sets2get.eles <- c(sets2get.eles, "maact")
  
  sets2get.name <- c(sets2get.name, "Production activities in supply model")
  sets2get.eles <- c(sets2get.eles, "mpact")
  
  sets2get.name <- c(sets2get.name, "Aggregated aninam production activities")
  sets2get.eles <- c(sets2get.eles, "daact")

  #lapmactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "lapmact")
  #lapmact_fssactexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "lapmact_fssact")
  #feed_rowsexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "FEED_ROWS")
  #fert_distexp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "fert_dist")
  #feed_to_o<-(rgdx.set(setfile,ts = TRUE,symName = "FEED_TO_O"))
  #frmbal_cols<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "frmbal_cols"))
  #mrkbal_cols<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "MRKBAL_COLS"))
  #ncnc_posexp<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NCNC_POS"))
  #ncnc_pos<-as.character(ncnc_posexp[,1])
  #nbil<-(rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NBIL"))
  #nbil_exp<-rgdx.set(setfile,te=TRUE,ts = TRUE,symName = "NBIL_COLS_MAIN")
  #nbil<-as.character(nbil_exp[,1])
  

  # 2. Products
  sets2get.name <- c(sets2get.name, "All CAPRI rows")
  sets2get.eles <- c(sets2get.eles, "ROWS")
  
  sets2get.name <- c(sets2get.name, "Final crop and animal outputs")
  sets2get.eles <- c(sets2get.eles, "FROWS")
  
  sets2get.name <- c(sets2get.name, "Intermediate crop products. Non-marketable like GRAS, fodder maize etc.")
  sets2get.eles <- c(sets2get.eles, "ICO")
  
  sets2get.name <- c(sets2get.name, "Final crop outputs")
  sets2get.eles <- c(sets2get.eles, "FCO")
  
  sets2get.name <- c(sets2get.name, "Activies as in the FSS data base")
  sets2get.eles <- c(sets2get.eles, "fssacts")
  
  sets2get.name <- c(sets2get.name, "Young animals")
  sets2get.eles <- c(sets2get.eles, "oyani_rows")
  
  sets2get.name <- c(sets2get.name, "Animal outputs incl young animals, manure nutrients and livestock residues")
  sets2get.eles <- c(sets2get.eles, "animo_rows")

  sets2get.name <- c(sets2get.name, "Processed / secondary products")
  sets2get.eles <- c(sets2get.eles, "seco_rows")

  sets2get.name <- c(sets2get.name, "Soil balance elements")
  sets2get.eles <- c(sets2get.eles, "SOILBALPOS_R")
  
  sets2get.name <- c(sets2get.name, "Nitrogen balance")
  sets2get.eles <- c(sets2get.eles, "NBIL")
  
  sets2get.name <- c(sets2get.name, "Nutrients in food")
  sets2get.eles <- c(sets2get.eles, "NCNC_POS")
  
  sets2get.name <- c(sets2get.name, "Products in the market model")
  sets2get.eles <- c(sets2get.eles, "xx")
  
  
  
  
  # Regions
  # Countries and regions
  sets2get.name <- c(sets2get.name, "All countries in the supply model (EU plus some others)")
  sets2get.eles <- c(sets2get.eles, "nuts0")
  
  sets2get.name <- c(sets2get.name, "EU15-countries")
  sets2get.eles <- c(sets2get.eles, "nuts0_eu15")
  sets2get.name <- c(sets2get.name, "EU10-countries")
  sets2get.eles <- c(sets2get.eles, "nuts0_eu10")
  
  sets2get.name <- c(sets2get.name, "NUTS2 regions")
  sets2get.eles <- c(sets2get.eles, "srnuts2")
  
  sets2get.name <- c(sets2get.name, "All regions in market model")
  sets2get.eles <- c(sets2get.eles, "rall")
  
  cat("\n", sets2get.eles)
  
  # 4. Mappings
  maps2get.name <- c(maps2get.name, "Mapping between data base activities from COCO and high/low yield variants used in CAPREG/CAPMOD")
  maps2get.eles <- c(maps2get.eles, "dact_to_pact")
  
  cat("\nRetrieve sets... ")
  s <- lapply(1 : length(sets2get.eles), function(x) {
    cat(" ", x, ":", sets2get.eles[x])
    e <- NULL
    try( e <- rgdx.set(gdxName = setfile, 
             te = TRUE,           # Include the associated text for each set element
             ts = TRUE,           # Include the explanatory text for the symbol
             symName = sets2get.eles[x]), 
         silent = TRUE)
    if(! is.null(e)){
      e <- as.character(e[, 1])     # Store element name
    }
  })
  cat("\nRetrieve set descriptions... ")
  sdesc <- Reduce(rbind, lapply(1 : length(sets2get.eles), function(x) {
    cat(" ", sets2get.eles[x])
    e <- NULL
    try(e <- rgdx.set(gdxName = setfile, 
             te = TRUE,           # Include the associated text for each set element
             ts = TRUE,           # Include the explanatory text for the symbol
             symName = sets2get.eles[x]), 
        silent = TRUE)
    if(! is.null(e)){
      e <- as.data.table(e)         # Store element description
      setnames(e, names(e), c("element", "description"))
      e <- e[! is.na(e[, description])]
      e$set <- sets2get.name[x]
      e <- e[, .(set, element, description)]
    }else{
      e <- data.table(set = sets2get.name[x], element = "not available", description = "not available")
    }
    return(e)
  }))
  
  m <- Reduce(rbind, lapply(1 : length(maps2get.eles), function(x) {
    e <- rgdx.set(gdxName = setfile, 
                  te = TRUE,           # Include the associated text for each set element
                  ts = TRUE,           # Include the explanatory text for the symbol
                  symName = maps2get.eles[x])
    e <- as.data.table(e)[, 1:2]       # Store element description
    n <- names(e)
    e$set <- gsub("_to_", "2", maps2get.eles[x])
    
    View(e, title = as.character(x))
    return(e)
  }))
    
  # Apply R-styles to set names. Note that set elements remain in CAPRI style.
  elementnames <- tolower(sets2get.eles)
  elementnames <- gsub("_rows", "", elementnames)
  elementnames <- gsub("_r", "", elementnames)
  
  names(s) <- elementnames
  
  # Add sets with set-arithmetic without retrieving
  s$mcactnohighyild <- setdiff(s$mcact, c("NURS", "FLOW", "TOMA"))
  s$cropo <- c(s$fco, s$ico)
  s$oseco <- c(s$fco, s$ico, s$animo, s$seco)
  s$mbal <- c("GROF", "HCOM", "FEDM", "IMPT", "EXPT", "INDM", "PRCM")
  s$nbalsoil <- c("SURSOI", "SURTOT", "MINFER", "MANAPP", "MANGRA", "BIOFIX", "ATMOSD", "CRESID")
  
  s$nuts2 <- substr(s$srnuts2,1,4)
  s$regi  <- c(s$nuts0, s$srnuts2)
  

  # The sets below still need to be included above as they are relevant 
  # for several processing tasks
  #
  
  # Nitrogen and GHG relevant sets
  # if(exists("setfilen")){
  #   nflowsr_exp<-rgdx.set(setfilen,te=TRUE,ts = TRUE,symName = "nflowsr")
  #   nflowsr<-as.character(nflowsr_exp[,1])
  #   nemiscadd_exp<-rgdx.set(setfilen,te=TRUE,ts = TRUE,symName = "Nemiscadd")
  #   nemiscadd<-as.character(nemiscadd_exp[,1])
  # }
  # 
  # meta2keep<-c("DATE OF VERSION","NAME OF PROCESSOR ORGANISATION","User","Regional breakdown")
  # 
  
  # Push sets to the global environment
  s <<- s
  sdesc <<- sdesc
  
  save(s, sdesc, m, file = ".caprisets.Rdata")
  invisible()
}


startextract<-function(scope){
  
  #Check the current folder
  curfol <- getwd()
  if( ! grepl("capriextract", curfol) ){
    # Wrong folder
    if( grepl("logfiles", curfol) ){
      setwd("../capriextract/")
    }else(
      stop("You seem to be in a wrong folder. Please change to 'capriextract'!")
    )
  }
  
  scope <<- scope
  
  #source("capri_packages.r")
  #source("capri_dirs.r")
  #source("capri_sets.r")
  source("capriextract_functions.r")
  source("xobsfunctions.r")
  source("capriextract_functions_4mapping.r")
  source("capriplotcolors.r")
  source("capriplottexts.r")
  source("capriplots.r")
  
}
