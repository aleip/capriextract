#' Read capri output from a gdx file
#' @description The function \code{readcapmod()} is a wrapper around \code{gdxrrw::rgdx.param()}
#' @param gdxfile character name of a gdx file
#' @param symName character name of the GDX symbol to read (argument passed to \code{gdxrrw::rgdx.param())
#' @param names character vector of column names (argument passed to \code{gdxrrw::rgdx.param())
#' @return a data frame containing data from the gdx file.
#' @examples \dontrun{
#' library(gdxrrw) # Load the interface between 'GAMS' and R
#' # Specify the GAMS path on windows
#' igdx(gamsSysDir = "E:\\dev\\CAPRI\\GAMS\\win64\\24.7")
#' # Specify the GAMS path on Linux
#' igdx(gamsSysDir = "/opt/gams/gams25.1_linux_x64_64_sfx/")
#' 
#' # Specify where gdx files are located
#' datapath <- "E:\\dev\\CAPRI\\star_2.2\\output\\results\\capmod"
#' 
#' # Read one file 
#' firstfile <- file.path(datapath, "res_2_0810mtr_rd_ref.gdx")
#' res_2_0810mtr_rd_ref <- readcaprioutput(firstfile)
#' head(res_2_0810mtr_rd_ref)
#' 
#' # Read all files in the directory and combine them in a single data frame
#' gdxfiles <- list.files(datapath, ".gdx", full.names = TRUE)
#' caprilist <- lapply(gdxfiles, readcaprioutput)
#' capriout <- Reduce(rbind, caprilist)
#' head(capriout)
#' }
#' @export
readcapmod <- function(gdxfile, 
                       symName = "dataout",
                       names = c("RALL","EMPTY","COLS","ROWS","Y","VALUE")){
    if(gdxrrw::igdx() == FALSE){
        message("The GDX library has not been loaded. Use gdxrrw::igdx() to load it.")
        return(invisible(FALSE))
    }
    message("\nReading ", gdxfile)
    capriout <- gdxrrw::rgdx.param(gdxfile, symName = symName, names = names) 
    # Change factor to character variables
    capriout$RALL <- as.character(capriout$RALL)
    capriout$EMPTY <- as.character(capriout$EMPTY)
    capriout$COLS <- as.character(capriout$COLS)
    capriout$ROWS <- as.character(capriout$ROWS)
    # Change year from factor to a numeric variable
    capriout$Y <- as.numeric(as.character(capriout$Y))
    return(capriout)
}
