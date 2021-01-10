#' Extraction of disaggregated CAPRI timeseries data
#' 
#' Preparation of a high resolution dataset of the Nitrogen Budget and Nitrogen Balance, 
#' as well as of GHG emissions for the KIP-INCA and VERIFY projects.
#'
#' @param capri.runfile String. Link to a CAPRI runfile that contains the CAPRI setting describing run parameters
#'  such as input and output folders, parameter settings etc. The information will be saved for each run.
#' @param savedate String.  
#' @param curcountries String vector. Contains the regions that have been disaggregated and/or that 
#'   are to be extracted. 
#' @param curyears String vector. Contains years of the time series to be extracted.
#' @param baseyear String. Current CAPRI base year - last two digits.
#' @param reginame String. Name of the result files, usually an acronym of the region set but can be any
#'   other name indentifying the current disaggregation.
#'   
#' @return Data table. Main purpose is to write out result data in *rdata and *csv formats and perform checks. 
#'   Upon completion the data table containing N-budet data will be returned if successful.
#'   
#' @author Adrian Leip \email{adrian.leip@ec.europa.eu}
#' @references Leip, A., Koeble, R., 2018 The CAPRI disaggregation. Report in preparation
#' @referemces Leip, A., Koeble, R., Reuter, H.I., Lamboni, M., Homogeneous Spatial Units (HSU) - a Pan-European geographical basis for environmental and socio-economic modelling. PANGAEA. https://doi.org/10.1594/PANGAEA.860284, Unpublished data archive
#' @references Lamboni, M., Koeble, R., Leip, A., 2016. Multi-scale land-use disaggregation modelling: Concept and application to EU countries. Environ. Model. Softw. 82, 183-217. https://doi.org/10.1016/j.envsoft.2016.04.028
#' @references Leip, A., Marchi, G., Koeble, R., Kempen, M., Britz, W., Li, C., 2008. Linking an economic model for European agriculture with a mechanistic model to estimate nitrogen and carbon losses from arable soils in Europe. Biogeosciences 5, 73-94. https://doi.org/10.5194/bg-5-73-2008
#' 
#' @export
#' 
#' @examples


extract4kipinca <- function(
  
  capri.runfile = NULL, 
  
  savedate = NULL, 
  
  curcountries = NULL, 
  curyears = '2012',
  baseyear = '12',
  
  reginame = NULL
  
  ){
  
  
  if(is.null(savedate)){savedate <- paste0("_",format(Sys.time(), "%Y%m%d"))}
  yearrange <- paste0(min(curyears), "-", max(curyears))
  
  # 1. Run 'initializecapri.R' (file ./R/initializecapri.R)
  #    - indicating the cur_run.gms file. This is important as it contains all the settings.
  #      therefore prepare by chosing the cur_run.gms file & rename it properly.
  #      NEW: Add comments. Aslo startextractcapdis() not called automatically
  source("R/initializecapri.R")
  InitCapriEnv(capri.runfile = "cur_ru_13.gms", 
               addcomment = "Test of disaggregation chain after making changes (see commit entries). Only few NUTS.")
  
  # 2. Run 'startextractcapdis' (file: capdistseries_function.r)
  startextractcapdis()
  
  # 3. Start extraction
  #    Note that below steps are checked one-by-one.
  # Sequence (<-- means: is RH is called by L:H):
  # extractall  <-- loadcurfile (both in: capdistseries_function.r)
  #             <-- filtermultiple (file: openfiltermultiple.r)
  #             <-- filteropen (file: openfiltermultiple.r)
  #             <-- opendata (file: openfiltermultiple.r)
  xobshsu<-extractall(reginame=reginame, scope="capdis", cols=mcactuaar, ydim=NULL, #curdim5=NULL, 
                      curcountries=curcountries, regi=NULL, curyears=curyears, baseyear='12', 
                      curscens='', curscensshort='')
  
  # 4. Checkregions makes only sense of all EU regions are calculated
  if(reginame=="EU27") {checkregions(folderdate=folderdate, savedate=savedate)}
  
  # 5. Combine data for KIP-INCA : extract relevant data that are submitted to the EEA
  #    Note 20210108 off-farm grazing data are not yet included
  # done <- combinedata4kipinca(date2load=savedate, reginame = reginame, yearrange = yearrange)
  
  # 6. Perform checks on distributions and outliers and clean very small (delete) and high MANAPP (put cap) values
  #    Some additional tests are carried out below with the nbudget data loaded. If still necessary 
  #    needs to be added to the cleandate4kipinca function
  cleaned <- cleandate4kipinca(folderdate=folderdate , extractiondate = savedate, reginame = reginame, yearrange = yearrange)
  
  # 7. Write out in the format agreed with EEA
  # Note 20200108 the function requires 'HeadperHa' which was loaded from p_livestock
  #      This is currently not extracted and needs to be added
  cat("\n", paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot", savedate, "_dcastclean.rdata"))
  load(     paste0(savepath, "/xobs_", reginame, "_", yearrange, "_GHGNtot", savedate, "_dcastclean.rdata"))
  wrapoverwrite(x = nbudget[, -c("CAPRINUTS2", "CNTR_CODE"), with=FALSE], reginame = reginame, yearrange=yearrange)
  writelivestock(paste0(savepath, "/xobs_", reginame, "_", yearrange, "_LIVESTOCK_", format(Sys.time(), "%Y%m%d"), ".rdata"), reginame = reginame)
  
  dohistos(nbudget)
  
  return(nbudget)
}
