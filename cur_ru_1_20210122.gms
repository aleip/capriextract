* -------------------------------------------------------------------- *  
*
*   Program code generated by Grapical Interface Converter 
*
*   Workstep        : Disaggregate Results
*   Task            : Disaggregate CAPREG
*   User            : Adrian Leip
*   Time and date   : 2021-01-22 21:39:41
*
* -------------------------------------------------------------------- * 

$SETGLOBAL task Disaggregate CAPREG
$SETGLOBAL workStep Disaggregate Results

$ONTEXT

   Directory settings from ini file

$OFFTEXT

$SETGLOBAL curDir  'D:\dev\CAPRImodel\epnf201907\gams'
$SETGLOBAL curDirR 'D://dev//CAPRImodel//epnf201907//gams'
$SETGLOBAL Resdir  'D:\dev\CAPRImodel\ECAMPA4Results'
$SETGLOBAL ResdirR 'D://dev//CAPRImodel//ECAMPA4Results'
$SETGLOBAL Restartdir  'D:\dev\CAPRImodel\users\leipadr\restart'
$SETGLOBAL RestartdirR 'D://dev//CAPRImodel//users//leipadr//restart'
$SETGLOBAL Datdir  'D:\dev\CAPRImodel\epnf201907\dat'
$SETGLOBAL DatdirR 'D://dev//CAPRImodel//epnf201907//dat'
$SETGLOBAL scrdir 'D:\dev\CAPRImodel\users\leipadr\tempcapdis\1'
$SETGLOBAL GamsStartNo 1
$SETGLOBAL scrdirR 'D://dev//CAPRImodel//users//leipadr//tempcapdis//1'

$ONTEXT

   Executables

$OFFTEXT

$SETGLOBAL GAMSexe  'C:\Apps\GAMS\win64\25.1\gams.exe'
$SETGLOBAL gamsPath C:\Apps\GAMS\win64\25.1\
$SETGLOBAL Rexe  'C:\Apps\R\R-3.5.0\bin\R.exe'
$SETGLOBAL Trollexe  ''

$ONTEXT

   GAMS options

$OFFTEXT

$SETGLOBAL gamsArg  threads 0
$SETGLOBAL NoCPU 28
$SETGLOBAL procSpeedRelative  100
$SETGLOBAL JAVA ON

$ONTEXT

   All settings stored as textual information in s_META set 

$OFFTEXT

$if not set mode_gui $set mode_gui readGamsSymbols
$iftheni.decl '%mode_gui%'=='readGamsSymbols'
SET META_STEP /'Disaggregate CAPREG'/;
SET countriesMeta /
  ES61
/;
SET META /
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'WORKSTEP         ' 'Disaggregate Results'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'NAME OF PROCESSOR ORGANISATION' 'Adrian Leip'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'TEMPORAL COVERAGE' 'null : null'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'DATE OF VERSION' '2021-01-22 21:39:41'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'BASEYEAR'        null
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'LANGUAGE WITHIN THE DATA SET'          ENGLISH
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'NAME OF EXCHANGE FORMAT'               GDX
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'NAME OF OWNER ORGANISATION'            CAPRI network
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'NAME OF ORIGINATOR ORGANISATION'       CAPRI network
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'DESCRIPTION OF PROCESS STEP' 'Disaggregate Results, Disaggregate CAPREG'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'MEMBER_STATES' 
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'GEOGRAPHIC COVERAGE BY NAME' 
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Date and time' '2021-01-22 21:39:41'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'User' 'Adrian Leip' 
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Gams version' 'GAMS.%system.GamsRelease%' 
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'local path modelDir' 'D:\dev\CAPRImodel\epnf201907\gams'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnRev modelDir' '9010'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnURL modelDir' '/svn/capri/branches/epnf/gams'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnLocalMod1' 'D:\dev\CAPRImodel\epnf201907\gams\capdis\disninput.gms'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnLocalMod2' 'D:\dev\CAPRImodel\epnf201907\gams\capdis\dislivestock.gms'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnLocalMod3' 'D:\dev\CAPRImodel\epnf201907\gams\envind\gases.gms'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnLocalMod4' 'D:\dev\CAPRImodel\epnf201907\gams\ghg_emission_factors_per_commodity\define_priors.gms'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'local path datDir' 'D:\dev\CAPRImodel\epnf201907\dat'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnRev datDir' '9010'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnURL datDir' '/svn/capri/branches/epnf/dat'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnLocalMod5' 'D:\dev\CAPRImodel\epnf201907\dat\capdishsu\fssdata\capdis_FR_10GRID.gdx'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'local path resDir' 'D:\dev\CAPRImodel\ECAMPA4Results'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnRev resDir' '370'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'svnURL resDir' '/repos/GHG/ECAMPA4Results'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Disaggregate landuse' 'ON'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Use a priori map from constraining at' 'FSS_grid10km [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Disaggregate livestock density' 'ON'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Calculate soil loss indicator' 'OFF'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Distribution of yield and irrigation share' 'ON'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Disaggregate fertiilizer distribution' 'ON'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Calculate spatial indicators' 'OFF'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Base year' '2012 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Nuts2' 'ES61'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Minimum relative crop share' '0.0 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Suppression of crops if the share is very low' '0.0 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Relative stde for land that is asssumed to be known' '0.01 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Relative stde for other land' '1.0 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Relative stde for permanent crops' '0.05 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Multiply deviations for crops not predicted before' '10.0 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Use new format 5dim xobs' 'false [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Load meta information from older task' 'true [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Print gams code to listing' 'offListing [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Solprint' 'On'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Limrow' '0.0 [default]'
   (set.countriesMeta).'Disaggregate CAPREG'.'Disaggregate CAPREG'.'Limcol' '0.0 [default]'
 /;
SET allMetaItems 'Title of all controls, tasks and worksteps possible for the GUI' /
  'Installation'
  'Build database'
  'Generate baseline'
  'Run scenario'
  'Disaggregate Results'
  'Tests and Reporting'
  'Create result and restart directory structure'
  'Prepare national database'
  'Finish national database'
  'Find No of fts in Nuts2'
  'Define fts from FSS'
  'Compile RDP data'
  'Build regional time series'
  'Build GHG Inventories'
  'Compare GHG with EEA'
  'Build regional database'
  'Prepare FAOSTAT database'
  'Build global database'
  'Estimate GHG emission coefficients'
  'Generate expost results'
  'Generate trend projection'
  'Generate farm type trends'
  'Baseline calibration market model'
  'Baseline calibration supply models'
  'Baseline calibration farm types'
  'Define scenario'
  'Run scenario with market model'
  'Run scenario without market model'
  'Test alternative market model'
  'Run scenario only with market model'
  'Calibrate CGE'
  'Re-calibrate behavioural functions CGE'
  'Run policy experiment with CGE'
  'Run test shocks with CGE'
  'A priori land use distribution'
  'Disaggregate CAPREG'
  'Disaggregate timeseries'
  'Disaggregate baseline'
  'Disaggregate scenario'
  'Collect disaggregation results'
  'Create experiment folder structure'
  'Collect experiment results'
  'Compare task results'
  'Merge comparison results'
  'Export Scenarios in AgMIP format'
  'Base year selection'
  'Scenario description'
  'Reference scenario'
  'Select SSP Options'
  'Scenario description CGE'
  'Generate GAMS child processes on different threads'
  'Generate CMD per Thread'
  'Constrain a priori data'
  'Constrain a priori data with'
  'Disaggregate landuse'
  'Use a priori map from constraining at'
  'Disaggregate livestock density'
  'Calculate soil loss indicator'
  'Distribution of yield and irrigation share'
  'Disaggregate fertiilizer distribution'
  'Calculate spatial indicators'
  'Disagg-years'
  'Collect disagg-results'
  'Continue previous collection'
  'Delete Nuts2 results'
  'EU28'
  'GHG abatement technology'
  'Technology Package'
  'techOption'
  'Sensitivity EU'
  'Miti Eff EU'
  'Sensitivity Non-EU'
  'Miti Eff Non-EU'
  'GHG abatement initialization'
  'Bilateral model'
  'Only ad valorem equivalents'
  'Use profit functions for countries with programming models'
  'Income fixed'
  'Fixed price of non-agricultural sector'
  'Use pre-steps'
  'Solution algorithm'
  'GHG_per_Country'
  'Pool_GHG_Results'
  'Map_CAPRI_NIR'
  'First inventory year'
  'First ex post year'
  'Base year'
  'Base year farm types'
  'Last inventory year'
  'Last ex post year'
  'Simulation years'
  'Last simulation year'
  'Last year of NUTS2 trends for FTs'
  'Time series'
  'Yields/Input Costs per farm from FADN'
  'Automatic check for selection of years'
  'Only integrate results for single MS'
  'Only map market data from FAO then unload and exit'
  'Run for selected regions'
  'Countries'
  'Consolidation_steps'
  'Disaggregate countries'
  'Nuts2'
  'Regional breakdown'
  'Non-default reference scenario for carbon accounting'
  'Reference scenario for carbon accounting'
  'Non-default FAO trade matrix vintage'
  'FAO trade matrix vintage (determines FAOregions)'
  'Task to analyse'
  'Task not in list'
  'Gams parameter to analyse'
  'Gams parameter not in list'
  'Add comparison output to cumulative list of comparisons'
  'Clear cumulative list of all comparisons done'
  'Name of cumulative comparisons list'
  'First file'
  'Second file'
  'File containing list of comparisons'
  'Path to experiment root'
  'Experiment 1'
  'Experiment 2'
  'Experiment 3'
  'Experiment 4'
  'Experiment 5'
  'Run price shocks country by product'
  'Run tfp shocks globally'
  'Size of tfp shock'
  'Product group selection'
  'Estimate areas, yields and crop production'
  'Estimate animals'
  'Estimate markets'
  'Estimate land use'
  'Country Data Consolidation Steps'
  'Trade Consolidation Steps'
  'Productgroups to consolidate'
  'Macro data'
  'Ref SSP'
  'Taste shifter'
  'Diet data'
  'Dietshock'
  'RCP'
  'CO2 effect'
  'GDP yield shift'
  'CPrice'
  'Tariff change'
  'Policy Type'
  'ghgTrade'
  'ScenDim1Type'
  'ScenDim1Value'
  'ScenDim2Type'
  'ScenDim2Value'
  'ScenDim3Type'
  'ScenDim3Value'
  'Global, spatial multi-commodity model'
  'Math programming supply model'
  'Baseline modus'
  'Expost mode'
  'Re-compile Edgar data'
  'Only compile Edgar'
  'Endogenous bio-fuel markets in global market model'
  'Dampening of high activity level elasticities'
  'Policy blocks (additional geographical layer)'
  'Calibrate non-homothetic Armington-system (creates new simini)'
  'Explicit representation of NTMs'
  'Endogenous margins between trade block and country prices'
  'Advanced tariff aggregation module'
  'Endogenous young animal markets'
  'Update transportcost matrix'
  'Update longrun projections'
  'Advanced tariff aggregation'
  'Longrun Option'
  'Regional CGEs'
  'Number of iterations'
  'Use lower price iterations weights after iteration'
  'Alternative GAMS license file for GHG emission estimation'
  'Aggregates for activities and commodities'
  'Environmental Indicators'
  'Life-cycle assessment for energy'
  'Multi-functionality indicators'
  'Iteration tracking'
  'Sensitivity experiments with features in supply model'
  'Use specific Aglink Scenario settings'
  'Aglink Cosimo model version'
  'Aglink Cosimo scenario name'
  'Scaling to DG Agri baseline'
  'Long run baseline selection'
  'Primes scenarios for biofuel supports'
  'Wage curve elasticity'
  'Prudency factor for DPSV investment rule'
  'Base year CAPRI'
  'Simulation year CAPRI'
  'Baseline scenario CAPRI'
  'Supply elasticities'
  'Transformation elasticity'
  'Demand parameters'
  'Use seperate threads'
  'Endogenous net migration'
  'Fixed budget for factor subsidies'
  'Capital stock'
  'Labor supply'
  'Capital mobility'
  'Labor mobility'
  'Land mobility'
  'Closure current account and trade balance'
  'Closure household account'
  'Closure government account'
  'TFP increase 10%'
  'TFP increase 50%'
  'Factor endowment increase 10%'
  'Investment increase 50%'
  'RD measure increase 10%'
  'RD measure decrease 100%'
  'Additional farm groups from fss from 1.600 onwards'
  'Minimum relative crop share'
  'Suppression of crops if the share is very low'
  'Relative stde for land that is asssumed to be known'
  'Relative stde for other land'
  'Relative stde for permanent crops'
  'Multiply deviations for crops not predicted before'
  'Use new format 5dim xobs'
  'Simulation years (or retrieve over set curfiles)'
  'Basis year for scenarios'
  'Pre-Midpoint year'
  'Post-Midpoint year'
  'Reference simulations'
  'SSPs'
  'Carbon price'
  'Radiative Forcings'
  'Diet Scenarios'
  'Load meta information from older task'
  'Additional input data identifier'
  'Additional result type identifier'
  'Override results_in:'
  'Override results_out:'
  'Override restart_in:'
  'Override restart_out:'
  'Solution printing'
  'Determine point price elasticities'
  'Print gams code to listing'
  'Solprint'
  'Limrow'
  'Limcol'
  'Trace data flow'
  'Maximum number of pre-steps market model'
  'Solution print at preparatory solve'
  'Abort after preparatory solve'
  'Solution print for pre-steps in 1st iteration with abort'
  'Plus iterlim to zero for 1st pre-steps in 1st iteration'
  'Number of presteps before abort'
  'Start threats in visible dos windows'
  'Kill simini file'
  'Create joint trendfile for market calibration'
  'Country selection'
  'Regional level'
  'Simulation year selection'
  'Year selection'
 /;
$endif.decl

$ONTEXT

 ------------------ General settings-------------------------------------------

$OFFTEXT

$SETGLOBAL dosteplanduse ON
$SETGLOBAL fsslevel FSS_grid10km
$SETGLOBAL dosteplivestk ON
$SETGLOBAL dostepsoillos OFF
$SETGLOBAL dostepirriyld ON
$SETGLOBAL dostepfertdis ON
$SETGLOBAL dostepfertnut OFF

* ---- Years and Regions

$SETGLOBAL BaseYear 2012
$SETGLOBAL BaseYear_lst2 12
$SETGLOBAL BaseYear_fst2 20
$SETGLOBAL BaseYear_fst4 2012
$iftheni.decl '%mode_gui%'=='readGamsSymbols'
$onempty

 SET  disCountries(*) "Disaggregate countries" / 
 /;
$offempty

$endif.decl
$SETGLOBAL discountries ON
$SETGLOBAL discountries 
$iftheni.decl '%mode_gui%'=='readGamsSymbols'
$onempty

 SET  selNuts2(*) "Nuts2" / 
 'ES61' 
 /;
$offempty

$endif.decl
$SETGLOBAL selnuts2ES61 ON
$SETGLOBAL selnuts2 ES61
$SETGLOBAL selnuts2_lst2 61
$SETGLOBAL selnuts2_fst2 ES
$SETGLOBAL selnuts2_fst4 ES61

$ONTEXT

 ------------------ Disaggregation options-------------------------------------

$OFFTEXT


$ONTEXT

 ------------------ CAPREG disaggregation options------------------------------

$OFFTEXT

$SETGLOBAL mincropshare 0.0
$SETGLOBAL relcropshare 0.0
$SETGLOBAL relstdefix 0.01
$SETGLOBAL relstdeothe 1.0
$SETGLOBAL relstdeperm 0.05
$SETGLOBAL penalizenewcrops 10.0
$SETGLOBAL newFormat OFF

$ONTEXT

 ------------------ Debug options----------------------------------------------

$OFFTEXT

$SETGLOBAL loadMeta ON
$SETGLOBAL inputDataId 
$SETGLOBAL ResId 
$SETGLOBAL results_in 
$SETGLOBAL results_out 
$SETGLOBAL restart_in 
$SETGLOBAL restart_out 
$offListing
option solprint = On;
$setglobal solprint On
option limrow = 0.0;
$setglobal limrow 0.0
option limcol = 0.0;
$setglobal limcol 0.0

* ---------------------------------
* Setting for executing the task in batch file mode
* --------------------------------- 
$ONTEXT

* ---------------------------------
*
* General settings for batch file mode
*
* --------------------------------- 

  gamsexe = C:\Apps\GAMS\win64\25.1\gams.exe

* Enter here the output directory for the HTML page with the report from the batch mode 
 output dir = D:\dev\CAPRImodel\users\leipadr\tempcapdis

 work dir = D:\dev\CAPRImodel\epnf201907\gams

 Res dir = D:\dev\CAPRImodel\ECAMPA4Results

 Restart dir = D:\dev\CAPRImodel\users\leipadr\restart

 Dat dir = D:\dev\CAPRImodel\epnf201907\dat

 scratch dir =D:\dev\CAPRImodel\users\leipadr\tempcapdis

* ---------------------------------
*
* Task specific settings for batch file mode
*
* --------------------------------- 
*

  task = Disaggregate CAPREG

  Disaggregate landuse = ON
  Use a priori map from constraining at = FSS_grid10km
  Disaggregate livestock density = ON
  Calculate soil loss indicator = OFF
  Distribution of yield and irrigation share = ON
  Disaggregate fertiilizer distribution = ON
  Calculate spatial indicators = OFF
  Base year = 2012
  Disaggregate countries = 
  Nuts2 = ES61
  Minimum relative crop share = 0.0
  Suppression of crops if the share is very low = 0.0
  Relative stde for land that is asssumed to be known = 0.01
  Relative stde for other land = 1.0
  Relative stde for permanent crops = 0.05
  Multiply deviations for crops not predicted before = 10.0
  Use new format 5dim xobs = OFF
  Load meta information from older task = ON
  Additional input data identifier = 
  Additional result type identifier = 
  Override results_in: = 
  Override results_out: = 
  Override restart_in: = 
  Override restart_out: = 
  Print gams code to listing = offListing
  Solprint = On
  Limrow = 0.0
  Limcol = 0.0

  execute=Gamsrun

$OFFTEXT

* end batch execution file
