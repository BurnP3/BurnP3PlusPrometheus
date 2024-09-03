# Clean global environment variables
native_proj_lib <- Sys.getenv("PROJ_LIB")
Sys.unsetenv("PROJ_LIB")

# Check and load packages ----
library(rsyncrosim)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(data.table))

checkPackageVersion <- function(packageString, minimumVersion){
  result <- compareVersion(as.character(packageVersion(packageString)), minimumVersion)
  if (result < 0) {
    updateRunLog("The R package ", packageString, " (", 
         as.character(packageVersion(packageString)), 
         ") does not meet the minimum requirements (", minimumVersion, 
         ") for this version of BurnP3+ Prometheus. Please upgrade this package if the scenario fails to run.", 
         type = "warning")
  } else if (result > 0) {
    updateRunLog("Using a newer version of ", packageString, " (", 
                 as.character(packageVersion(packageString)), 
                 ") than BurnP3+ Prometheus was built against (", 
                 minimumVersion, ").", type = "info")
  }
}

checkPackageVersion("rsyncrosim", "2.0.0")
checkPackageVersion("tidyverse",  "2.0.0")
checkPackageVersion("terra",      "1.5.21")
checkPackageVersion("dplyr",      "1.1.2")
checkPackageVersion("codetools",  "0.2.19")
checkPackageVersion("data.table", "1.14.8")

# Setup ----
progressBar(type = "message", message = "Preparing inputs...")

# Initialize first breakpoint for timing code
currentBreakPoint <- proc.time()

## Check Prometheus installation ----
if (.Platform$OS.type == "unix") {
  stop("Prometheus is currently not supported on Unix systems.")
}

prometheusLocation <- Sys.which("prometheus.exe")
if (prometheusLocation == "") {
  stop("Could not find the Prometheus installation location. Please check that Prometheus is installed correctly.")
}

prometheusVersion <- str_c('powershell "(Get-Item -path ', prometheusLocation, ').VersionInfo.ProductVersion"') %>%
  shell(intern = T)
if (prometheusVersion != "6,2021,12,03") {
  stop("Could not find the correct version of Prometheus. Please ensure that you have installed Prometheus v2021.12.03.")
}

# Find the proj lib directory for prometheus
prometheus_proj_lib <- prometheusLocation %>% dirname %>% file.path("proj_nad/") %>% normalizePath

# Print all spatial environment variables to run log
updateRunLog(paste0("Environment variables:",
                    "\r\nPROJ_LIB: ", Sys.getenv("PROJ_LIB"),
                    "\r\nGDAL_DATA: ", Sys.getenv("GDAL_DATA"),
                    "\r\nprometheus_proj_lib: ", prometheus_proj_lib),
             type = "info")

## Connect to SyncroSim ----

myScenario <- scenario()

# Load run controls and get iterations
RunControl <- datasheet(myScenario, "burnP3Plus_RunControl", returnInvisible = T)
iterations <- seq(RunControl$MinimumIteration, RunControl$MaximumIteration)

# Load remaining datasheets
Multithreading <- datasheet(myScenario, "burnP3PlusPrometheus_PrometheusMultithreading")
BatchOption <- datasheet(myScenario, "burnP3Plus_BatchOption")
ResampleOption <- datasheet(myScenario, "burnP3Plus_FireResampleOption")
DeterministicIgnitionLocation <- datasheet(myScenario, "burnP3Plus_DeterministicIgnitionLocation", lookupsAsFactors = F, optional = T, returnInvisible = T) %>% unique()
DeterministicBurnCondition <- datasheet(myScenario, "burnP3Plus_DeterministicBurnCondition", lookupsAsFactors = F, optional = T, returnInvisible = T) %>% unique()
FuelType <- datasheet(myScenario, "burnP3Plus_FuelType")
FuelTypeCrosswalk <- datasheet(myScenario, "burnP3PlusPrometheus_FuelCodeCrosswalk", lookupsAsFactors = F, optional = T)
ValidFuelCodes <- datasheet(myScenario, "burnP3PlusPrometheus_FuelCode") %>% pull()
SeasonTable <- datasheet(myScenario, "burnP3Plus_Season", lookupsAsFactors = F, optional = T, includeKey = T, returnInvisible = T)
WindGrid <- datasheet(myScenario, "burnP3Plus_WindGrid", lookupsAsFactors = F, optional = T)
GreenUp <- datasheet(myScenario, "burnP3Plus_GreenUp", lookupsAsFactors = F, optional = T)
Curing <- datasheet(myScenario, "burnP3Plus_Curing", lookupsAsFactors = F, optional = T)
FuelLoad <- datasheet(myScenario, "burnP3Plus_FuelLoad", lookupsAsFactors = F, optional = T)
OutputOptions <- datasheet(myScenario, "burnP3Plus_OutputOption", optional = T)
OutputOptionsSpatial <- datasheet(myScenario, "burnP3Plus_OutputOptionSpatial", optional = T)
OutputOptionsSpatialPrometheus <- datasheet(myScenario, "burnP3PlusPrometheus_OutputOptionSpatial", optional = T)
FireZoneTable <- datasheet(myScenario, "burnP3Plus_FireZone")
WeatherZoneTable <- datasheet(myScenario, "burnP3Plus_WeatherZone")

# Import relevant rasters
# - Note that datasheetRaster is avoided as it requires rgdal
# - Under conda, this causes Prometheus to point to the wrong version of GDAL
fuelsRaster <- datasheet(myScenario, "burnP3Plus_LandscapeRasters")[["FuelGridFileName"]] %>% rast()
elevationRaster <- tryCatch(
  datasheet(myScenario, "burnP3Plus_LandscapeRasters")[["ElevationGridFileName"]] %>% rast(),
  error = function(e) NULL
)

## Handle empty values ----
if (nrow(FuelTypeCrosswalk) == 0) {
  updateRunLog("No fuels code crosswalk found! Using default crosswalk for Canadian Forest Service fuel codes.", type = "warning")
  FuelTypeCrosswalk <- read_csv(file.path(ssimEnvironment()$PackageDirectory, "Default Fuel Crosswalk.csv")) %>% as.data.frame()
  saveDatasheet(myScenario, FuelTypeCrosswalk, "burnP3PlusPrometheus_FuelCodeCrosswalk")
}

if (nrow(OutputOptions) == 0) {
  updateRunLog("No tabular output options chosen. Defaulting to keeping all tabular outputs.", type = "info")
  OutputOptions[1, ] <- rep(TRUE, length(OutputOptions[1, ]))
  saveDatasheet(myScenario, OutputOptions, "burnP3Plus_OutputOption")
} else if (any(is.na(OutputOptions))) {
  updateRunLog("Missing one or more tabular output options. Defaulting to keeping unspecified tabular outputs.", type = "info")
  OutputOptions <- OutputOptions %>%
    replace(is.na(.), TRUE)
  saveDatasheet(myScenario, OutputOptions, "burnP3Plus_OutputOption")
}

if (nrow(OutputOptionsSpatial) == 0) {
  updateRunLog("No spatial output options chosen. Defaulting to keeping all spatial outputs.", type = "info")
  OutputOptionsSpatial[1, ] <- rep(TRUE, length(OutputOptionsSpatial[1, ]))
  saveDatasheet(myScenario, OutputOptionsSpatial, "burnP3Plus_OutputOptionSpatial")
} else if (any(is.na(OutputOptionsSpatial))) {
  updateRunLog("Missing one or more spatial output options. Defaulting to keeping unspecified spatial outputs.", type = "info")
  OutputOptionsSpatial <- OutputOptionsSpatial %>%
    replace(is.na(.), TRUE)
  saveDatasheet(myScenario, OutputOptionsSpatial, "burnP3Plus_OutputOptionSpatial")
}

if (nrow(OutputOptionsSpatialPrometheus) == 0) {
  updateRunLog("No Prometheus-specific spatial output options chosen. Defaulting to keeping no secondary spatial outputs.", type = "info")
  OutputOptionsSpatialPrometheus[1, ] <- rep(FALSE, length(OutputOptionsSpatialPrometheus[1, ]))
  saveDatasheet(myScenario, OutputOptionsSpatialPrometheus, "burnP3PlusPrometheus_OutputOptionSpatial")
}

if(nrow(BatchOption) == 0) {
  updateRunLog("No batch size chosen. Defaulting to batches of 250 iterations.", type = "info")
  BatchOption[1,] <- c(250)
  saveDatasheet(myScenario, BatchOption, "burnP3Plus_BatchOption")
}

if (nrow(ResampleOption) == 0) {
  updateRunLog("No Minimum Fire Size chosen.\nDefaulting to a Minimum Fire Size of 1ha.\nPlease see the Fire Resampling Options table for more details.", type = "info")
  ResampleOption[1, ] <- c(1, 0)
  saveDatasheet(myScenario, ResampleOption, "burnP3Plus_FireResampleOption")
}

if (nrow(GreenUp) == 0) {
  GreenUp[1, ] <- c(NA, TRUE)
  saveDatasheet(myScenario, GreenUp, "burnP3Plus_GreenUp")
} else if (is.character(GreenUp$GreenUp)) GreenUp$GreenUp <- GreenUp$GreenUp != "No"

if (nrow(Curing) == 0) {
  Curing[1, ] <- c(NA, 75L)
  saveDatasheet(myScenario, Curing, "burnP3Plus_Curing")
}

if(nrow(FireZoneTable) == 0)
  FireZoneTable <- data.frame(Name = "", ID = 0)
if(nrow(WeatherZoneTable) == 0)
  WeatherZoneTable <- data.frame(Name = "", ID = 0)

## Check raster inputs for consistency ----

test.point <- vect(matrix(crds(fuelsRaster)[1,],ncol=2), crs = crs(fuelsRaster))
# Ensure fuels crs can be converted to Lat / Long
if(test.point %>% is.lonlat){stop("Incorrect coordinate system. Projected coordinate system required, please reproject your grids.")}
tryCatch(test.point %>% project("epsg:4326"), error = function(e) stop("Error parsing provided Fuels map. Cannot calculate Latitude and Longitude from provided Fuels map, please check CRS."))

# Define function to check input raster for consistency
checkSpatialInput <- function(x, name, checkProjection = T, warnOnly = F) {
  # Only check if not null
  if (!is.null(x)) {
    # Ensure comparable number of rows and cols in all spatial inputs
    if (nrow(fuelsRaster) != nrow(x) | ncol(fuelsRaster) != ncol(x)) {
      if (warnOnly) {
        updateRunLog("Number of rows and columns in ", name, " map do not match Fuels map. Please check that the extent and resolution of these maps match.", type = "warning")
        invisible(NULL)
      } else {
        stop("Number of rows and columns in ", name, " map do not match Fuels map. Please check that the extent and resolution of these maps match.")
      }
    }

    # Info if CRS is not matching
    if (checkProjection) {
      if (crs(x) != crs(fuelsRaster)) {
        updateRunLog("Projection of ", name, " map does not match Fuels map. Please check that the CRS of these maps match.", type = "info")
      }
    }
  }

  # Silently return for clean pipelining
  invisible(x)
}

# Check optional inputs
checkSpatialInput(elevationRaster, "Elevation")

## Set constants ----

# Names and codes of Prometheus-specific secondary outputs
outputComponentNames <- c("RateOfSpread", "FireIntensity", "SpreadDirection", "SurfaceFuelConsumption", "CrownFractionBurned", "CrownFractionConsumed", "TotalFuelConsumption")
outputComponentCodes <- c("ros", "fi", "raz", "sfc", "cfb", "cfc", "tfc")

# Parameter file template place holders
parameterFilePlaceHolders <- list(
  fileTag     = "fileTagPlaceHolder",
  lon         = "lonPlaceHolder",
  lat         = "latPlaceHolder",
  weatherFile = "weatherFilePlaceHolder",
  greenup     = "greenupPlaceHolder",
  grassCuring = "grassCuringPlaceHolder",
  fuelLoad    = "fuelLoadPlaceHolder",
  duration    = "durationPlaceHolder")

## Extract relevant parameters ----

# Check if multithreading enabled
numThreads <- Multithreading$ThreadsPerIteration

# Batch size for batched runs
batchSize <- BatchOption$BatchSize

# Determine which, if any, extra ignitions (in iteration 0) this job is responsible for burning
extraIgnitionIDs <- DeterministicIgnitionLocation %>%
    filter(Iteration == 0) %>%
    pull(FireID)

# Define function to determine if the current job is multiprocessed
getRunContext <- function() {
  libraryPath <- ssimEnvironment()$LibraryFilePath %>% normalizePath()
  libraryName <- libraryPath %>% basename %>% {tools::file_path_sans_ext(.)}

  # Libraries are identified as remote if the path includes the Parallel folder and library follows the Job-<jobid> naming convention
  isParallel <- libraryPath %>%
    str_split("/|(\\\\)") %>%
    pluck(1) %>%
    str_detect("MultiProc") %>%
    any %>%
    `&`(str_detect(libraryName, "Job-\\d"))

  # Return if false
  if (!isParallel)
    return(list(isParallel = F, numJobs = 1, jobIndex = 1))

  # Otherwise parse number of jobs and current job index
  numJobs <- libraryPath %>%
    dirname() %>%
    list.files("Job-\\d+.ssim.temp") %>%
    length()
  jobIndex <- str_extract(libraryName, "\\d+") %>% as.integer()

  return(list(isParallel = T, numJobs = numJobs, jobIndex = jobIndex))
}

# Determine if jobs are being multiprocessed
runContext <- getRunContext()

# Determine which subset of the extra iterations this job is responsible for
if(runContext$numJobs > 1 & length(extraIgnitionIDs) > 0)
  extraIgnitionIDs <- split(extraIgnitionIDs, 
                            cut(seq_along(extraIgnitionIDs), 
                                runContext$numJobs, labels = F)) %>% 
  pluck(as.character(runContext$jobIndex))

# Filter deterministic tables accordingly

DeterministicIgnitionLocation <- DeterministicIgnitionLocation %>%
  filter(Iteration %in% iterations | (Iteration == 0 & FireID %in% extraIgnitionIDs))
DeterministicBurnCondition <- DeterministicBurnCondition %>%
  filter(Iteration %in% iterations | (Iteration == 0 & FireID %in% extraIgnitionIDs))

# Burn maps must be kept to generate summarized maps later, this boolean summarizes
# whether or not burn maps are needed
saveBurnMaps <- any(OutputOptionsSpatial$BurnMap, OutputOptionsSpatial$SeasonalBurnMap,
                    OutputOptionsSpatial$BurnProbability, OutputOptionsSpatial$SeasonalBurnProbability,
                    OutputOptionsSpatial$RelativeBurnProbability, OutputOptionsSpatial$SeasonalRelativeBurnProbability,
                    OutputOptionsSpatial$BurnCount, OutputOptionsSpatial$SeasonalBurnCount,
                    OutputOptionsSpatial$AllPerim)

# Decide whether or not to save outputs seasonally
saveSeasonalBurnMaps <- any(OutputOptionsSpatial$SeasonalBurnMap,
                            OutputOptionsSpatial$SeasonalBurnProbability,
                            OutputOptionsSpatial$SeasonalRelativeBurnProbability,
                            OutputOptionsSpatial$SeasonalBurnCount)

minimumFireSize <- ResampleOption$MinimumFireSize

# Combine fuel type definitions with codes if provided
if (nrow(FuelTypeCrosswalk) > 0) {
  FuelType <- FuelType %>%
    left_join(FuelTypeCrosswalk, by = c("Name" = "FuelType"))
} else {
  FuelType <- FuelType %>%
    mutate(Code = Name)
}

# Decide whether or not to manually set grass fuel loading
useWindGrid <- !all(is.na(WindGrid))

# Decide whether or not to manually set grass fuel loading and curing
setFuelLoad <- nrow(FuelLoad) > 0
setGrassCuring <- nrow(Curing) > 0

## Error check fuels ----

# Ensure all fuel types are assigned to a fuel code
if (any(is.na(FuelType$Code))) {
  stop(
    "Could not find a valid Prometheus Fuel Code for one or more Fuel Types. Please add Prometheus Fuel Code Crosswalk records for the following Fuel Types in the project scope: ",
    FuelType %>% filter(is.na(Code)) %>% pull(Name) %>% str_c(collapse = "; ")
  )
}

# Ensure all fuel codes are valid
# - This should only occur if the Fuel Code Crosswalk is empty and Fuel Type names are being used as codes
if (any(!FuelType$Code %in% ValidFuelCodes)) {
  stop("Invalid fuel codes found in the Fuel Type definitions. Please consider setting an exlicit Fuel Code Crosswalk for Prometheus in the project scope.")
}

# Ensure that there are no fuels present in the grid that are not tied to a valid code
fuelIdsPresent <- unique(fuelsRaster) %>% pull()
if (any(!fuelIdsPresent %in% c(FuelType$ID, NaN))) {
  stop(
    "Found one or more values in the Fuels Map that are not assigned to a known Fuel Type. Please add definitions for the following Fuel IDs: ",
    setdiff(fuelIdsPresent, FuelType$ID) %>% str_c(collapse = " ")
  )
}

## Setup files and folders ----

# Create temp folder, ensure it is empty
tempDir <- ssimEnvironment()$TempDirectory %>%
  str_replace_all("\\\\", "/") %>%
  file.path("growth-pandora/")
unlink(tempDir, recursive = T, force = T)
dir.create(tempDir, showWarnings = F)

weatherFolder <- file.path(tempDir, "Weathers")
unlink(weatherFolder, recursive = T, force = T)
dir.create(weatherFolder, showWarnings = F)

# Set names for model input files to be created
fuelsRasterAscii <- file.path(tempDir, "fuels.asc")
fuelsRasterProjection <- file.path(tempDir, "fuels.prj")
fuelLookup <- file.path(tempDir, "fuels.lut")
parameterFile <- file.path(tempDir, "parameters.txt")

# Create folders for various outputs
gridOutputFolder <- file.path(tempDir, "grids")
shapeOutputFolder <- file.path(tempDir, "shapes")
accumulatorOutputFolder <- file.path(tempDir, "accumulator")
seasonalAccumulatorOutputFolder <- file.path(tempDir, "accumulator-seasonal")
secondaryOutputFolder <- file.path(tempDir, "secondary")
allPerimOutputFolder <- file.path(tempDir, "allperim")
dir.create(gridOutputFolder, showWarnings = F)
dir.create(shapeOutputFolder, showWarnings = F)
dir.create(accumulatorOutputFolder, showWarnings = F)
dir.create(seasonalAccumulatorOutputFolder, showWarnings = F)
dir.create(secondaryOutputFolder, showWarnings = F)
dir.create(allPerimOutputFolder, showWarnings = F)

## Function Definitions ----

### Convenience and conversion functions ----

# Function to time code by returning a clean string of time since this function was last called
updateBreakpoint <- function() {
  # Calculate time since last breakpoint
  newBreakPoint <- proc.time()
  elapsed <- (newBreakPoint - currentBreakPoint)["elapsed"]

  # Update current breakpoint
  currentBreakPoint <<- newBreakPoint

  # Return cleaned elapsed time
  if (elapsed < 60) {
    return(str_c(round(elapsed), " seconds"))
  } else if (elapsed < 60^2) {
    return(str_c(round(elapsed / 60, 1), " minutes"))
  } else {
    return(str_c(round(elapsed / 60 / 60, 1), " hours"))
  }
}

# Define a function to facilitate recoding values using a lookup table
lookup <- function(x, old, new) dplyr::recode(x, !!!set_names(new, old))

# Function to delete files in file
resetFolder <- function(path) {
  list.files(path, full.names = T) %>%
    unlink(recursive = T, force = T)
  invisible()
}

# Function to convert a raster and row and column indices to Lat Long
latlonFromRowCol <- function(x, row, col) {
  cellFromRowCol(x, row, col) %>%
    {
      as.points(x, na.rm = F)[.]
    } %>%
    project("EPSG:4326") %>%
    crds() %>%
    return()
}

# Function to convert from latlong to cell index
cellFromLatLong <- function(x, lat, long) {
  # Convert list of lat and long to SpatVector, reproject to source crs
  points <- matrix(c(long, lat), ncol = 2) %>%
    vect(crs = "EPSG:4326") %>%
    project(x)

  # Get vector of cell ID's from points
  return(cells(x, points)[, "cell"])
}

# Get burn area from output asc
getBurnArea <- function(inputFile) {
  if (file.exists(inputFile)) {
  fread(inputFile, header = F, skip = 6, sep = " ") %>%
    as.matrix() %>%
    sum %>%
    return
  } else {
     return(0)
  }
}

# Get burn areas from all generated output files
getBurnAreas <- function(rawOutputGridPaths) {
  # Calculate burn areas for each fire
  burnAreas <- c(NA_real_)
  length(burnAreas) <- length(rawOutputGridPaths)

  burnAreas <- unlist(lapply(rawOutputGridPaths[seq_along(burnAreas)],getBurnArea))

  # Convert pixels to hectares (resolution is assumed to be in meters)
  burnAreas <- burnAreas * (xres(fuelsRaster) * yres(fuelsRaster) / 1e4)
  
  return(burnAreas)
}

# Function to determine which fires should be kept after resampling
getResampleStatus <- function(burnSummary) {
  burnSummary %>%
    mutate(
      ResampleStatus = case_when(
        Area < minimumFireSize ~ "Discarded",
        Iteration == 0         ~ "Extra",
        TRUE                   ~ "Kept"
      )) %>%
    return()
}

# Function to convert, accumulate, and clean up raw outputs
processOutputs <- function(batchOutput, rawOutputGridPaths) {
  # Identify which unique fire ID's belong to each iteration
  # - bind_rows is used to ensure iterations aren't lost if all fires in an iteration are discarded due to size
  batchOutput <- batchOutput %>%
    filter(ResampleStatus == "Kept" | ResampleStatus == "Extra") %>%
    bind_rows(tibble(Iteration = unique(batchOutput$Iteration)))
    
  # Summarize the FireIDs to export by Iteration
  ignitionsToExportTable <- batchOutput %>%
    dplyr::select(Iteration, UniqueBatchFireIndex, FireID, Season) %>%
    group_by(Iteration) %>%
    summarize(UniqueBatchFireIndices = list(UniqueBatchFireIndex),
              FireIDs = list(FireID),
              Seasons = list(Season))
  
  # Generate burn count maps
  for (i in seq_len(nrow(ignitionsToExportTable)))
    generateBurnAccumulators(Iteration = ignitionsToExportTable$Iteration[i], UniqueFireIDs = ignitionsToExportTable$UniqueBatchFireIndices[[i]], burnGrids = rawOutputGridPaths, FireIDs = ignitionsToExportTable$FireIDs[[i]], Seasons = ignitionsToExportTable$Seasons[[i]])
}

# Function to call Pandora on the (global) parameter file
runPandora <- function() {
  # Ensure the correct Proj Lib is being used
  Sys.setenv("PROJ_LIB" = prometheus_proj_lib)

  resetFolder(gridOutputFolder)

  # Note than pandora can't handle spaces in the paramter file path
  # - if there are spaces in tempdir, copy the parameter file to a system temp file
  # - also update paramterFile location in the local scope
  if (str_detect(tempDir, " ")) {
    parameterTempFile <- tempfile(pattern = "pandora_parameter", fileext = ".txt")
    file.copy(parameterFile, parameterTempFile, overwrite = T)
    parameterFile <- parameterTempFile
  }

  ssimEnvironment()$PackageDirectory %>%
    str_replace_all("\\\\", "/") %>%
    str_c("/pandora.exe /silent /nowin ", parameterFile) %>%
    shell()

  # Reset proj lib variable for terra
  Sys.unsetenv("PROJ_LIB")
}

# Function to run one batch of iterations
runBatch <- function(batchInputs) {
  # Generate batch-specific inputs
  # - Unnest and process ignition info
  batchInputs <- unnest(batchInputs, data) %>%
    mutate(UniqueBatchFireIndex = row_number())
  
  # - Unnest and process weather info
  batchWeather <- unnest(batchInputs, data)
  generateWeatherFiles(batchWeather)
  
  # Reset and build parameter file, get list of expected output file tags
  unlink(parameterFile)
  file.create(parameterFile)
  fileTags <- batchInputs %>%
    dplyr::rename(season = Season) %>% # used to avoid a name conflict with the Season datasheet
    pmap_chr(generateParameterFile, placeHolderNames = parameterFilePlaceHolders)

  firstline <- "Landscape_Constant 1"
  content <- readLines(parameterFile)
  newContent <- c(firstline, content)
  writeLines(newContent, parameterFile)

  # Run Pandora on the batch
  runPandora()

  # Get relative paths to all raw outputs
  rawOutputGridPaths <- str_c(gridOutputFolder, "/", fileTags, "_burn.asc")

  # Get burn areas
  burnAreas <- getBurnAreas(rawOutputGridPaths)
  
  # Convert and save spatial outputs as needed
  batchOutput <- batchInputs %>%
    select(Iteration, FireID, UniqueBatchFireIndex, Season) %>%
    mutate(Area = burnAreas) %>%
    getResampleStatus()
    
  # Save GeoTiffs if needed
  if(saveBurnMaps)
    processOutputs(batchOutput, rawOutputGridPaths)
  
  # Clear up temp files
  resetFolder(gridOutputFolder)
  
  # Update Progress Bar
  progressBar("step")
  progressBar(type = "message", message = "Growing fires...")
  
  # Return relevant outputs
  batchOutput %>%
    select(-UniqueBatchFireIndex, -Season) %>%
    return()
}

### File generation functions ----

# Function to convert daily weather data for every day of burning to format
# expected by Pandora and save to file
generateWeatherFile <- function(weatherData, UniqueBatchFireIndex) {
  weatherData %>%
    # To convert daily weather to hourly, we need to repeat each row for every
    # hour burned that day and pad the rest of the day with zeros. To do this,
    # we first generate and append a row of all zeros.
    add_row() %>%
    mutate_all(function(x) c(head(x, -1), 0)) %>%
    # Next we use slice to repeat rows as needed.
    slice(pmap(.,
      function(BurnDay, HoursBurning, ..., zeroRowID) {
        if (BurnDay != 0) {
          c(rep(BurnDay, HoursBurning), rep(zeroRowID, 24 - HoursBurning))
        }
      },
      zeroRowID = nrow(.)
    ) %>%
      unlist()) %>%
    # Next we add in columns of mock date and time since this is requried by Pandora
    mutate(
      date = as.integer((row_number() + 12) / 24) + ymd(20000101),
      date = str_c(day(date), "/", month(date), "/", year(date)),
      time = (row_number() + 12) %% 24
    ) %>%
    # Finally we rename and reorder columns and write to file
    dplyr::select(HOURLY = date, HOUR = time, TEMP = Temperature, RH = RelativeHumidity, WD = WindDirection, WS = WindSpeed, PRECIP = Precipitation, HFFMC = FineFuelMoistureCode, HISI = InitialSpreadIndex, HFWI = FireWeatherIndex, DMC = DuffMoistureCode, DC = DroughtCode, BUI = BuildupIndex) %>%
    fwrite(file.path(weatherFolder, str_c("Weather", UniqueBatchFireIndex, ".txt")))
  invisible()
}

# Function to split deterministic burn conditions into separate weather files by iteration and fire id
generateWeatherFiles <- function(DeterministicBurnCondition){
  # Clear out old weather files if present
  resetFolder(weatherFolder)
  
  # Generate files as needed
  DeterministicBurnCondition %>%
    group_by(Iteration, FireID, UniqueBatchFireIndex) %>%
    nest() %>%
    ungroup() %>%
    arrange(Iteration, FireID, UniqueBatchFireIndex) %>%
    dplyr::select(weatherData = data, UniqueBatchFireIndex = UniqueBatchFireIndex) %>%
    pmap(generateWeatherFile)
  invisible()
}

# Function to generate Pandora paramter file template for single fire
generateParamaterTemplate <- function(placeHolderNames){
  # Build the parameter file line-by-line
  parameterFileTemplate <- c(
    str_c("--- Fire ", placeHolderNames$fileTag, " ---"),
    str_c("Fire_name ", placeHolderNames$fileTag),
    str_c("Projection_File ", fuelsRasterProjection),
    str_c("FBP_GridFile ", fuelsRasterAscii),
    if (!is.null(elevationRaster)) {
      str_c("Elev_GridFile ", sources(elevationRaster))
    } else {
      NA
    },
    str_c("Fuel_Table ", fuelLookup),
    str_c("Ign_DateTime 1/1/2000:13:00:00"),
    str_c("Ign_Lon ", placeHolderNames$lon),
    str_c("Ign_Lat ", placeHolderNames$lat),
    str_c("WxStation_Lon ", weatherStationLocation[1]),
    str_c("WxStation_Lat ", weatherStationLocation[2]),
    str_c("WxStation_Elev ", weatherStationElevation),
    str_c("Wx_file ", placeHolderNames$weatherFile),
    str_c("Init_hour 13"),
    str_c("FFMC_Method 5"),
    str_c("Threads ", numThreads),
    if (useWindGrid) {
      WindGridParameterStrings
    } else {
      NA
    },
    str_c("Greenup ", placeHolderNames$greenup),
    if (setGrassCuring) {
      str_c("%Curing_GridFile ", placeHolderNames$grassCuring)
    } else {
      NA
    },
    if (setFuelLoad) {
      str_c("Fuel_Load_GridFile ", placeHolderNames$fuelLoad)
    } else {
      NA
    },
    str_c("Duration  ", placeHolderNames$duration),
    str_c("Export_Every ", placeHolderNames$duration)
  ) %>%
    discard(is.na)

  # Choose which outputs to save based on chosen output options
  if (OutputOptionsSpatial$BurnPerimeter) {
    parameterFileTemplate <- parameterFileTemplate %>%
      c(str_c("Out_ShapeFiles ", file.path(shapeOutputFolder, placeHolderNames$fileTag), "_"))
  }
  if (saveBurnMaps) {
    parameterFileTemplate <- parameterFileTemplate %>%
      c(
        str_c("Out_GridFiles ", file.path(gridOutputFolder, placeHolderNames$fileTag)),
        str_c("Out_Components ", outputComponents)
      )
  }

  return(parameterFileTemplate)
}

# Function to generate Pandora parameter file based on rows of the fireGrowthInputs dataframe
generateParameterFile <- function(Iteration, FireID, UniqueBatchFireIndex, season, Lat, Lon, data, placeHolderNames) {
  # Define a unique identifier to name files
  fileTag <- str_c("it", Iteration, ".fid", FireID)

  # Calculate values to fill placeholders in template
  weatherFile <- file.path(weatherFolder, str_c("Weather", UniqueBatchFireIndex, ".txt"))

  greenupValue <-  GreenUp %>%
    filter(Season %in% c(season, NA)) %>%
    arrange(Season) %>% pull(GreenUp) %>%
    pluck(1) %>%
    as.numeric()

  if(setGrassCuring) {
    grassCuringValue <- Curing %>%
      filter(Season %in% c(season, NA)) %>%
      arrange(Season) %>%
      pull(FileName) %>%
      pluck(1)
  } else {
     grassCuringValue <- NA
  }

  if(setFuelLoad) {
    fuelLoadValue <- FuelLoad %>%
      filter(Season %in% c(season, NA)) %>%
      arrange(Season) %>%
      pull(FileName) %>%
      pluck(1)
  } else {
     fuelLoadValue <- NA
  }

  durationValue <- max(data$BurnDay) * 24L - 1

  # Replace placeholders in template
  parameterFileText <- parameterFileTemplate %>%
    str_replace_all(placeHolderNames$fileTag, fileTag) %>%
    str_replace_all(placeHolderNames$lon, as.character(Lon)) %>%
    str_replace_all(placeHolderNames$lat, as.character(Lat)) %>%
    str_replace_all(placeHolderNames$weatherFile, weatherFile) %>%
    str_replace_all(placeHolderNames$greenup, as.character(greenupValue)) %>%
    str_replace_all(placeHolderNames$grassCuring, as.character(grassCuringValue)) %>%
    str_replace_all(placeHolderNames$fuelLoad, as.character(fuelLoadValue)) %>%
    str_replace_all(placeHolderNames$duration, as.character(durationValue))

  # Open and append to parameter file
  outputFile <- file(parameterFile, "a")
  writeLines(parameterFileText, outputFile)
  close(outputFile)

  return(fileTag)
}

# Function to summarize individual burn grids by iteration
generateBurnAccumulators <- function(Iteration, UniqueFireIDs, burnGrids, FireIDs, Seasons) {
  # For iteration zero (fires for resampling), only save individual burn maps
  if(Iteration == 0) {
    for(i in seq_along(UniqueFireIDs)){
      if(!is.na(UniqueFireIDs[i])){
        burnArea <- as.matrix(fread(burnGrids[UniqueFireIDs[i]], header = F, skip = 6, sep = " "))

        rast(fuelsRaster, vals = burnArea) %>% 
          mask(fuelsRaster) %>%
          writeRaster(str_c(allPerimOutputFolder, "/it", Iteration,"_fire_", FireIDs[i], ".tif"), 
              overwrite = T,
              NAflag = -9999,
              wopt = list(filetype = "GTiff",
                    datatype = "INT4S",
                    gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
      }
    }
    return()
  }

  # initialize empty matrix for overall accumulator
  accumulator <- matrix(0, nrow(fuelsRaster), ncol(fuelsRaster))

  # initialize a list of empty matrices for each season
  seasonValues <- SeasonTable %>%
    filter(Name != "All") %>%
    pull(Name) %>%
    unique
  seasonalAccumulators <- accumulator %>% 
    list() %>%
    rep(length(seasonValues)) %>%
    set_names(seasonValues)
  
  # Combine burn grids
  for(i in seq_along(UniqueFireIDs)){
    if(!is.na(UniqueFireIDs[i])){
      # Pandora occassionally doesn't produce an output. Possibly when there is truly no burn?
      if(file.exists(burnGrids[UniqueFireIDs[i]])) {
        # Read and add in the current burn map to the accumulator
        burnArea <- as.matrix(fread(burnGrids[UniqueFireIDs[i]], header = F, skip = 6, sep = " "))
        accumulator <- accumulator + burnArea

        # Add to seasonal accumulator
        if(saveSeasonalBurnMaps) {
          thisSeason <- Seasons[i]
          if (thisSeason %in% seasonValues)
            seasonalAccumulators[[thisSeason]] <- seasonalAccumulators[[thisSeason]] + burnArea
        }
        
        # Save individual fire map if requested
        if(OutputOptionsSpatial$AllPerim == T){
          rast(fuelsRaster, vals = burnArea) %>% 
            mask(fuelsRaster) %>%
              writeRaster(str_c(allPerimOutputFolder, "/it", Iteration,"_fire_", FireIDs[i], ".tif"), 
                  overwrite = T,
                  NAflag = -9999,
                  wopt = list(filetype = "GTiff",
                      datatype = "INT4S",
                      gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
        }

        # Save requested secondary outputs
        fileTag <- str_c("it", Iteration, ".fid", FireIDs[i])
        for (component in outputComponentsToKeep) {
          inputComponentFileName <- str_c(gridOutputFolder, "/", fileTag, "_", lookup(component, outputComponentNames, outputComponentCodes), ".asc")
          if (file.exists(inputComponentFileName)) {
            # Generate output file name
            outputComponentFileName <- file.path(secondaryOutputFolder, basename(inputComponentFileName) %>% str_replace("asc", "tif"))

            # Rewrite as GeoTiff to output folder
            rast(inputComponentFileName) %>%
              writeRaster(outputComponentFileName,
                overwrite = T,
                NAflag = -9999,
                wopt = list(
                  filetype = "GTiff",
                  datatype = "INT4S",
                  gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "PREDICTOR=2")
                )
              )

            # Update corresponding table in SyncroSim
            outputComponentTables[[component]] <<- rbind(
              outputComponentTables[[component]],
              data.frame(
                Iteration = Iteration,
                Timestep = FireIDs[i], # TODO: Separate out timestep and fire ID
                FireID = FireIDs[i],
                FileName = outputComponentFileName
              )
            )
          }
          unlink(inputComponentFileName)
        }
      }
    }
  }

  # Standardize accumulator to binary
  accumulator[accumulator != 0] <- 1

  # Mask and save as raster
  rast(fuelsRaster, vals = accumulator) %>%
    mask(fuelsRaster) %>%
    writeRaster(str_c(accumulatorOutputFolder, "/it", Iteration, ".tif"), 
                overwrite = T,
                NAflag = -9999,
                wopt = list(filetype = "GTiff",
                    datatype = "INT4S",
                    gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
  
    # Repeat for each seasonal accumulator
  if(saveSeasonalBurnMaps) {
    for (season in seasonValues) {
      # Binarize accumulator to burn or not
      seasonalAccumulators[[season]][seasonalAccumulators[[season]] != 0] <- 1

      # Mask and save as raster
      rast(fuelsRaster, vals = seasonalAccumulators[[season]]) %>%
        mask(fuelsRaster) %>%
        writeRaster(str_c(seasonalAccumulatorOutputFolder, "/it", Iteration, "-sn", lookup(season, SeasonTable$Name, SeasonTable$SeasonId), ".tif"), 
                    overwrite = T,
                    NAflag = -9999,
                    wopt = list(filetype = "GTiff",
                        datatype = "INT4S",
                        gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
    }
  }
}

updateRunLog("Finished parsing run inputs in ", updateBreakpoint())

# Prepare shared inputs ----

# Create a local copy of the fuels grid as ASCII and projection file
# Pandora appears to require this format for the fuels grid, but tif is accepted for the elevation grid
writeRaster(fuelsRaster, fuelsRasterAscii, filetype = "AAIGrid", overwrite = T, NAflag = -9999, datatype = "INT2S")
crs(fuelsRaster) %>%
  cat(file = fuelsRasterProjection)

# Reformat fuel lookup table
FuelType %>%
  transmute(
    grid_value = ID,
    export_value = ID,
    descriptive_name = str_c(Name),
    fuel_type = Code
  ) %>%
  mutate(r = 0, g = 0, b = 0, h = 0, s = 0, l = 0) %>%
  write_csv(fuelLookup, escape = "none")

# Setup dummy location for weather station in the middle of the extent
weatherStationLocation <- latlonFromRowCol(fuelsRaster, floor(nrow(fuelsRaster) / 2), floor(ncol(fuelsRaster) / 2))
weatherStationElevation <- ifelse(!is.null(elevationRaster), elevationRaster[floor(nrow(elevationRaster) / 2), floor(ncol(elevationRaster) / 2)], 0)

# Convert ignition location to lat/long
# Keep only indexes and location
ignitionLocation <- DeterministicIgnitionLocation %>%
  dplyr::select(Iteration, FireID, Season, Lat = Latitude, Lon = Longitude)

# Copy wind grids if needed
if (useWindGrid) {
  # Todo: ensure filepaths are full, copy to temp folder?
  WindGridParameterStrings <- WindGrid %>%
    pivot_longer(cols = -"WindSpeed", names_to = c("Variable", "Direction"), names_prefix = "Wind", values_to = "FileName", names_pattern = "([SD][a-z]*)([NSEW].*)") %>%
    transmute(
      parameter = lookup(Variable, c("Speed", "Direction"), c("WSgrid", "WDgrid")),
      sector = lookup(Direction, c("North", "NorthEast", "East", "SouthEast", "South", "SouthWest", "West", "NorthWest"), 1:8),
      speed = WindSpeed,
      file = FileName %>% str_replace_all("\\\\", "/")
    ) %>%
    unite("parameterFileLine", sep = " ") %>%
    pull()
}

# Decide which burn components to keep
# - Parse table
outputComponentsToKeep <- OutputOptionsSpatialPrometheus %>%
  pivot_longer(-starts_with(c("Scenario", "Project", "Parent")), names_to = "component", values_to = "keep") %>%
  filter(keep) %>%
  pull(component)

# - Translate to input keywords as expected by Pandora, prepend burn map keyword
outputComponents <- outputComponentsToKeep %>%
  lookup(old = outputComponentNames, new = outputComponentCodes) %>%
  str_c(collapse = " ") %>%
  str_c("burn ", .)

# - Initialize list of tables to hold outputs
outputComponentTables <- list()
for (component in outputComponentsToKeep) {
  outputComponentTables[[component]] <- data.frame()
}

# Generate empty parameter file template for single fire
parameterFileTemplate <- generateParamaterTemplate(parameterFilePlaceHolders)

# Generate fuel loading maps if used
# - It seems that pandora can only set fuel loading using geotiffs, so these must be created based on the season-specific fuel loading value chosen by the user
# - tempfile is used to catch season names that are not acceptable as filenames
if (setFuelLoad) {
  FuelLoad <- FuelLoad %>%
    mutate(
      FileName = map_chr(Season, ~ tempfile(pattern = "FuelLoad-", tmpdir = tempDir, fileext = ".tif")),
      FileName = str_replace_all(FileName, "\\\\", "/")
    )

  maskValues <- FuelType %>%
    filter(str_detect(Code, "O-1")) %>%
    pull(ID)

  for (i in seq(nrow(FuelLoad))) {
    rast(fuelsRaster, vals = FuelLoad$FuelLoad[i]) %>%
      mask(fuelsRaster, inverse = T, maskvalues = maskValues) %>%
      writeRaster(FuelLoad$FileName[i],
        overwrite = T,
        NAflag = -9999,
        wopt = list(
          filetype = "GTiff",
          datatype = "FLT4S",
          gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "PREDICTOR=2")
        )
      )
  }
}

# Generate grass curing maps if used
# - It seems that pandora can only set fuel loading using geotiffs, so these must be created based on the season-specific fuel loading value chosen by the user
# - tempfile is used to catch season names that are not acceptable as filenames
if (setGrassCuring) {
  Curing <- Curing %>%
    mutate(
      FileName = map_chr(Season, ~ tempfile(pattern = "Curing-", tmpdir = tempDir, fileext = ".tif")),
      FileName = str_replace_all(FileName, "\\\\", "/")
    )

  for (i in seq(nrow(Curing))) {
    rast(fuelsRaster, vals = Curing$Curing[i]) %>%
      writeRaster(Curing$FileName[i],
        overwrite = T,
        NAflag = -9999,
        wopt = list(
          filetype = "GTiff",
          datatype = "FLT4S",
          gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "PREDICTOR=2")
        )
      )
  }
}

# Combine deterministic input tables ----
fireGrowthInputs <- DeterministicBurnCondition %>%
  # Group by iteration and fire ID for the `growFire()` function
  nest(.by = c(Iteration, FireID)) %>%

  # Add ignition location information
  left_join(ignitionLocation, c("Iteration", "FireID")) %>%

  # Split extra ignitions into reasonable batch sizes
  mutate(extraIgnitionsBatch = (row_number() - 1) %/% batchSize + 1, 
         extraIgnitionsBatch = ifelse(Iteration == 0, extraIgnitionsBatch, 0)) %>%

  # Group by just iteration for the `runIteration()` function
  nest(.by = c(Iteration, extraIgnitionsBatch)) %>% 
  dplyr::select(-extraIgnitionsBatch) %>%

  # Finally split into batches of the appropriate size
  group_by(batchID = (cumsum(map_int(data, nrow)) - 1) %/% batchSize) %>%
  group_split(.keep = F)

updateRunLog("Finished generating model inputs in ", updateBreakpoint())

# Grow fires ----
progressBar("begin", totalSteps = length(fireGrowthInputs))
progressBar(type = "message", message = "Growing fires...")

OutputFireStatistic <- fireGrowthInputs %>%
  map_dfr(runBatch)
  
updateRunLog("Finished burning fires in ", updateBreakpoint())

# Save relevant outputs ----

## Fire statistics table ----
if (OutputOptions$FireStatistics | minimumFireSize > 0) {
  progressBar(type = "message", message = "Generating fire statistics table...")

  # Load necessary rasters and lookup tables
  fireZoneRaster <- tryCatch(
    rast(datasheet(myScenario, "burnP3Plus_LandscapeRasters")[["FireZoneGridFileName"]]),
    error = function(e) NULL) %>%
    checkSpatialInput("Fire Zone", warnOnly = T)
  weatherZoneRaster <- tryCatch(
    rast(datasheet(myScenario, "burnP3Plus_LandscapeRasters")[["WeatherZoneGridFileName"]]),
    error = function(e) NULL) %>%
    checkSpatialInput("Weather Zone", warnOnly = T)

  # Add extra information to Fire Statistic table
  OutputFireStatistic <- OutputFireStatistic %>%
    
    # Start by joining summarized burn conditions
    left_join({
      # Start by summarizing burn conditions
      DeterministicBurnCondition %>%
      
        # Only consider iterations this job is responsible for
        filter(Iteration %in% iterations | (Iteration == 0 & FireID %in% extraIgnitionIDs)) %>%
          
        # Summarize burn conditions by fire
        group_by(Iteration, FireID) %>%
        summarize(
          FireDuration = max(BurnDay),
          HoursBurning = sum(HoursBurning)) %>%
        ungroup()},
      by = c("Iteration", "FireID")) %>%
  
      # Determine Fire and Weather Zones if the rasters are present, as well as fuel type of ignition location
      left_join(DeterministicIgnitionLocation, by = c("Iteration", "FireID"))
    
    # Determine Fire and Weather Zones if the rasters are present, as well as 
    # fuel type of ignition location
  OutputFireStatistic$cell <- cellFromLatLong(
      fuelsRaster, 
      OutputFireStatistic$Latitude, 
      OutputFireStatistic$Longitude)
  
  if (!is.null(weatherZoneRaster)){
    OutputFireStatistic <- OutputFireStatistic %>%
      mutate(
        weatherzoneID = weatherZoneRaster[][cell],
        WeatherZone = lookup(weatherzoneID, WeatherZoneTable$ID, WeatherZoneTable$Name)
      ) %>%
      dplyr::select(-weatherzoneID)
  } else{
    OutputFireStatistic$WeatherZone <- WeatherZoneTable$Name
  }
  
  if (!is.null(fireZoneRaster)){
    OutputFireStatistic <- OutputFireStatistic %>%
      mutate(
        firezoneID = fireZoneRaster[][cell],
        FireZone = lookup(firezoneID, FireZoneTable$ID, FireZoneTable$Name)
      ) %>%
      dplyr::select(-firezoneID)
  } else{
    OutputFireStatistic$FireZone <- FireZoneTable$Name
  }
  
  OutputFireStatistic <- OutputFireStatistic %>%
    mutate(
      fueltypeID = fuelsRaster[][cell],
      FuelType = lookup(fueltypeID, FuelType$ID, FuelType$Name),
      Timestep = 0) %>%
    
      # Incorporate Lat and Long and add TimeStep manually
    
      # Clean up for saving
      dplyr::select(Iteration, Timestep, FireID, Latitude, Longitude, Season, 
                    Cause, FireZone, WeatherZone, FuelType, FireDuration, 
                    HoursBurning, Area, ResampleStatus) %>%
      as.data.frame()
      
    # Output if there are records to save
    if(nrow(OutputFireStatistic) > 0)
      saveDatasheet(myScenario, OutputFireStatistic, "burnP3Plus_OutputFireStatistic", append = T)
  
  updateRunLog("Finished collecting fire statistics in ", updateBreakpoint())
}

## Burn maps ----
if (saveBurnMaps) {
  progressBar(type = "message", message = "Saving burn maps...")

  # Build table of burn maps and save to SyncroSim
  OutputBurnMap <-
    tibble(
      FileName = list.files(accumulatorOutputFolder, pattern = "*.tif$", full.names = T),
      Iteration = str_extract(FileName, "\\d+.tif") %>% str_sub(end = -5) %>% as.integer(),
      Timestep = 0,
      Season = "All") %>%
    filter(Iteration %in% iterations) %>%
    as.data.frame()

  if(saveSeasonalBurnMaps) {
    # If seasonal burn maps have been saved, append them to the table
    OutputBurnMap <- OutputBurnMap %>%
      bind_rows(
        tibble(
          FileName = list.files(seasonalAccumulatorOutputFolder, full.names = T) %>% normalizePath(),
          Iteration = str_extract(FileName, "\\d+-sn") %>% str_sub(end = -4) %>% as.integer(),
          Timestep = 0,
          Season = str_extract(FileName, "\\d+.tif") %>% str_sub(end = -5) %>% as.integer()) %>%
        mutate(
          Season = lookup(Season, SeasonTable$SeasonId, SeasonTable$Name)) %>%
        filter(Iteration %in% iterations)) %>%
      as.data.frame
  }
  
  # Output if there are records to save
  if (nrow(OutputBurnMap) > 0) {
    saveDatasheet(myScenario, OutputBurnMap, "burnP3Plus_OutputBurnMap", append = T)
  }

  # Output secondary outputs if present
  if (length(outputComponentsToKeep) > 0) {
    for (i in seq_along(outputComponentTables)) {
      if (nrow(outputComponentTables[[i]]) > 0) {
        saveDatasheet(myScenario, outputComponentTables[[i]], str_c("burnP3PlusPrometheus_Output", outputComponentsToKeep[i], "Map"))
      }
    }
  }

  updateRunLog("Finished collecting burn maps in ", updateBreakpoint())
}

## Burn perimeters ----
if (OutputOptionsSpatial$BurnPerimeter) {
  progressBar(type = "message", message = "Saving burn perimeters...")
  OutputBurnPerimeter <-
    tibble(
      FileName = list.files(shapeOutputFolder, pattern = "*.shp", full.names = T),
      Tag = str_extract(FileName, "it\\d+\\.fid\\d+"),
      Iteration = str_extract(Tag, "it\\d+") %>% str_sub(3) %>% as.integer(),
      FireID = str_extract(Tag, "fid\\d+") %>% str_sub(4) %>% as.integer(),
      Timestep = 0
    ) %>%
    filter(Iteration %in% iterations) %>%
    dplyr::select(-Tag) %>%
    as.data.frame()

  # Output if there are records to save
  if (nrow(OutputBurnPerimeter) > 0) {
    saveDatasheet(myScenario, OutputBurnPerimeter, "burnP3Plus_OutputFirePerimeter", append = T)
  }

  updateRunLog("Finished collecting burn perimeters in ", updateBreakpoint())
}

## All Perims ----
if (OutputOptionsSpatial$AllPerim | (saveBurnMaps & minimumFireSize > 0)) {
  progressBar(type = "message", message = "Saving individual burn maps...")

  # Build table of burn maps and save to SyncroSim
  OutputAllPerim <-
    tibble(
      FileName = list.files(allPerimOutputFolder, pattern = "*.tif", full.names = T),
      Iteration = str_extract(FileName, "\\d+_fire") %>% str_sub(end = -6) %>% as.integer(),
      FireID = str_extract(FileName, "\\d+.tif") %>% str_sub(end = -5) %>% as.integer(),
      Timestep = FireID
    ) %>%
    filter(Iteration %in% iterations | (Iteration == 0 & FireID %in% extraIgnitionIDs)) %>%
    as.data.frame()

  # Output if there are records to save
  if (nrow(OutputAllPerim) > 0) {
    saveDatasheet(myScenario, OutputAllPerim, "burnP3Plus_OutputAllPerim", append = T)
  }

  updateRunLog("Finished individual burn maps in ", updateBreakpoint())
}

# Remove grid outputs if present
unlink(gridOutputFolder, recursive = T, force = T)
progressBar("end")
