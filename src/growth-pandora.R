# Setup ----
Sys.unsetenv("PROJ_LIB")
library(rsyncrosim)

# Find and source shared setup script and function definitions
getSharedDefinitionsPath <- function() {
  sessionPackages <- rsyncrosim::packages(session())
  libraryPackages <- rsyncrosim::packages(ssimLibrary())
  burnP3PlusVersion <- libraryPackages[libraryPackages$name == "burnP3Plus", "version"]
  sharedDefinitionsPath <- paste0(sessionPackages[sessionPackages$name == "burnP3Plus" & sessionPackages$version == burnP3PlusVersion, "location"], "/shared.R")
  return(sharedDefinitionsPath)
}
source(getSharedDefinitionsPath())

## Check Prometheus installation ----
if (.Platform$OS.type == "unix") {
  stop("Prometheus is currently not supported on Unix systems.")
}

prometheusLocation <- Sys.which("prometheus.exe")
if (prometheusLocation == "") {
  prometheusLocation <- shortPathName("C:/Program Files/Prometheus/Prometheus.exe")
}

if (!file.exists(prometheusLocation)) {
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
             type = "status")

## Connect to SyncroSim ----

# Load remaining datasheets
BatchOption <- datasheet(myScenario, "burnP3Plus_BatchOption")
ResampleOption <- datasheet(myScenario, "burnP3Plus_FireResampleOption")
DeterministicIgnitionLocation <- datasheet(myScenario, "burnP3Plus_DeterministicIgnitionLocation", lookupsAsFactors = F, optional = T, returnInvisible = T) %>% unique()
DeterministicBurnCondition <- datasheet(myScenario, "burnP3Plus_DeterministicBurnCondition", lookupsAsFactors = F, optional = T, returnInvisible = T) %>% unique()
FuelType <- datasheet(myScenario, "burnP3Plus_FuelType")
FuelTypeCrosswalk <- datasheet(myScenario, "burnP3PlusPrometheus_FuelCodeCrosswalk", lookupsAsFactors = F, optional = T)
ValidFuelCodes <- datasheet(myScenario, "burnP3PlusPrometheus_FuelCode") %>% pull()
SeasonTable <- datasheet(myScenario, "burnP3Plus_Season", lookupsAsFactors = F, optional = T, includeKey = T, returnInvisible = T)
FBPVariableTable <- datasheet(myScenario, "burnP3Plus_FBPOutputVariable", lookupsAsFactors = F, optional = T, returnInvisible = T)
WindGrid <- datasheet(myScenario, "burnP3Plus_WindGrid", lookupsAsFactors = F, optional = T)
GreenUp <- datasheet(myScenario, "burnP3Plus_GreenUp", lookupsAsFactors = F, optional = T)
Curing <- datasheet(myScenario, "burnP3Plus_Curing", lookupsAsFactors = F, optional = T)
# FuelLoad <- datasheet(myScenario, "burnP3Plus_FuelLoad", lookupsAsFactors = F, optional = T) # Currently disabled by BP3+
OutputOptions <- datasheet(myScenario, "burnP3Plus_OutputOption", optional = T)
OutputOptionsSpatial <- datasheet(myScenario, "burnP3Plus_OutputOptionSpatial", optional = T) %>% mutate(BurnPerimeter = as.character(BurnPerimeter))
OutputOptionFBPSpatial <- datasheet(myScenario, "burnP3Plus_OutputOptionFBPSpatial", optional = T, returnInvisible = T) %>% mutate(Variable = as.character(Variable))
FireZoneTable <- datasheet(myScenario, "burnP3Plus_FireZone")
WeatherZoneTable <- datasheet(myScenario, "burnP3Plus_WeatherZone")

# Import relevant rasters
fuelsRaster <- loadSpatial$fuels()
elevationRaster <- loadSpatial$elevation()

## Validate and parse datasheets ----

# Note: these functions are defined in the shared definitions script
validateAndParseData$FuelType(crosswalk = "burnP3PlusPrometheus_FuelCodeCrosswalk")
validateAndParseData$Zones()
validateAndParseData$DeterminsiticIgnitions()
validateAndParseData$DeterminsiticBurnConditions()
validateAndParseData$OutputOptions()
validateAndParseData$FBPOutputOptions()
validateAndParseData$BatchOptions()
validateAndParseData$ResampleOptions()
validateAndParseData$FireGrowthOptions()
validateAndParseData$WindGrids()

# Drop list of function for handling missing data from memory to free up some memory
rm(validateAndParseData)


## Report selected but unsupported options ---

# Prometheus currently supports all BP3+ features


## Set model-specific constants ----

# Names and codes of Prometheus-specific secondary outputs
outputComponentNames <- c("RateOfSpread", "FireIntensity", "SpreadDirection", "SurfaceFuelConsumption", "CrownFractionBurned", "CrownFractionConsumed", "TotalFuelConsumption")
outputComponentCodes <- c("ros", "fi", "raz", "sfc", "cfb", "cfc", "tfc")

# Parameter file template place holders
parameterFilePlaceHolders <- list(
  fileTag     = "fileTagPlaceHolder",
  lon         = "lonPlaceHolder",
  lat         = "latPlaceHolder",
  weatherFile = "weatherFilePlaceHolder",
  ignDate     = "ignitionDatePlaceHolder",
  ignFile     = "ignitionFilePlaceHolder",
  greenup     = "greenupPlaceHolder",
  grassCuring = "grassCuringPlaceHolder",
  fuelLoad    = "fuelLoadPlaceHolder",
  duration    = "durationPlaceHolder")


## Determine which fires this job is responsible for ----
# Use the BP3+ heuristic job allocation rather than default SyncroSim allocation
firesToBurn <- splitFiresByJob()

#  Stop with a warning if there's nothing to do
if(nrow(firesToBurn) == 0) {
  updateRunLog("Found no fires to burn for this job! Please ensure your Run Controls are set properly for you Deterministic Inputs", type = "warning")
  # As we are not in a function call, this is the only way to end the transformer early without raising an error
  quit(save = "no", status = 0)
}


## Setup files and folders ----

# Create temp folder structure, ensure it is empty
generateSharedTempFilePaths("growth-pandora")

# Set names for model input files to be created
fuelsRasterAscii <- file.path(tempDir, "fuels.asc")
fuelsRasterProjection <- file.path(tempDir, "fuels.prj")
fuelLookup <- file.path(tempDir, "fuels.lut")
parameterFile <- file.path(tempDir, "parameters.txt")


## Transformer-specific Functions Definitions ----

# Get path of final burn grids
# - pandora could produce no grids (if fires don't burn), a single grid, or daily grids
getFinalRawOutputGridPaths <- function(gridOutputFolder, fileTags) {
  # Build regex pattern for output grid file names (might include hour of burning or not)
  file_pattern <- str_c(fileTags, "_burn\\d*.asc$")

  # List files matching pattern for each fire's file tag
  map(file_pattern, list.files, path = gridOutputFolder, full.names = TRUE) %>%
    # If not outputs are found, replace with NA to avoid silent dropping
    map(~ if(length(.x) == 0) NA else .x) %>%
    # Convert to a table with a column for file tag
    map2_dfr(fileTags, ~tibble(file_tag = .y, filename = .x)) %>%
    # Parse burn hour, if included in file name
    mutate(burn_hour = str_extract(filename, "\\d+.asc") %>% str_extract("\\d+") %>% as.integer %>% replace_na(0)) %>%
    # Keep only the grid file associated with the last burn hour (if multiple are present)
    dplyr::filter(burn_hour == max(burn_hour), .by = file_tag) %>%
    pull(filename)
}

# Get burn area from output asc
getBurnArea <- function(inputFile) {
  if (!is.na(inputFile) && file.exists(inputFile)) {
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

# Function to get the cell IDs of burned pixels from the raw output burn grid
findBurnedCellIDs <- function(layer_filepath) {
  # Load layer
  rast(layer_filepath) %>%
    # Find IDs of burned cells (value = 1)
    cells(y = 1) %>%
    unlist
}

extractTabularData <- function(layer_filepath, cellIDs) {
  # Load layer
  rast(layer_filepath) %>%
    # Extract values from Cell IDs
    `[`(cellIDs) %>%
    # Return as vector
    pull()
}

processOutputsPerFire <- function(BatchID, Iteration, FireID, UniqueFireID, burnGrids, ...) {
  # Return nothing if no maps were produced for this fire
  if(length(burnGrids) < UniqueFireID | is.na(burnGrids[[UniqueFireID]]))
    return()
  
  # Load spatial data if present
  burnPath <- burnGrids[[UniqueFireID]]

  # Get table of burned cells
  burnData <- data.table(
    BatchID = BatchID,
    Iteration = Iteration,
    FireID = FireID,
    CellID = findBurnedCellIDs(burnPath))

  # Add columns of data for any FBP outputs
  for (component in outputComponentsToKeep) {
    # Find the correct secondary output in the same folder as the burn grid
    inputComponentFileName <- burnPath %>%
      str_replace(
        "_burn\\d*.asc$",
        str_c("_", lookup(component, outputComponentNames, outputComponentCodes), ".asc"))

    # If it exists, add a corresponding column to burnData
    # - Note that with data.table syntax this does not need to be assigned back to burnData
    if (file.exists(inputComponentFileName)) {
      burnData[, (component) := extractTabularData(inputComponentFileName, CellID)]
    }
  }

  # Generate vector outputs if required
  if(OutputOptionsSpatial$BurnPerimeter != "No")
    generateVectorPerimeters(Iteration, FireID, UniqueFireID, burnGrids[[UniqueFireID]])

  # Return raw tabular outputs
  return(burnData)
}

# Function to convert, accumulate, and clean up raw outputs
processOutputs <- function(batchOutputs, rawOutputGridPaths) {
  # Don't save outputs from fires below minimum size
  batchOutputs <- batchOutputs %>%
    filter(ResampleStatus == "Kept" | ResampleStatus == "Extra")
    
  # Generate outputs and collect raw tabular outputs
  batchTabularData <- pmap_dfr(batchOutputs, processOutputsPerFire, burnGrids = rawOutputGridPaths)

  # Save raw batch outputs to temp dataset
  if(!isDatasheetEmpty(batchTabularData))
    arrow::write_dataset(
      dataset = batchTabularData %>% group_by(BatchID),
      path = rawTableTempPath,
      format = "parquet",
      existing_data_behavior = "delete_matching")
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

  pandoraExe <- ssimEnvironment()$PackageDirectory %>%
    str_replace_all("\\\\", "/") %>%
    str_c("/pandora.exe")
  
  if (!file.exists(pandoraExe))
    stop("Could not find the Pandora executable within the BP3+ Prometheus package folder. Please reinstall the package.")
  
  str_c("\"", pandoraExe, "\"", " /silent /nowin ", parameterFile) %>%
    shell()

  # Reset proj lib variable for terra
  Sys.unsetenv("PROJ_LIB")
}

# Function to run one batch of iterations
runBatch <- function(batchInputs) {
  # Generate batch-specific index for fires
  batchInputs <- batchInputs %>%
    mutate(UniqueFireID = row_number())
  
  # - Unnest and process weather info
  batchWeather <- unnest(batchInputs, data)
  generateIgnitionFiles(batchInputs)
  generateWeatherFiles(batchWeather)
  
  # Reset and build parameter file with header line, get list of expected output file tags
  cat("Landscape_Constant 1", file = parameterFile, sep = "\n")
  fileTags <- batchInputs %>%
    dplyr::rename(season = Season) %>% # used to avoid a name conflict with the Season datasheet
    pmap_chr(generateParameterFile, placeHolderNames = parameterFilePlaceHolders)

  # Run Pandora on the batch
  runPandora()

  # Get relative paths to all raw outputs
  rawOutputGridPaths <- getFinalRawOutputGridPaths(gridOutputFolder, fileTags)

  # Get burn areas
  burnAreas <- getBurnAreas(rawOutputGridPaths)
  
  # Convert and save spatial outputs as needed
  batchOutputs <- batchInputs %>%
    select(BatchID, UniqueFireID, Iteration, FireID, Season) %>%
    mutate(Area = burnAreas) %>%
    getResampleStatus()
    
  # Save GeoTiffs if needed
  if(saveBurnMaps)
    processOutputs(batchOutputs, rawOutputGridPaths)
  
  # Clear up temp files
  resetFolder(gridOutputFolder)
  
  # Update Progress Bar
  progressBar("step")
  progressBar(type = "message", message = "Growing fires...")
  
  # Return relevant outputs
  batchOutputs %>%
    select(-BatchID, -UniqueFireID, -Season) %>%
    return()
}

### File generation functions ----

# Function to convert daily weather data for every day of burning to format
# expected by Pandora and save to file
generateWeatherFile <- function(weatherData, UniqueFireID, season, year = 2001) {
  ignDate <- getSeasonMedianDate(season, year)

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
      date = as.integer((row_number() + 12) / 24) + ignDate,
      date = str_c(day(date), "/", month(date), "/", year(date)),
      time = (row_number() + 12) %% 24
    ) %>%
    # Finally we rename and reorder columns and write to file
    dplyr::select(HOURLY = date, HOUR = time, TEMP = Temperature, RH = RelativeHumidity, WD = WindDirection, WS = WindSpeed, PRECIP = Precipitation, HFFMC = FineFuelMoistureCode, HISI = InitialSpreadIndex, HFWI = FireWeatherIndex, DMC = DuffMoistureCode, DC = DroughtCode, BUI = BuildupIndex) %>%
    fwrite(file.path(weatherFolder, str_c("Weather", UniqueFireID, ".txt")))
  invisible()
}

# Function to split deterministic burn conditions into separate weather files by iteration and fire id
generateWeatherFiles <- function(DeterministicBurnCondition){
  # Clear out old weather files if present
  resetFolder(weatherFolder)
  
  # Generate files as needed
  DeterministicBurnCondition %>%
    group_by(Iteration, FireID, UniqueFireID, Season) %>%
    nest() %>%
    ungroup() %>%
    arrange(Iteration, FireID, UniqueFireID) %>%
    dplyr::select(weatherData = data, UniqueFireID = UniqueFireID, season = Season) %>%
    pmap(generateWeatherFile)
  invisible()
}

# Function to convert ignition locations into shape files expected by Pandora
generateIgnitionFile <- function(Latitude, Longitude, UniqueFireID, ...) {
  # Providing ignition location or a shapefile of points causes Pandora to simulate the fire with acceleration, which is not appropriate for these simulations
  # Instead, we provide a very small polygon that includes the centroid of the pixel to start the ignition in to simulate without acceleration
  padding <- 6e-6
  x <- data.frame(
    lat = c(Latitude - padding, Latitude - padding, Latitude + padding, Latitude + padding),
    lon = c(Longitude - padding, Longitude + padding, Longitude + padding, Longitude - padding) 
  ) %>%
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = "epsg:4326"
    ) %>% # TODO: Does this need to be projected?
    st_combine() %>%
    sf::st_cast("POLYGON") %>%
    st_write(file.path(ignitionFolder, str_c("Ignition", UniqueFireID, ".shp")), quiet = TRUE)
  invisible()
}

# Function to split deterministic ignition location into ignition files by iteration and fire id
generateIgnitionFiles <- function(DeterministicIgnitionLocation){
  # Clear out old weather files if present
  resetFolder(ignitionFolder)
  
  # Generate files as needed
  DeterministicIgnitionLocation %>%
    pmap(generateIgnitionFile)
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
    str_c("Ign_File ", placeHolderNames$ignDate, ":13:00:00 ", placeHolderNames$ignFile),
    #str_c("Ign_DateTime 1/6/2000:13:00:00"),
    #str_c("Ign_Lon ", placeHolderNames$lon),
    #str_c("Ign_Lat ", placeHolderNames$lat),
    str_c("WxStation_Lon ", weatherStationLocation[1]),
    str_c("WxStation_Lat ", weatherStationLocation[2]),
    str_c("WxStation_Elev ", weatherStationElevation),
    str_c("Wx_file ", placeHolderNames$weatherFile),
    str_c("Init_hour 13"),
    str_c("FFMC_Method 5"),
    str_c("Minimum_Size 0"),
    str_c("Out_GridType 0"),
    str_c("Threads 1"),
    if (useWindGrid) {
      WindGridParameterStrings
    } else {
      NA
    },
    str_c("Greenup ", placeHolderNames$greenup),
    str_c("Grass_Curing ", placeHolderNames$grassCuring, " ", str_c(FuelType %>% filter(str_detect(Code, "O-1")) %>% pull(ID), collapse = " ")),
    if (setFuelLoad) {
      str_c("Fuel_Load_GridFile ", placeHolderNames$fuelLoad)
    } else {
      NA
    },
    str_c("Duration  ", placeHolderNames$duration),
    if (OutputOptionsSpatial$BurnPerimeter == "Daily") {
      str_c("Export_Every 24")
    } else {
      str_c("Export_Every ", placeHolderNames$duration)
    }
  ) %>%
    discard(is.na)

  # Choose which outputs to save based on chosen output options
  if (OutputOptionsSpatial$BurnPerimeter != "No") {
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
generateParameterFile <- function(Iteration, FireID, UniqueFireID, season, data, placeHolderNames, ...) {
  # Define a unique identifier to name files
  fileTag <- str_c("it", Iteration, ".fid", FireID)

  # Calculate values to fill placeholders in template
  weatherFile <- file.path(weatherFolder, str_c("Weather", UniqueFireID, ".txt"))
  ignFile <- file.path(ignitionFolder, str_c("Ignition", UniqueFireID, ".shp"))

  ignDate <- getSeasonMedianDate(season) %>%
    format("%d/%m/%Y") # dd/mm/yyyy is expected by Pandora

  greenupValue <-  GreenUp %>%
    dplyr::filter(Season %in% c(season, NA, "All")) %>%
    mutate(Season = na_if(Season, "All")) %>% # Replace "All" season with NA so it sorts to end with `arrange`
    arrange(Season) %>% 
    pull(GreenUp) %>%
    pluck(1) %>%
    as.numeric()

  grassCuringValue <- Curing %>%
    dplyr::filter(Season %in% c(season, NA, "All")) %>%
    mutate(Season = na_if(Season, "All")) %>%
    arrange(Season) %>%
    pull(Curing) %>%
    pluck(1)

  if(setFuelLoad) {
    fuelLoadValue <- FuelLoad %>%
      filter(Season %in% c(season, NA, "All")) %>%
      mutate(Season = na_if(Season, "All")) %>%
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
    str_replace_all(placeHolderNames$weatherFile, weatherFile) %>%
    str_replace_all(placeHolderNames$ignDate, ignDate) %>%
    str_replace_all(placeHolderNames$ignFile, ignFile) %>%
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

# Function to find a vector shapefile outputs using the burn grid name
getRawPerimeterPaths <- function(burnGrid) {
  list.files(
    path = shapeOutputFolder,
    pattern = basename(burnGrid) %>% str_replace("burn\\d*.asc$", ".*shp$"),
    full.names = T) %>%
    # Convert to a tibble
    enframe(value = "filename") %>%
    # Extract day info, if present
    mutate(burn_hour = str_extract(filename, "\\d+\\.shp") %>% str_extract("\\d+") %>% as.integer %>% replace_na(0)) %>%
    # Sort shapefile paths chronologically
    arrange(burn_hour) %>%
    pull(filename)
}

readAndCleanPerimeter <- function(layerPath) {
  st_read(layerPath, quiet = TRUE) %>%
    st_as_sf() %>%
    st_buffer(0) %>% # Prevent specific invalidity case that st_make_valid doesn't catch
    st_make_valid() %>%
    st_cast("MULTIPOLYGON") %>%
    return()
}

generateVectorPerimeters <- function(Iteration, FireID, UniqueFireID, burnGrid, ...) {
  layerPaths <- getRawPerimeterPaths(burnGrid)

  # Make sure outputs were created
  if(length(layerPaths) == 0)
    return()

  # Generate vector outputs if needed
  if(OutputOptionsSpatial$BurnPerimeter == "Final") {
    # Vectorize map from last day of burning
    readAndCleanPerimeter(layerPaths %>% tail(1)) %>%
      mutate(
        Iteration = Iteration,
        FireID = FireID,
        geometry = geometry,
        .keep = "none") %>%

      # Save outputs
      st_write(
        dsn = geopackage_path,
        layer = geopackage_layer_name,
        quiet = TRUE,
        append = TRUE)
  }

  if(OutputOptionsSpatial$BurnPerimeter == "Daily") {
    # For daily perimeters, iterate over daily burn maps to generate burn perimeters
    burn_to_date <- NULL
    for (burnDay in seq_along(layerPaths)) {
      # Update yesterday's burn
      burn_yesterday <- burn_to_date

      # Vectorize current day's grid
      burn_to_date <- readAndCleanPerimeter(layerPaths[burnDay]) %>%
        mutate(
          Iteration = Iteration,
          FireID = FireID,
          BurnDay = burnDay,
          geometry = geometry,
          .keep = "none")
      
      # Subtract previous days burn if not the first day
      if (burnDay == 1) {
        burn_today <- burn_to_date
      } else {
        st_agr(burn_to_date) = "constant"
        burn_today <- burn_to_date %>%
          st_difference(st_geometry(burn_yesterday))
      }

      # Save output
      st_write(
        obj = burn_today,
        dsn = geopackage_path,
        layer = geopackage_layer_name,
        quiet = TRUE,
        append = TRUE)
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

# Reformat and write Prometheus fuel lookup table
FuelType %>%
  bind_rows(
    # Insert a non-fuel record to start of lookup
    # - workaround for curing parsing issue in Pandora
    tibble(ID = -1, Color = "0,0,0,0", Name = "Non-fuel", Code = "Non-fuel"),
    .
  ) %>%
  transmute(
    grid_value = ID,
    export_value = ID,
    descriptive_name = str_c(Name),
    fuel_type = Code
  ) %>%
  mutate(r = 0, g = 0, b = 0, h = 0, s = 0, l = 0) %>%
  write_csv(fuelLookup, escape = "none")

# Setup dummy location for weather station in the middle of the extent
weatherStationLocation <- st_coordinates(
                              st_transform(
                                st_as_sf(
                                  data.frame(
                                    xyFromCell(fuelsRaster,
                                               cellFromRowCol(fuelsRaster,
                                                              floor(nrow(fuelsRaster) / 2),
                                                              floor(ncol(fuelsRaster) / 2)))),
                                  coords=c("x","y"),
                                  crs=crs(fuelsRaster)),
                                "EPSG:4326"))
weatherStationElevation <- ifelse(!is.null(elevationRaster), elevationRaster[floor(nrow(elevationRaster) / 2), floor(ncol(elevationRaster) / 2)], 0)

# Copy wind grids if needed
if (useWindGrid) {
  WindGridParameterStrings <- WindGrid %>%
    pivot_longer(cols = -"WindSpeed", names_to = c("Variable", "Direction"), names_prefix = "Wind", values_to = "FileName", names_pattern = "([SD][a-z]*)([NSEW].*)") %>%
    transmute(
      parameter = lookup(Variable, c("Speed", "Direction"), c("WSgrid", "WDgrid")),
      sector = lookup(Direction, c("North", "NorthEast", "East", "SouthEast", "South", "SouthWest", "West", "NorthWest"), 1:8),
      speed = WindSpeed,
      file = FileName %>% normalizePath() %>% str_replace_all("\\\\", "/")
    ) %>%
    unite("parameterFileLine", sep = " ") %>%
    pull()
}

# Decide which burn components to keep
# - Translate to input keywords as expected by Pandora, prepend burn map keyword
outputComponents <- outputComponentsToKeep %>%
  lookup(old = outputComponentNames, new = outputComponentCodes) %>%
  str_c(collapse = " ") %>%
  str_c("burn ", .)

# # Generate fuel loading maps if used
# # - It seems that pandora can only set fuel loading using geotiffs, so these must be created based on the season-specific fuel loading value chosen by the user
# # - tempfile is used to catch season names that are not acceptable as filenames
# if (setFuelLoad) {
#   FuelLoad <- FuelLoad %>%
#     mutate(
#       FileName = map_chr(Season, ~ tempfile(pattern = "FuelLoad-", tmpdir = tempDir, fileext = ".tif")),
#       FileName = str_replace_all(FileName, "\\\\", "/")
#     )

#   maskValues <- FuelType %>%
#     filter(str_detect(Code, "O-1")) %>%
#     pull(ID)

#   for (i in seq(nrow(FuelLoad))) {
#     rast(fuelsRaster, vals = FuelLoad$FuelLoad[i]) %>%
#       mask(fuelsRaster, inverse = T, maskvalues = maskValues) %>%
#       writeRaster(FuelLoad$FileName[i],
#         overwrite = T,
#         NAflag = -9999,
#         wopt = list(
#           filetype = "GTiff",
#           datatype = "FLT4S",
#           gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "PREDICTOR=2")
#         )
#       )
#   }
# }

# Generate empty parameter file template for single fire
parameterFileTemplate <- generateParamaterTemplate(parameterFilePlaceHolders)

# Organize ignition location
ignitionLocation <- DeterministicIgnitionLocation %>%
  dplyr::select("Iteration","FireID","Latitude","Longitude","Season") %>%
  arrange("Iteration", "FireID")

# Combine deterministic input tables ----
fireGrowthInputs <- 
  generateFireGrowthInputs(
    firesToBurn = firesToBurn,
    DeterministicBurnCondition = DeterministicBurnCondition,
    ignitionLocation = ignitionLocation) %>% 

  # Group and split for iterating over batches
  group_split(BatchID)

updateRunLog("Finished generating model inputs in ", updateBreakpoint())

# Grow fires ----
progressBar("begin", totalSteps = length(fireGrowthInputs))
progressBar(type = "message", message = "Growing fires...")

OutputFireStatistic <- fireGrowthInputs %>%
  map_dfr(runBatch)
  
updateRunLog("Finished burning fires in ", updateBreakpoint())

# Save relevant outputs ----

## Fire statistics table ----
# Generate the table if it is a requested output, or resampling is requested
if(OutputOptions$FireStatistics | minimumFireSize > 0) {
  progressBar(type = "message", message = "Generating fire statistics table...")
  
  # Add extra information to Fire Statistic table
  OutputFireStatistic <- augmentOutputFireStatistic(
    OutputFireStatistic = OutputFireStatistic,
    firesToBurn = firesToBurn,
    DeterministicBurnCondition = DeterministicBurnCondition)
  

    # Output if there are records to save
    if(!isDatasheetEmpty(OutputFireStatistic))
      saveDatasheet(myScenario, OutputFireStatistic, "burnP3Plus_OutputFireStatistic", append = T)
  
  updateRunLog("Finished collecting fire statistics in ", updateBreakpoint())
}

## Tabular burn maps ----
consolidateTabularOutputs()

## Burn perimeters ----
consolidateVectorOutputs()

# Clean up
progressBar("end")
