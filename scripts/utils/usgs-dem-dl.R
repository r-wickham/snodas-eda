#'
#' Download 1 arc second DEM from USGS website given geospatial extents
#'
#' Primary URL:
#' https://www.sciencebase.gov/catalog/item/4f70aa71e4b058caae3f8de1
#' 
#' Host on AWS:
#' https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/
#'   historical/n27w099/USGS_1_n27w099_20201228.tif
#'
#' The northing and westing strings define the end extent of the 
#'   1 arc-second rectangle.
#'   e.g., if the northing is 27 and westing is 99 in the definition,
#'   then the rectangle extents are northing = 26-27 deg,
#'   westing = 98-99 deg
#'

library(raster)
library(rgdal)
library(tidyverse)

source("scripts/utils/url.R")

# URL string template for sprintf printing
# first and second strings are northing and westing concatenation
#   e.g., "n27w099"
# thirst string is the date as a string in %Y%m%d format
#   e.g., 20201228
demUrlTemplate <- paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
                         "Elevation/1/TIFF/historical/%s/",
                         "USGS_1_%s_%s.tif", collapse = "")

# PROJ4 string for WGS84 GCS
proj4String_wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#' Download DEM to tif format, returning the download file location
dlTif <- function(northingWestingString){
  if(!dir.exists(rawUsgsdemDir)) dir.create(rawUsgsdemDir)
  
  # Check if a DEM already downloaded
  regExFile <- sprintf("USGS_1_%s.*.tif", northingWestingString)
  toFile <- list.files(path = rawUsgsdemDir,
                       pattern = regExFile,
                       full.names = T)
  if(length(toFile) >= 1){
    writeLog(sprintf("\nLoading DEM from file:\t%s", toFile[1]))
    return(toFile[1])
  }
  # Create url, download file, return file location
  urlString <- findUrlForNorthingWestingDEM(northingWestingString)
  toFile <- sprintf("%s/%s", rawUsgsdemDir, basename(urlString))
  writeLog(sprintf("\nLoading DEM from url:\t%s", toFile[1]))
  try(curl::curl_download(url = urlString, destfile = toFile, quiet=T))
  toFile
}

#' Get the Norhting-Westing strings as expected by the 1-arc second
#'   USGS dataset from an extent object
#' @param shpObj Spatial* object that extents can be extracted from
#' @return strings defining
extentsToNWStrings <- function(shpObj){
  maxExtents <- 
    # Ensure using WGS84
    shpObj %>%
    spTransform(CRSobj = proj4String_wgs84) %>%
    extent() %>%
    # Find floor of minimums and ceiling of maximum extents
    getMaxExtents()
  # Expand to all combinations of northing and westings
  nwCombos <- 
    expand.grid(n = maxExtents@ymin:maxExtents@ymax,
                w = maxExtents@xmin:maxExtents@xmax)
  sprintf("n%02gw%03g", nwCombos$n, -nwCombos$w)
}

#' Given a northing and westing url for USGS 1 Arc-Second DEMs,
#'   finds the latest DEM version's URL and resturns as string
#' @param northingWestingString northing and westing string corresponding
#'                                to DEM.  e.g., "n27w099"
#' @param debug                 logical, toggles print statement
#' @param string, url string to be used to pull 1 arc-second DEM
findUrlForNorthingWestingDEM <- function(northingWestingString, debug = F){
  # Iterate from current date to 2000, checking if URL exists
  suppressWarnings({
    tryCatch({
      d <- Sys.Date()
      while(d >= as.Date("2000-01-01")){
        dateString <- format(d, "%Y%m%d")
        testUrl <- sprintf(demUrlTemplate,
                           northingWestingString,
                           northingWestingString,
                           dateString)
        
        writeLog(sprintf("Searching for raster url:\t%s", testUrl))
        if(urlExists(testUrl)){
          return(testUrl)
        }
        if(debug) cat("\n",d)
        d <- d - 1
      }
    }, error = function(e){
      message(e)
      return(NULL)
    })
  })
}

#' Northing and westing concatenated string from northing and
#'   westing as integers, following nXXwXXX format
#' @param northing integer, defining the upper (north) bound of the 
#'                   1 arc-second rectangle
#' @param westing  integer, defining the left (west) bound of the 1 arc-second
#'                   rectangle 
#' @return string of northing and westing. e.g., "n27w121"
formNorthingWestingString <- function(northing, westing){
  sprintf("n%2gw%04g", northing, westing)
}

#' Round out extents of an extent object to nearest digit
#' @param exObj extent object
#' @param roundDigit the precision at which to round the digits
#' @return extent object with expanded boundaries
getMaxExtents <- function(exObj, roundDigit = 1){
  roundMultiplier <- 10^(roundDigit-1)
  exObj@xmin <- floor(exObj@xmin*roundMultiplier)/roundMultiplier
  exObj@ymin <- floor(exObj@ymin*roundMultiplier)/roundMultiplier
  exObj@xmax <- ceiling(exObj@xmax*roundMultiplier)/roundMultiplier
  exObj@ymax <- ceiling(exObj@ymax*roundMultiplier)/roundMultiplier
  exObj
}

#' Download the USGS 1-arc second DEM given an extent object or
#'   an ojbect that may be converted to an shape object
#' @param shpObj Spatial* object that extents can be extracted from
#' @return 1 arc-second raster spanning extents of Spatial* object
getOneArcSecDemFromShp <- function(shpObj){
  tryCatch({
    # Create a cache file name
    cacheFileName <- sprintf("%s/%s.tif",
                             cacheDemDir,
                             hash(shpObj))
    if(file.exists(cacheFileName))
      return(raster(cacheFileName))
    # Interpret extents to northing-westing string(s)
    nwStrings <-
      shpObj %>%
      extentsToNWStrings()
    # Create compute cluster
    cl <- makeCluster(min(detectCores()-1, length(nwStrings)))
    # Load libraries
    clusterEvalQ(cl = cl,
                 expr = {library(curl); library(purrr); library(raster)})
    # Export objects to compute nodes
    clusterExport(cl = cl,
                  varlist = c("demUrlTemplate",
                              "dlTif",
                              "findUrlForNorthingWestingDEM",
                              "rawUsgsdemDir",
                              "urlExists",
                              "writeLog"))
    # Download USGS 1-arc second DEMs
    out <-
      clusterMap(cl = cl,
                 fun = getRasterFromNWString,
                 northingWestingString = nwStrings) %>%
      compact() %>%
      reduce(merge)
    # Clip to shape object extents
    out <- raster::crop(x = out, y = shpObj)
    # Save cache file
    writeRaster(x = out, filename = cacheFileName)
    return(out)
  }, error =function(e){
    return(NULL)
  }, finally = {
    if(exists("cl"))
      stopCluster(cl)
  })
}

#' Get a one arc-second raster from a northing-westing string
#' @param northingWestingString northing and westing concatenation.
#'                                e.g., "n27w099"
#' @return raster object corresponding to northing and westing extents
getRasterFromNWString <- function(northingWestingString){
  library(purrr)
  library(raster)
  northingWestingString %>%
    dlTif() %>%
    raster()
}

### Test ----------------------------------------------------------------------

if(F){
  system.time({
    source("webapp/global.R")
    shpObj <- readOGR(dsn = "data/vector/test/WBDHU6.shp")
    dem <- getOneArcSecDemFromShp(shpObj)
    plot(dem)
    plot(shpObj, add = T)
  })
}
