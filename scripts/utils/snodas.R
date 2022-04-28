#'
#' Functions for downloading and loading SNODAS dataa
#'

library(raster)
library(parallel)
library(doParallel)
library(foreach)
library(lubridate)
library(purrr)
library(rgdal)
library(tidyverse)

### Config --------------------------------------------------------------------

# Standard location of 7z command line
sevenZipExe <- "C:/Program Files/7-Zip/7z.exe"

### Constants -----------------------------------------------------------------

# Provides a pairing between the parameter and the file
#   that gets downloaded from the ftp.  Only need
#   to add in a date in the form of YYYYMMDD 
#   to the file template using sprintf
snodasMeta <- data.frame(
  parType =
    c("swe",
      "snow depth",
      "snow melt runoff at the base of the snow pack",
      "sublimination from the snow pack", 
      "sublimination from blowing snow",
      "solid precipitation",
      "liquid precipitation",
      "snow pack average temperature"
    ),
  code = c(1034,1036,1044,1050,1039,1025, 1025,1038),
  # https://nsidc.org/data/g02158: To convert integers in files to model
  #  output values, divide integers in files by scale factor
  scaleFactor =c(1000,1000,100000,100000,100000,10,10,1), 
  dirName=c("swe","depth","snow_melt","sublim_snow_pack",
            "sublim_blowing_snow","precip_solid","precip_liquid","snow_temp"),
  vcode= c("",  "",  "",  "",  "", "IL01","IL00",  ""),
  fileTemplate=c(
    "us_ssmv11034tS__T0001TTNATS\\d+05HP001.dat.gz",
    "us_ssmv11036tS__T0001TTNATS\\d+05HP001.dat.gz",
    "us_ssmv11044bS__T0024TTNATS\\d+05DP000.dat.gz",
    "us_ssmv11050lL00T0024TTNATS\\d+05DP000.dat.gz",
    "us_ssmv11039lL00T0024TTNATS\\d+05DP000.dat.gz",
    "us_ssmv01025SlL01T0024TTNATS\\d+05DP001.dat.gz",
    "us_ssmv01025SlL00T0024TTNATS\\d+05DP001.dat.gz",
    "us_ssmv11038wS__A0024TTNATS\\d+05DP001.dat.gz"
  ),
  units=c("m","m","m","m","m","kg_m2","kg_m2","K"),
  stringsAsFactors = F)


# SNODAS info:
# File format: Flat binary, 16-bit signed integer (big-endian)
# From NSDIC Special report
#   (http://nsidc.org/pubs/documents/special/nsidc_special_report_11.pdf)
# byteorder M  big-endian
# layout bil   raster is formatted using Band Interleave by Line (BIL):
#                X[col,band,row]
# nbands 1     Only one band for SWE
# nbits 16     2-byte
# ncols 6935  lines
# nrows 3351  samples
ulxmap <- -124.729583333331703
ulymap <- 52.871249516804028
llxmap <- -66.9420833564
llymap <- 24.9504161946
xdim <- 0.00833333333
ydim <- 0.00833333333

# SNODAS spatial reference and scaling for masked version of contiguous US
nCol <- 6935
nRow <- 3351
snodasCRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

### Functions -----------------------------------------------------------------



#' Download a SNODAS tar file given a date or date string
#' By default, will download to a temporary file location
#' @param dateObj  Date or string object defining date at which to download 
#'                   SNODAS tar file
#' @return         String, location of downloaded tar file
downloadSnodasTar <- 
  function(dateObj = Sys.Date() - 1){
    toFile = sprintf("%s/%s.tar",
                     rawSnodasTarDir,
                     format(Sys.Date(), "%Y-%m-%d"))
    if(file.exists(toFile))
      return(toFile)
    urlString <- formSnodasDlurl(dateObj)
    try(curl::curl_download(urlString, toFile, quiet=T))
    toFile
  }

#' Create the URL to be used for downloading SNODAS from
#'   the Colorado FTP
#' @param date Date or string object defining date at which to download 
#'               SNODAS tar file.  If string, must be formatted in a way
#'               that is accepted by as.Date function
#' @return string of download URL
formSnodasDlurl <- function(date){
  tryCatch({
    monthLabels = c("01_Jan", "02_Feb", "03_Mar", "04_Apr", "05_May", "06_Jun", 
                    "07_Jul", "08_Aug", "09_Sep", "10_Oct", "11_Nov", "12_Dec")
    date <- as.Date(date, origin = "1970-01-01")
    dateTag <- format(date,"%Y%m%d")
    tarFileName <- sprintf("SNODAS_%s.tar", dateTag)
    return(
      sprintf("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/%g/%s/%s",
              year(date), monthLabels[month(date)], tarFileName)
    )
  }, error = function(e){
    warning(e)
    return(NULL)
  })
}

#' Download SNODAS unmasked (US + Columbia basin in Canada) dataset
#' @param date    Date or string object defining date.  By default, will use
#'                 yesterday's date
#' @param parType String defining SNODAS parameter type to load
#'                  Options are "swe", "snow depth", 
#'                  "snow melt runoff at the base of the snow pack",
#'                  "sublimination from the snow pack",
#'                  "sublimination from blowing snow",
#'                  "solid precipitation",
#'                  "liquid precipitation",
#'                  or "snow pack average temperature"
#' @return raster object
getSnodas <- function(date = Sys.Date() - 1, parType = "swe"){
  downloadSnodasTar(dateObj = date) %>%
    untarSnodasFileToDir() %>%
    gzFileToRaster(parType = parType)
}

#' Load raster from SNODAS dat file
#' @param gzFileNames string of gz file to load
#' @return raster object
gzFileToRaster <- function(gzFileNames, parType = "swe"){
  tryCatch({
    # Determine parameter type
    searchString <- snodasMeta$fileTemplate[snodasMeta$parType == parType]
    gzFileName <- 
      gzFileNames[str_detect(gzFileNames, searchString)]
    # Read in binary data, applying scale factor
    dataScaleFactor <- snodasMeta$scaleFactor[snodasMeta$parType == parType]
    rasCon <- gzcon(file(gzFileName,"rb"))
    rasData <- readBin(rasCon, integer(), n=nRow*nCol, size=2,
                       signed=TRUE, endian='big')
    close(rasCon)
    rasData[rasData == -9999] <- NA
    rasData <- rasData/dataScaleFactor
    # Assign data to raster
    r <- raster(x = matrix(rasData, nrow=nRow, ncol=nCol, byrow = T))
    # Assign extents, crs
    extent(r) <- extent(c(ulxmap, llxmap, llymap,  ulymap) )
    crs(r) = snodasCRS
    r
  },
  error = function(e){
    message(e)
    return(NULL)
  }
  )
}

#' Untar tar file into a directory
#' @param tarFileName string, full path to tar file
#' @return string vector, gz file names of untarred file contents
untarSnodasFileToDir <- function(tarFileName){
  tryCatch({
    # Establishing directory to untar the results
    gzDir <- gsub(".tar","",tarFileName)
    system(sprintf('"%s" e -y -o"%s" "%s"', sevenZipExe, gzDir, tarFileName))
    # Return the .gz files in the unpacked dir
    return(dir(gzDir, full.names = T, pattern = "*.dat.gz"))
  },
  error=function(e)
    return(NULL)
  )
}


