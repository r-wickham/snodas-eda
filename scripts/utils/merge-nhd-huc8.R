#' Merge CONUS HUC8 vectors downloaded from NHD
#'
#'
#'
#' Expecting HUC8 shapefiles to be relative to working directory, defined as:
#'   ../data/vector/huc/*/Shape/WBDH%g.shp
#'

library(raster)
library(rgdal)
library(tidyverse)
library(sp)
library(tools)

if(F){
  
  baseHucDir <- "data/vector/huc"
  
  hucsToMerge <- c(2,4,6,8,10,12,14,16)
  
  
  
  #'
  #' TODO
  #' THere are a lot of duplicate shapefiles in the dataset
  #' Filter these out based on some metadata for unique object ids
  #'
  
  # Filter a spatial SpatialPolyDataframe using a boolean
  filterSpatialPolyDf <- function(shp, index){
    shp[index,]
  }
  
  
  for(huc in hucsToMerge){
    
    # Iterate through each directory in baseHucDir,
    #   search for path Shape/WBDH%g.shp, where %g = huc
    hucDirs <- dir(baseHucDir, full.names = T)
    shpFileBaseName <- sprintf("WBDHU%g.shp", huc)
    
    shpFiles <- file.path(hucDirs, "Shape", shpFileBaseName)
    
    saveFileName <- 
      file.path(getwd(), "data", "vector",
                sprintf("huc%02g.shp", huc))
    layerName <- saveFileName %>%
      file_path_sans_ext() %>%
      basename()
    
    
    if(!dir.exists(dirname(saveFileName))) dir.create(dirname(saveFileName))
    
    # Create an iterator through all of the possible shapefiles,
    #   with the combination function that removes any duplicates
    #   and calls a garbage collection
    
    
    
    
    
    # New test, keeping only unique HUC
    s <- shpFiles[1:500] %>%
      map(readShp) %>%
      compact() %>%
      reduce(rbind) %>%
      filterSpatialPolyDf(!duplicated(.@data$objectid)) %>%
      writeOGR(dsn = saveFileName,
               layer = layerName,
               driver = "ESRI Shapefile")
    
    
    # This worked!
    s1 <- readOGR(shpFiles[1])
    s2 <- readOGR(shpFiles[100])
    s <- list(s1, s2) %>% reduce(rbind)
    writeOGR(obj = s, dsn = "D:/temp/test.shp", layer = "test", driver = "ESRI Shapefile")
    s <- readOGR(dsn = "D:/temp/test.shp")
    
    
    # Trying to chain it together - this worked too!
    s <- shpFiles[1:10] %>%
      map(readOGR) %>%
      reduce(rbind) #%>%
    # writeOGR(dsn = saveFileName,
    #          layer = layerName,
    #          driver = "ESRI Shapefile")
    
  }
  
  
}
