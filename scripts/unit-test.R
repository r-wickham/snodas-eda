#'
#'
#'

# Load necessary functions
source("webapp/global.R")

# Load shapefile, DEM, and SNODAS
polyObj <- rgdal::readOGR(dsn = "data/vector/test/WBDHU6.shp")
baseRaster <- getOneArcSecDemFromShp(polyObj)
extractRasters <- getSnodas()


### CONTINUE HERE
# May want to swap base/extract rasters
# logic for polyObj = NULL

extractPtsFromTwoRastersInPoly <-
  function(baseRaster, extractRasters, polyObj = NULL){
    
    tryCatch({
      # Clip base and extract raster to polygon extents
      b <- raster::crop(x = baseRaster, y = polyObj)
      e <- raster::crop(x = extractRasters, y = polyObj)
      
      # Extract points from polygon
      pPts <- polyToPts(polyObj)
      
      # Pull points from extract raster
      sXY <- 
        rasterToPoints(x = e) %>%
        as.data.frame()
      
      sXY %>%
        select(c("x","y")) %>%
        mutate(
          base = raster::extract(x = baseRaster, y = .),
          inPoly = sp::point.in.polygon(point.x = x,
                                        point.y = y,
                                        pol.x = pPts$x,
                                        pol.y = pPts$y),
          extract = sXY$layer
        )
    }, error = function(e){
      message(e)
      return(NULL)
    })
    
    
  }

normalize <- function(x, minVal = 0, maxVal = 1){
  y <- (x - min(x, na.rm=T))/diff(range(x, na.rm=T))
  y*(maxVal - minVal) + minVal
}

rData <-
  extractPtsFromTwoRastersInPoly(baseRaster, extractRasters, polyObj)

library(rgl)
plot3d(rData$x, rData$y, rData$base, col = hsv(normalize(rData$extract)))

