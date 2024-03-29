#'
#'
#'

# Load necessary functions
source("webapp/global.R")

### SNODAS --------------------------------------------------------------------

s <- getSnodas()
plot(s)

### SNODAS-Terrain -----------------------------------------------------------

# Load shapefile, DEM, and SNODAS
polyObj <- rgdal::readOGR(dsn = "data/vector/test/WBDHU6.shp")
baseRaster <- getOneArcSecDemFromShp(polyObj)
extractRasters <- getSnodas()


### CONTINUE HERE
# logic for polyObj = NULL

extractPtsFromTwoRastersInPoly <-
  function(baseRaster, extractRasters, polyObj = NULL){
    
    tryCatch({
      if(is.null(polyObj)){
        return(NULL) # placeholder
      }else{
        # Clip base and extract raster to polygon extents
        b <- raster::crop(x = baseRaster, y = polyObj)
        e <- raster::crop(x = extractRasters, y = polyObj)
      }

      
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

# Plot terrain, colorized by SWE
library(rgl)
plot3d(rData$x, rData$y, rData$base, col = hsv(normalize(rData$extract)))

# Plot extracted SWE
plot3d(rData$x, rData$y, rData$extract)


# Plot relationship of elevation to SWE
pData <- rData %>% subset(inPoly  == 1 & extract > 0)
plot(pData$base, pData$extract, pch=3,
     cex=0.1, col = grey(0.3,0.3),
     xlab = "Elevation (ft)", ylab = "SWE (in)")

# Multi-plot
graphics::layout(matrix(c(1,2,3,3), 2,2, byrow=T))
par(mar=c(4,4,2,2))
plot(s)
plot(polyObj, add=T, col="red")
hist(pData$extract, main = "", xlab = "SWE (m)",freq=F)
plot(pData$base, pData$extract, pch=3,
     cex=0.1, col = grey(0.3,0.3),
     xlab = "Elevation (ft)", ylab = "SWE (m)")


