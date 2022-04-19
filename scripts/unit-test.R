#'
#'
#'

# Load necessary functions
source("webapp/global.R")

# Load shapefile, DEM, and SNODAS
shpObj <- rgdal::readOGR(dsn = "data/vector/test/WBDHU6.shp")
dem <- getOneArcSecDemFromShp(shpObj)
s <- getSnodas()

extractPtsFromTwoRastersInPoygon <-
  function(baseRaster, extractRaster, polyObj){
    
    # Clip SNODAS raster to polygon extents
    b <- raster::crop(x = baseRaster, y = polyObj)
    
    # Extract points from polygon
    pPts <- polyToPts(polyObj)
    
    # Pull points from SNODAS raster
    sXY <- rasterToPoints(x = b) %>%
      as.data.frame()
    
    sXY %>%
      select(c("x","y")) %>%
      mutate(
        dem = raster::extract(x = extractRaster, y = .),
        inBasin = sp::point.in.polygon(point.x = x,
                                       point.y = y,
                                       pol.x = pPts$x,
                                       pol.y = pPts$y),
        snodas = sXY$layer
      )
  }

par(xaxs = "i", yaxs = "i")
plot(data$dem,
     data$snodas,
     pch=3,
     cex = 0.1,
     col = grey(level = 0.8, alpha = 0.8))



