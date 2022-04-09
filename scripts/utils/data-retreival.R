#'
#'
#'




#' Create a data.frame with merged elevation and SNODAS data from a date
#'   and boundary
#'   
#' @param snodasPar  string, the SNODAS parameter to download
#' @param date       string or Date, defining which date to pull data from
#' @param shp        SpatialPointsDataframe or x-y data.frame defining
#'                     boundary of locations to pull data.  CRS must be
#'                     WGS84.
getSNODASElevationDataframe <- 
  function(snodasParType,
           date,
           shp){
    # Get the SNODAS data from that date
    rSnodas <- getSnodas(date = date, parType = snodasParType)
    
    # Get the USGS DEM(s) for extents
    rDEM <- getOneArcSecDemFromShp(shp)
    
    # Extract overlapping points in raster
    NULL
  }