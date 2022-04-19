#'
#' General functions 
#'


#' Loads from config.yaml file and exports list to global environment
configToGlobal <- function(configFileName = "config.yaml"){
  configFileName %>%
    read_yaml() %>%
    list2env(envir = .GlobalEnv)
  NULL
}


#' Read shapefile with error handling
readShp <- function(fileName){
  tryCatch(
    {
      return(invisible(readOGR(fileName)))
    }, error = function(e){
      return(NULL)
    })
}

#' Remove log file
removeLog <- function(){
  if(file.exists("log.txt"))
    try(file.remove("log.txt"))
}

#' Extract SpatialPolygon points along perimeter
#'
#' @param shpObj SpatialPolygon 
#'
#' @return data.frame with "x" and "y" columns for points along the perimeter
polyToPts <- function(shpObj){
  shpObj@polygons[[1]]@Polygons[[1]]@coords %>%
    as.data.frame() %>%
    set_names(c("x","y"))
}

#' Write to log text file
#'
#' @param m String, message to write
writeLog <- function(m = ""){
  write(x = as.character(m), file = "log.txt", append = T)
}
