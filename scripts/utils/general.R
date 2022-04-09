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
