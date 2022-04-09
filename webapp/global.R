#'
#' Set global environment for webapp
#'

# Source all utility scripts
dir(path = "scripts/utils", pattern = "*.R", full.names = T) %>%
  map(source)
  
# Load from config file to global environment
configToGlobal()