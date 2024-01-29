#'
#' Set global environment for webapp
#'


library(curl)
library(parallel)
library(purrr)
library(raster)
#library(rgdal)
library(sp)
library(rlang)
library(shiny)
library(sp)
library(tidyverse)
library(yaml)


# Source all utility scripts
dir(path = "scripts/utils", pattern = "*.R", full.names = T) %>%
  map(source)
  
removeLog()

# Load from config file to global environment
configToGlobal()

# ensure we have directories set up
if(!dir.exists("data")) dir.create("data")
