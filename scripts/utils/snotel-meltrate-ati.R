#'
#' SNOTEL meltrate ATI relationship development
#'

library(lubridate)
library(snotelr)
library(tidyverse)

### Config --------------------------------------------------------------------

sntlIds <- c(721, 563, 357)

### Functions -----------------------------------------------------------------

wateryear <- function(dates){
  wy <- year(dates)
  wy[month(dates) >= 10] <- wy + 1
  wy
}

#' Compute daily accumulated temperature index
computeATI <- function(x){
  x %>%
    group_by(wateryear(date), site_name) %>%
    mutate(
      ati = cumsum(temperature_mean)
    ) %>%
    ungroup()
}

### Main ----------------------------------------------------------------------

# Download raw data
sntlRaw <-
  sntlIds %>%
  map(snotel_download, internal = T) %>%
  bind_rows() %>%
  mutate(
    date = as.Date(date)
  )


test <-
  sntlRaw %>%
  na.omit() %>%
  group_by(site_name) %>%
  mutate(
    date_diff = c(NA, diff(date))
  ) %>%
  ungroup()


plot(sntlRaw)


snlt <- sntlRaw %>%
  map(
    .f = function(x){
      x$swe_diff <- c(NA, diff(x$snow_water_equivalent))
      x
    }
  ) %>%
  bind_rows() %>%
  mutate(
    ati = computeATI(.)
  ) %>%
  na.omit()




