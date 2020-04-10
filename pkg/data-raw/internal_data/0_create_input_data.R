
# libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(readxl)

options(digits=16)


# EU_MIXFOR ---------------------------------------------------------------
#' `EU MIXFOR`
site_eum <- read_excel('data-raw/internal_data/input_eum.xlsx', sheet = 'site')
  
species_eum <- read_excel('data-raw/internal_data/input_eum.xlsx', sheet = 'species')

climate_eum <- read_excel('data-raw/internal_data/input_eum.xlsx', sheet = 'climate') %>%
  select(year, month, tmp_min, tmp_max, tmp_ave, prcp, srad, frost_days, co2, d13catm)

parameters_eum <- read_excel('data-raw/internal_data/input_eum.xlsx', sheet = 'parameters')

sizeDist_eum <- read_excel('data-raw/internal_data/input_eum.xlsx', sheet = 'sizeDist')

thinn_eum <- read_excel('data-raw/internal_data/input_eum.xlsx', sheet = 'thinning')


usethis::use_data( site_eum, species_eum, climate_eum, parameters_eum, sizeDist_eum, thinn_eum, overwrite = TRUE)
  
