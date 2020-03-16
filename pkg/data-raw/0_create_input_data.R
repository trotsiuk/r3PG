
# libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(readxl)

options(digits=16)


# EU_MIXFOR ---------------------------------------------------------------
#' `EU MIXFOR`
site_eum <- read_excel('data-raw/input_eum.xlsx', sheet = 'site') %>%
  mutate(Altitude = 100) %>%
  select(latitude = Latitude, soil_class = `Soil class`, asw_i = `Initial ASW`, asw_min = `Min ASW`, asw_max = `Max ASW`, year_i = iYear, month_i = iMonth, altitude = Altitude)
  
species_eum <- read_excel('data-raw/input_eum.xlsx', sheet = 'species') %>%
  mutate(species = c('sp1', 'sp2')) %>%
  select(species, year_p = pYear, month_p = pMonth, fertility = `Fertility rating`, biom_foliage = `Initial WF`, biom_root = `Initial WR`, biom_stem = `Initial WS`, n_trees =  `Initial stocking`) 

climate_eum <- read_excel('data-raw/input_eum.xlsx', sheet = 'climate') %>%
  select(tmp_min = Tmin, tmp_max = Tmax, prcp = Rain, srad = `Solar rad`, frost_days = `Frost Days`, co2 = CO2, d13catm)

parameters_eum <- read_excel('data-raw/input_eum.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`, sp2 = `Pinus sylvestris3`)
parameters_eum$sp1[11] <- 5

bias_eum <- read_excel('data-raw/input_eum.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`, sp2 = `Pinus sylvestris3`) 

thinn_eum <- data.frame(species = c('sp1', 'sp2', 'sp2'), age = c(48, 47, 50), n_trees = c(500, 600, 400), foliage = 1, root = 1, stem = 1)



usethis::use_data( site_eum, species_eum, climate_eum, parameters_eum, bias_eum, thinn_eum, overwrite = TRUE)
  
