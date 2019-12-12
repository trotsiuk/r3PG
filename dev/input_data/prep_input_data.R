
# libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(readxl)

options(digits=16)



# Variable naming ---------------------------------------------------------
var_names <- readxl::read_excel('dev/r3PGmix_info.xlsx', sheet = 'var_names') %>%
  select(group_id, variable_id, variable_group, variable_name, description, unit, variable_vba)

save(var_names, file = 'pkg/data/r3pg_helpers.rda')


# EU_MIXFOR ---------------------------------------------------------------
#' `EU MIXFOR`
site_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'site') %>%
  mutate(Altitude = 100) %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth, Altitude)

species_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'species') %>%
  mutate(species = 1:n()) %>%
  select(species, pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStocking =  `Initial stocking`)

parameters_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`, sp2 = `Pinus sylvestris3`)

bias_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`, sp2 = `Pinus sylvestris3`)

climate_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'climate') %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2, d13catm)

thinning_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'thinning')

save(site_eum, species_eum, climate_eum, parameters_eum, bias_eum, thinning_eum, file = 'pkg/data/eu_mixfor.rda')

load(file = 'pkg/data/eu_mixfor.rda')


# Shitai_23 ---------------------------------------------------------------
#' `Shitai-23`
site_shi23 <- read_excel('dev/input_data/input_shitai23.xlsx', sheet = 'site') %>%
  mutate(Altitude = 100) %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth, Altitude)

species_shi23 <- read_excel('dev/input_data/input_shitai23.xlsx', sheet = 'species') %>%
  mutate(species = 1:n()) %>%
  select(species, pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStocking =  `Initial stocking`)

parameters_shi23 <- read_excel('dev/input_data/input_shitai23.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Castanopsis sclerophylla`, sp2 = `Cunninghamia lanceolata`)

bias_shi23 <- read_excel('dev/input_data/input_shitai23.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Castanopsis sclerophylla`, sp2 = `Cunninghamia lanceolata`)

climate_shi23 <- read_excel('dev/input_data/input_shitai23.xlsx', sheet = 'climate') %>%
  mutate( d13catm = 0) %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2, d13catm)

thinning_shi23 <- read_excel('dev/input_data/input_shitai23.xlsx', sheet = 'thinning')

save(site_shi23, species_shi23, climate_shi23, parameters_shi23, bias_shi23, thinning_shi23, file = 'pkg/data/shitai23.rda')

load(file = 'pkg/data/shitai23.rda')




# Shitai_3 ---------------------------------------------------------------
#' `Shitai-3`
site_shi3 <- read_excel('dev/input_data/input_shitai3.xlsx', sheet = 'site') %>%
  mutate(Altitude = 100) %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth, Altitude)

species_shi3 <- read_excel('dev/input_data/input_shitai3.xlsx', sheet = 'species') %>%
  mutate(species = 1:n()) %>%
  select(species, pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStocking =  `Initial stocking`)

parameters_shi3 <- read_excel('dev/input_data/input_shitai3.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Cunninghamia lanceolata`, sp2 = `Liquidambar formosana`)

bias_shi3 <- read_excel('dev/input_data/input_shitai3.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Cunninghamia lanceolata`, sp2 = `Liquidambar formosana`)

climate_shi3 <- read_excel('dev/input_data/input_shitai3.xlsx', sheet = 'climate') %>%
  mutate( d13catm = 0) %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2, d13catm)

thinning_shi3 <- read_excel('dev/input_data/input_shitai3.xlsx', sheet = 'thinning')

save(site_shi3, species_shi3, climate_shi3, parameters_shi3, bias_shi3, thinning_shi3, file = 'pkg/data/shitai3.rda')

load(file = 'pkg/data/shitai3.rda')

