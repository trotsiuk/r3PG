
# libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(readxl)

options(digits=15)


# EU_MIXFOR ---------------------------------------------------------------
#' `EU MIXFOR`
site_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'site') %>%
  mutate(Altitude = 300) %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth, Altitude)

species_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'species') %>%
  mutate(species = 1:n()) %>%
  select(species, pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStocking =  `Initial stocking`)

parameters_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`, sp2 = `Pinus sylvestris3`)

bias_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`, sp2 = `Pinus sylvestris3`)

climate_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'climate') %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2)

thinning_eum <- read_excel('dev/input_data/input_eu_mixed.xlsx', sheet = 'thinning')

save(site_eum, species_eum, climate_eum, parameters_eum, bias_eum, thinning_eum, file = 'pkg/data/eu_mixfor.rda')

load(file = 'pkg/data/eu_mixfor.rda')



# SHITAI3 ----------------------------------------------------------------
site_shi3 <- read_excel('dev/data/input_Shitai3.xlsx', sheet = 'site') %>%
  mutate(Altitude = 300) %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth, Altitude)


species_shi3 <- read_excel('dev/data/input_Shitai3.xlsx', sheet = 'species') %>%
  mutate(species = 1:n()) %>%
  select(species, pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStocking =  `Initial stocking`)


parameters_shi3 <- read_excel('dev/data/input_Shitai3.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Cunninghamia lanceolata`, sp2 = `Liquidambar formosana`)

bias_shi3 <- read_excel('dev/data/input_Shitai3.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Cunninghamia lanceolata`, sp2 = `Liquidambar formosana`)

climate_shi3 <- read_excel('dev/data/input_Shitai3.xlsx', sheet = 'climate') %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2)

save(site_shi3, species_shi3, climate_shi3, parameters_shi3,bias_shi3, file = 'pkg/data/shitai_3.rda')



# SHITAI23 -----------------------------------------------------------------
site_shi23 <- read_excel('dev/data/input_Shitai23.xlsx', sheet = 'site') %>%
  mutate(Altitude = 300) %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth, Altitude)


species_shi23 <- read_excel('dev/data/input_Shitai23.xlsx', sheet = 'species') %>%
  mutate(species = 1:n()) %>%
  select(species, pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStocking =  `Initial stocking`)


parameters_shi23 <- read_excel('dev/data/input_Shitai23.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Castanopsis sclerophylla`, sp2 = `Cunninghamia lanceolata`)

bias_shi23 <- read_excel('dev/data/input_Shitai23.xlsx', sheet = 'bias') %>%
  select(parameter = Name, sp1 = `Castanopsis sclerophylla`, sp2 = `Cunninghamia lanceolata`)

climate_shi23 <- read_excel('dev/data/input_Shitai23.xlsx', sheet = 'climate') %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2)

save(site_shi23, species_shi23, climate_shi23, parameters_shi23,bias_shi23, file = 'pkg/data/shitai_23.rda')



# 2. Single species version -----------------------------------------------
#' FASY
site_fasy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'site') %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth)


species_fasy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'species') %>%
  filter(Species %in% 'Fagus sylvatica') %>%
  select(pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStemNo =  `Initial stocking`) %>%
  mutate_at(vars(iWF, iWR, iWS, iStemNo), funs(.*2))


parameters_fasy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Fagus sylvatica`) %>%
  .[c(1:42, 44:50,55:62, 64:67),]

climate_fasy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'climate') %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2)

thinning_fasy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'thinning')

save(site_fasy, species_fasy, climate_fasy, parameters_fasy,thinning_fasy, file = 'pkg/data/eu_fasy.rda')

load(file = 'pkg/data/eu_pisy.rda')


#' `PISY`
site_pisy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'site') %>%
  select(latitude = Latitude, soilClass = `Soil class`, iASW = `Initial ASW`, minASW = `Min ASW`, maxASW = `Max ASW`, iYear, iMonth)


species_pisy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'species') %>%
  filter(Species %in% 'Pinus sylvestris3') %>%
  select(pYear, pMonth, fertilityRating = `Fertility rating`, iWF = `Initial WF`, iWR = `Initial WR`, iWS = `Initial WS`, iStemNo =  `Initial stocking`) %>%
  mutate_at(vars(iWF, iWR, iWS, iStemNo), funs(.*2))


parameters_pisy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'parameters') %>%
  select(parameter = Name, sp1 = `Pinus sylvestris3`) %>%
  .[c(1:42, 44:50,55:62, 64:67),]

climate_pisy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'climate') %>%
  select(Tmin, Tmax, Rain, sRad = `Solar rad`, fDays = `Frost Days`, co2 = CO2)

thinning_pisy <- read_excel('dev/data_test/eu_mixed.xlsx', sheet = 'thinning')

save(site_pisy, species_pisy, climate_pisy, parameters_pisy,thinning_pisy, file = 'pkg/data/eu_pisy.rda')
