
# libraries ---------------------------------------------------------------

library(dplyr)
library(readxl)

options(digits=16)


# EU_MIXFOR ---------------------------------------------------------------
#' `EU MIXFOR`
d_site <- read_excel('data-raw/internal_data/data.input.xlsx', sheet = 'site')
  
d_species <- read_excel('data-raw/internal_data/data.input.xlsx', sheet = 'species')

d_climate <- read_excel('data-raw/internal_data/data.input.xlsx', sheet = 'climate')

d_parameters <- read_excel('data-raw/internal_data/data.input.xlsx', sheet = 'parameters')

d_sizeDist <- read_excel('data-raw/internal_data/data.input.xlsx', sheet = 'sizeDist')

d_thinning <- read_excel('data-raw/internal_data/data.input.xlsx', sheet = 'thinning')


usethis::use_data( d_site, d_species, d_climate, d_parameters, d_sizeDist, d_thinning, overwrite = TRUE)
  
