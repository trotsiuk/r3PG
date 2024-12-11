# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)

library(ggplot2)

library(r3PG)

# 1. Load and simulate the r3PG -------------------------------------------
f_input <- 'tests/r_vba_compare/r3PG_input.xls'

site.df <- read_xls(f_input, 'site')
species.df <- read_xls(f_input, 'species')
climate.df <- read_xls(f_input, 'climate')
thinn.df <- read_xls(f_input, 'thinning')
param.df <- read_xls(f_input, 'parameters')
sizeDist.df <- read_xls(f_input, 'sizeDist')
settings.df <- read_xls(f_input, 'settings')

# run the simulation
r3PG_out.df <- tibble()

for( site_id in settings.df$site){
  # site_id = "mixtures_eu"
  # site_id = 'mixtures_other'
  # site_id = 'broadleaf_pjs'
  # site_id = 'evergreen_pjs'
  
  settings_i = filter(settings.df, site %in% site_id)
  
  site_i = filter(site.df, site %in% site_id) %>% .[,-1]
  species_i = filter(species.df, site %in% site_id) %>% .[,-1]
  climate_i = filter(climate.df, 
    climate_id %in% settings_i$climate_id) %>%
    select_if(function(x){!all(is.na(x))}) %>%
    select(-climate_id)
  if( settings_i$thinning == 'yes'){
    thinn_i = filter(thinn.df, site %in% site_id) %>% .[,-1]
  }else{ 
    thinn_i = NULL
  }
  param_i = select(param.df, c('parameter', species_i$species))
  sizeDist_i = select(sizeDist.df, c('parameter', species_i$species))
  
  out <- run_3PG(site_i, species_i, climate_i, thinn_i, param_i, sizeDist_i, as.list(settings_i[,2:7])) %>%
    mutate( model = 'r3PG',
      site = site_id)
  
  r3PG_out.df <- bind_rows( r3PG_out.df, out)
  
}


# 2. Load and transform VBA -----------------------------------------------
var_names_vba <- select(i_output, variable_vba, variable_name) %>% 
  filter(nchar(variable_vba)>0) %>% 
  tibble::deframe()

tranf_vba <- function(sk = 5, n_m = 111, sp_names = 'Fagus sylvatica', site = 'evergreen_pjs'){
  #' @description tranforming the output of the Excel VBA 3PG run to match the long format
  
  out <- readxl::read_xls( 'tests/r_vba_compare/VBA_3PG_output.xls', sheet = 'Output', skip = sk, n_max = n_m) %>%
    # head() %>%
    rename( date = `Year & month`) %>%
    mutate( date = gsub("[[:space:]]", "", date) %>% anytime::anydate(.) ) %>%
    mutate( date = date + months(1) - 1) %>%
    mutate_at( vars(-date), list( as.numeric) ) %>%
    gather( variable_vba, value, -date) %>%
    filter( !is.na( value) ) %>%
    # get species id
    mutate( species = stringr::str_sub(variable_vba, -1, -1)) %>%
    rowwise() %>%
    mutate(
      species = as.numeric(species),
      variable_vba =  if_else( is.na( species ), variable_vba, stringr::str_sub(variable_vba, 1, nchar(variable_vba)-1)),
      variable = var_names_vba[variable_vba],
      species = if_else( is.na( species ), 1, species ),
      species = sp_names[species],
      model = 'VBA',
      site = site) %>%
    ungroup() %>%
    filter(!is.na(variable)) %>%
    select(date, species, variable, value, model, site)
  
  return(out)
}


VBA_out.df <- bind_rows(
  tranf_vba(5, 111, 'Pinus sylvestris', 'evergreen_pjs'),
  tranf_vba(120, 111, 'Pinus sylvestris', 'evergreen_mix'),
  tranf_vba(235, 111, 'Fagus sylvatica', 'broadleaf_pjs'),
  tranf_vba(350, 111, 'Fagus sylvatica', 'broadleaf_mix'),
  tranf_vba(465, 111, c('Fagus sylvatica', 'Pinus sylvestris'), 'mixtures_eu'),
  tranf_vba(580, 123, c('Cunninghamia lanceolata', 'Liquidambar formosana'), 'mixtures_other')
)



# 3. Explore the final results --------------------------------------------

data.df <- bind_rows( r3PG_out.df, VBA_out.df)


i_var <- c('stems_n', 'biom_stem', 'biom_root', 'biom_foliage')
i_lab <- c('Stem density', 'Stem biomass', 'Root biomass', 'Foliage biomass')

data.df %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = model, linetype = species), size = 0.5)+
  facet_grid( variable ~ site, scales = 'free') +
  theme_classic()

ggsave('data-raw/r_vba_compare/r3pg_vba.png', width = 15, height = 8, units = c("in"), dpi = 'retina', bg = "transparent")


i_var <- c('stems_n', 'biom_stem', 'biom_root', 'biom_foliage')
ast <- data.df %>%
  filter(date == as.Date('2009-12-31')) %>%
  filter(variable %in% i_var) %>%
  select(site, species, variable, value, model) %>%
  spread(model, value)
