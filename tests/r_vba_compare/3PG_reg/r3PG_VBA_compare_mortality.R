library(r3PG)
library(dplyr)
library(ggplot2)
library(readxl)

# 1. Load and simulate the r3PG -------------------------------------------
f_input <- 'tests/r_vba_compare/VBA_Mixture2.xls'

site.df <- read_xls(f_input, 'site') %>% prepare_site()
species.df <- read_xls(f_input, 'species') %>% prepare_species()
climate.df <- read_xls(f_input, 'climate') %>% prepare_climate( from = site.df$from, to = site.df$to)
thinn.df <- read_xls(f_input, 'thinning') %>% prepare_thinning( sp_names = species.df$species)
param.df <- read_xls(f_input, 'parameters') %>% prepare_parameters( sp_names = species.df$species)
sizeDist.df <- read_xls(f_input, 'sizeDist') %>% prepare_sizeDist( sp_names = species.df$species)


r3PG_out.df <- run_3PG(site = site.df,
                       species = species.df,
                       climate = climate.df,
                       thinning = thinn.df,
                       parameters = param.df,
                       size_dist = sizeDist.df,
                       settings = list(light_model = 2,      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                       transp_model = 2,     # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                       phys_model = 2,       # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                       height_model = 1,     # '1' - linear (default); '2' - non-linear
                                       correct_bias=1,       # '0' - no (default); '1' - yes
                                       calculate_d13c = 0),  # '0' - no (default); '1' - yes
                       check_input = FALSE, df_out = TRUE) %>%
  dplyr::mutate( model = 'r3PG')


# 2. Load and transform VBA -----------------------------------------------
var_names_vba <- select(i_output, variable_vba, variable_name) %>%
  filter(nchar(variable_vba)>0) %>%
  tibble::deframe()


VBA_out.df <- readxl::read_xls( 'tests/r_vba_compare/VBA_Mixture2.xls', sheet = 'VBAoutput', skip = 0, n_max = 338) %>%
  rename( date = `Year & month`) %>%
  dplyr::mutate( date = lubridate::make_date( year = 2000 + Year, month = Month, day = 1),
                 date = lubridate::ceiling_date( date, 'month') - 1,
                 across( c(everything(), -date), as.numeric)) %>%
  dplyr::select(-Year, -Month, -`Stand age`) %>%
  tidyr::gather( variable_vba, value, -date) %>%
  filter( !is.na( value) ) %>%
  # get species id
  mutate( species = stringr::str_sub(variable_vba, -1, -1)) %>%
  rowwise() %>%
  mutate(
    species = as.numeric(species),
    variable_vba =  if_else( is.na( species ), variable_vba, stringr::str_sub(variable_vba, 1, nchar(variable_vba)-1)),
    variable = var_names_vba[variable_vba],
    species = if_else( is.na( species ), 1, species ),
    species = c("Castanopsis sclerophylla","Cunninghamia lanceolata" )[species],
    model = 'VBA') %>%
  ungroup() %>%
  filter(!is.na(variable)) %>%
  select(date, species, variable, value, model)



# 3. Explore the final results --------------------------------------------

data.df <- bind_rows( r3PG_out.df, VBA_out.df)


i_var <- c('stems_n', 'biom_stem', 'biom_root', 'biom_foliage')
i_lab <- c('Stem density', 'Stem biomass', 'Root biomass', 'Foliage biomass')

data.df %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = model, linetype = species), size = 0.5)+
  facet_grid( variable ~ ., scales = 'free') +
  theme_classic()



