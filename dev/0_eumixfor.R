
# 0. Libraries ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)

library(r3PGmix)

library(ggplot2)

library(scales)

# 0. Custom functions -----------------------------------------------------
get_date <- function( day_start = as.Date('2002-01-31'), n = 10 ){
  #' @description function generate a sequence of dates with step 1 month

  seq( ceiling_date( day_start, "month"), by = "month", length.out = n) - 1
}

tranf_rout <- function( out ){

  out_mat <- c()
  n_sp = dim(out)[2]
  n_group = dim(out)[3]
  n_var = dim(out)[4]

  for( g in 1:n_group){ # for each group
    for( v in 1:n_var){ # for each variable
      x <- out[,,g,v] %>% as.data.frame() %>% purrr::set_names(paste( r.names[(g-1)*n_var + v], 1:n_sp, sep = '-'))

      out_mat <- bind_cols(out_mat, x)
    }
  }

  return(out_mat)
}

# 0. Data -----------------------------------------------------------------
var_names.df <- readxl::read_excel('dev/r3PGmix_info.xlsx', sheet = 'var_names')

vba.names <- var_names.df %>% select( variable_vba, variable_name) %>% filter(!is.na(variable_vba)) %>% tibble::deframe()
r.names <- var_names.df %>% pull(variable_name)

#' `Prepare VBA dataset`
vba.df <- readxl::read_xls('../3PG_examples/3PGmix/ExampleMixtureRuns3_fixed_sign from Jan fixed deciduous calcs2.xls', sheet = 'Shitaioutput', skip = 259, n_max = 140) %>%
  select( -`Year & month` ) %>%
  mutate_all( funs( as.numeric) ) %>%
  # get date
  mutate( date = get_date( n = n() ) ) %>%
  gather( variable, value, -date) %>%
  filter( !is.na( value) ) %>%
  # get species id
  mutate( sp_number = stringr::str_sub(variable, -1, -1)) %>%
  # filter(!is.na(sp_number)) %>%
  # rename variables
  rowwise() %>%
  mutate( sp_number = as.numeric(sp_number),
    sp_number = if_else(is.na(sp_number), 1, sp_number),
    variable = gsub(sp_number, '', variable)) %>%
  filter(variable %in% names(vba.names)) %>%
  mutate( variable = vba.names[[variable]] ) %>%
  ungroup() %>%
  mutate( obs = 'vba') %>%
  select( date, sp_number, variable, value, obs)

parameters_eum$sp1[11] <- 5
# parameters_eum$sp1[12] <- 11


#' `Prepare Fortran dataset`
r.df <- run_3PG(
  siteInputs = site_eum,
  speciesInputs = species_eum,
  forcingInputs = climate_eum,
  # parameterInputs = pt,
  parameterInputs = parameters_eum[,-1],
  biasInputs = bias_eum[,-1],
  settings = list(f_dbh_dist = 0L)) %>%
  tranf_rout()  %>%
  mutate( date = get_date( n = n() ) ) %>%
  gather( variable, value, -date) %>%
  filter( !value %in% -999 ) %>%
  # get species id
  separate( variable, c('variable', 'sp_number'), sep = '-' ) %>%
  mutate( obs = 'r',
    sp_number = as.numeric(sp_number)) %>%
  select( date, sp_number, variable, value, obs)



# 3. This part is used to understand descrepancies between R and V --------
sel_var <- c('biom_foliage', 'lai', 'lai_above', 'par','gpp', 'npp','transp','biom_stem', 'stems_n', 'lambda_h', 'lambda_v')

bind_rows(vba.df, r.df) %>%
  filter(!date %in% as.Date('2002-01-31')) %>%
  filter(year(date) %in% c(2002:2003))  %>%
  filter( variable %in% sel_var) %>%
  # mutate( variable = factor( variable, levels = sel_var)) %>%
  mutate(sp_number = factor(sp_number, labels = c('Fagus', 'Pinus'))) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = sp_number))+
  facet_wrap( ~ variable, scales = 'free_y') +
  theme_classic() +
  geom_vline(xintercept = as.numeric(as.Date('2002-05-31')), linetype = 2, colour = 'red')+
  geom_vline(xintercept = as.numeric(as.Date('2002-06-30')), linetype = 2, colour = 'grey50') +
  geom_vline(xintercept = as.numeric(as.Date('2002-04-30')), linetype = 2, colour = 'grey50')



#  Visualize everything ---------------------------------------------------


var_group <- var_names.df %>%
  filter(!substr(variable_name, 1, 3) %in% 'var') %>%
  select(variable_group, variable_name) %>%
  tibble::deframe()


bind_rows(vba.df, r.df) %>%
  filter(!date %in% as.Date('2002-05-31')) %>%
  # filter(variable %in% c(sel_var, 'asw')) %>%
  # filter(!variable %in% var_group[names(var_group) %in% c('climate', 'modifiers')]) %>%
  filter(year(date) < 2004) %>%
  filter( variable %in% pull(distinct(r.df, variable))) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = as.factor(sp_number)))+
  facet_wrap( ~ variable, scales = 'free_y') +
  theme_classic() +
  geom_vline(xintercept = as.numeric(as.Date('2003-05-31')), linetype = 2, colour = 'red')+
  geom_vline(xintercept = as.numeric(as.Date('2003-06-30')), linetype = 2, colour = 'grey50') +
  geom_vline(xintercept = as.numeric(as.Date('2003-04-30')), linetype = 2, colour = 'grey50')




bind_rows(vba.df, r.df) %>%
  filter(!date %in% as.Date('2002-05-31')) %>%
  filter(variable %in% c('biom_tree', 'biom_tree_max', 'mort_thinn', 'basal_area', 'stems_n', 'biom_stem')) %>%
  filter( variable %in% pull(distinct(r.df, variable))) %>%
  filter(year(date) < 2005) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = as.factor(sp_number)))+
  facet_wrap( ~ variable, scales = 'free_y') +
  # facet_wrap( ~ obs, scales = 'free') +
  theme_classic()
