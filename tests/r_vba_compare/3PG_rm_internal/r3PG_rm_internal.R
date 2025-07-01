library(r3PG)
library(ggplot2)
library(dplyr)


# Run the model -----------------------------------------------------------

out.df <- run_3PG(
  site = d_regeneration$site,
  species = d_regeneration$species,
  climate = d_regeneration$climate,
  thinning = d_regeneration$thinning,
  parameters = d_regeneration$parameters,
  size_dist = d_regeneration$sizeDist,
  settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                  height_model = 1, correct_bias = 0, calculate_d13c = 0,
                  mort_model = 2),
  check_input = TRUE, df_out = TRUE
)



# Visualise the data

# i_var <- c('stems_n', 'biom_stem', 'biom_root', 'biom_foliage',)
i_var <- c('stems_n', 'stems_loss_density', 'biom_loss_stem_density',
           'mort_thinn_total', 'dbh_total','stems_n_total')


out.df %>%
  dplyr::filter(variable %in% i_var) %>%
  # dplyr::mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = species), linewidth = 0.5)+
  facet_wrap( ~variable, scales = 'free_y') +
  theme_classic()+
  theme(legend.position = 'bottom')

# ggsave('tests/r_vba_compare/3PG_rm_internal/r3pg_rm_biomas.png', width = 15, height = 8, units = c("in"), dpi = 'retina', bg = "transparent")



# Mortality basead on the biomass -----------------------------------------
out.df <- run_3PG(
  site = d_regeneration$site,
  species = d_regeneration$species,
  climate = d_regeneration$climate,
  thinning = d_regeneration$thinning,
  parameters = d_regeneration$parameters,
  size_dist = d_regeneration$sizeDist,
  settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                  height_model = 1, correct_bias = 0, calculate_d13c = 0,
                  mort_model = 2, manag_model = 2),
  check_input = TRUE, df_out = TRUE
)




# Explore the results
out.df %>%
  dplyr::filter( date >= as.Date('1939-11-01'),
                 variable %in% c('stems_n_total', 'stems_n', 'stems_loss_density', 'mort_thinn_total', 'age')) %>%
  tidyr::pivot_wider(names_from = 'species', values_from = 'value') %>%
  dplyr::arrange( date, variable) %>%
  dplyr::select(-group) %>%
  dplyr::distinct() %>%
  head(12)


out_raw.df[479:482, 1:5, 2, 2] #stems_n

out_raw.df[479:482, 1:6, 8, 3] #stems_loss_density
out_raw.df[479:482, 1:6, 8, 5] #mort_thinn_total

out_raw.df[479:482, 1:6, 8, 9] #dbh_total
out_raw.df[479:482, 1:6, 8, 10] #stems_n_total
out_raw.df[479:482, 1:6, 8, 15] #stems_n before density
out_raw.df[479:482, 1:6, 11, 1]  # stems_loss_manag
out_raw.df[479:482, 1:6, 11, 2]  # stems_loss_manag

out_raw.df[479:482, 1:6, 4, 1] # biom_stem
out_raw.df[479:482, 1:6, 4, 4] # biom_tree

out_raw.df[479:482, 1:6, 3, 3] # lai

out_raw.df[479:482, 1:6, 2, 1] # age



60.9 * 3 + 107 + 198
# Mortality module on test data -------------------------------------------

out_3PG <- run_3PG(
  site        = d_site,
  species     = dplyr::mutate(d_species, lt_fN = 0.7,  lt_fT = 0.7, lt_fPhys = 0.7),
  climate     = d_climate,
  thinning    = d_thinning,
  parameters  = dplyr::mutate(d_parameters, beta0 = -15.9, betaB = 1.05, betaN = 2.18, betafN = -1.35, betafT = -1.58, betafPhys = -2.15 ),
  size_dist   = d_sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0,
                     mort_model = 2, manag_model = 1),
  check_input = TRUE, df_out = TRUE)



i_var <- c('stems_n',  'biom_stem', 'biom_root', 'biom_foliage',
           'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag', 'biom_loss_root_manag',
           'stems_loss_stress', 'biom_loss_stem_stress', 'biom_loss_foliage_stress', 'biom_loss_root_stress',
           'stems_loss_density', 'biom_loss_stem_density', 'biom_loss_foliage_density', 'biom_loss_root_density')
# i_lab <- c('Stem density', 'DBH', 'Height', 'Stem biomass', 'Root biomass', 'Foliage biomass',
#            'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag',
#            'stems_loss_stress', 'biom_loss_stem_stress', 'biom_loss_foliage_stress',
#            'stems_loss_density', 'biom_loss_stem_density', 'biom_loss_foliage_density')


out_3PG %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = species), size = 0.5)+
  facet_wrap( ~ variable, scales = 'free_y', ncol = 4,
              labeller = labeller(variable = setNames(i_var, i_var) )) +
  scale_color_brewer('', palette = 'Dark2') +
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("Calendar date") + ylab('Value')





# Some temp tests ---------------------------------------------------------

out.df %>%
  dplyr::filter(variable %in% c('ave_lt_fN', 'var_8_15')) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = variable), linewidth = 0.5)+
  theme_classic()+
  theme(legend.position = 'bottom')


out.df %>%
  dplyr::filter(variable %in% c('ave_lt_fN', 'var_8_15')) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::mutate( val_dif = ave_lt_fN - var_8_15) %>%
  head(100) %>%
  ggplot( aes(date, val_dif))+
  geom_line()
