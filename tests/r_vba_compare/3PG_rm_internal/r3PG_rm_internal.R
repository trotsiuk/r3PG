library(r3PG)
library(ggplot2)
library(dplyr)


# Run the model -----------------------------------------------------------

out.df <- run_3PG(
  site = d_site_r,
  species = d_species_r,
  climate = d_climate_r,
  thinning = d_thinning_r,
  parameters = d_parameters_r,
  size_dist = d_sizeDist_r,
  settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                  height_model = 1, correct_bias = 0, calculate_d13c = 0,
                  mort_model = 2),
  check_input = TRUE, df_out = TRUE
)



# Visualise the data ------------------------------------------------------

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

ggsave('tests/r_vba_compare/3PG_rm_internal/r3pg_rm_biomas.png', width = 15, height = 8, units = c("in"), dpi = 'retina', bg = "transparent")




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
