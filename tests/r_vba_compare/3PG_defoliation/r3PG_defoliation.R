# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(r3PG)
library(ggplot2)



# Management based on number of trees -------------------------------------
out_3PG <- run_3PG(
  site = d_defoliation$site,
  species = d_defoliation$species,
  climate = d_defoliation$climate,
  thinning = d_defoliation$thinning,
  defoliation = d_defoliation$defoliation,
  parameters = d_defoliation$parameters,
  size_dist = d_defoliation$sizeDist,
  settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                  height_model = 1, correct_bias = 0, calculate_d13c = 0,
                  mort_model = 2, manag_model = 2),
  check_input = TRUE, df_out = TRUE
)



ast <- out_3PG %>%
  dplyr::filter( variable %in% c('age', 'var_11_17', 'var_11_18', 'biom_loss_stem_def')) %>%
  dplyr::select(-group) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value)


i_var <- c('stems_n', 'dbh', 'height', 'basal_area',
           'biom_stem', 'biom_root', 'biom_foliage', 'volume_mai',
           'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag', 'biom_loss_root_manag',
           'stems_loss_stress', 'biom_loss_stem_stress', 'biom_loss_foliage_stress', 'biom_loss_root_stress',
           'stems_loss_density', 'biom_loss_stem_density', 'biom_loss_foliage_density', 'biom_loss_root_density',
           'stems_loss_def', 'biom_loss_stem_def', 'biom_loss_foliage_def', 'biom_loss_root_def')
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




prepare_defoliation(d_defoliation$defoliation, sp_names = 'Pinus sylvestris_1')
