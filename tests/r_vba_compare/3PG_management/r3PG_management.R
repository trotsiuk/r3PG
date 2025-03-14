# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(r3PG)
library(ggplot2)



# Management based on number of trees -------------------------------------
out_3PG <- run_3PG(
  site        = d_site,
  species     = d_species,
  climate     = d_climate,
  thinning    = d_thinning,
  parameters  = d_parameters,
  size_dist   = d_sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0,
                     mort_model = 1, manag_model = 1),
  check_input = TRUE, df_out = TRUE)



i_var <- c('stems_n',  'dbh', 'height', 'biom_stem', 'biom_root', 'biom_foliage', 'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag')
i_lab <- c('Stem density', 'DBH', 'Height', 'Stem biomass', 'Root biomass', 'Foliage biomass', 'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag')

out_3PG %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = species), size = 0.5)+
  facet_wrap( ~ variable, scales = 'free_y', ncol = 3,
              labeller = labeller(variable = setNames(i_lab, i_var) )) +
  scale_color_brewer('', palette = 'Dark2') +
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("Calendar date") + ylab('Value')

# Management based on Biomass -------------------------------------
d_thinning_biomass <- d_thinning
d_thinning_biomass$stems_n<- 0.5


out_3PG <- run_3PG(
  site        = d_site,
  species     = d_species,
  climate     = d_climate,
  thinning    = d_thinning_biomass,
  parameters  = d_parameters,
  size_dist   = d_sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0,
                     mort_model = 1, manag_model = 2),
  check_input = TRUE, df_out = TRUE)



i_var <- c('stems_n',  'dbh', 'height', 'biom_stem', 'biom_root', 'biom_foliage', 'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag')
i_lab <- c('Stem density', 'DBH', 'Height', 'Stem biomass', 'Root biomass', 'Foliage biomass', 'stems_loss_manag', 'biom_loss_stem_manag', 'biom_loss_foliage_manag')

out_3PG %>%
  filter(variable %in% i_var) %>%
  mutate(variable = factor(variable, levels = i_var)) %>%
  ggplot( aes(date, value))+
  geom_line( aes(color = species), size = 0.5)+
  facet_wrap( ~ variable, scales = 'free_y', ncol = 3,
              labeller = labeller(variable = setNames(i_lab, i_var) )) +
  scale_color_brewer('', palette = 'Dark2') +
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("Calendar date") + ylab('Value')
