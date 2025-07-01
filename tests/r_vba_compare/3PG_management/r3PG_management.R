# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(r3PG)
library(ggplot2)



# Management based on number of trees -------------------------------------
out_3PG <- run_3PG(
  site        = d_input$site,
  species     = d_input$species,
  climate     = d_input$climate,
  thinning    = d_input$thinning,
  parameters  = d_input$parameters,
  size_dist   = d_input$sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0,
                     mort_model = 1, manag_model = 1),
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

# Management based on Biomass -------------------------------------

out_3PG <- run_3PG(
  site        = d_input$site,
  species     = d_input$species,
  climate     = d_input$climate,
  thinning    = dplyr::mutate(d_input$thinning, stems_n = 0.5),
  parameters  = d_input$parameters,
  size_dist   = d_input$sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0,
                     mort_model = 1, manag_model = 2),
  check_input = TRUE, df_out = TRUE)



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
