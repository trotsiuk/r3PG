
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 966, n_m = 123, f = '../3PG_examples/3PGmix/ExampleMixtureRuns7.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

r.df <- run_3PG(site_eum, species_eum %>% filter(species == 2), climate_eum, parameters_eum[,'sp2'], bias_eum[,'sp2'], list(f_dbh_dist = 1L)) %>%
  transf_out(.,  day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)

g_sel <- unique(r.df$group)
g_sel <- c('stand', 'canopy', 'stocks', 'production')
g_sel <- c('weibull')
v_sel <- c('biom_tree')

v_sel <- c('biom_stem', 'biom_foliage', 'biom_root')

data.df %>%
  # filter(variable %in% c('wood_density', 'basal_area')) %>%
  # filter(year(date) %in% c(2002:2002))  %>%
  # filter(group %in% g_sel) %>%
  filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = obs))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Pinus Bias correction')


options(digits=10)

data.df %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'biom_root') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head()




data.df %>%
  filter(variable %in% 'Dweibulllocation') %>%
  spread(obs, value) %>%
  head()
