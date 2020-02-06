
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 410, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns11.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

parameters_eum$sp1[11] <- 5
r.df <- run_3PG(site_eum, species_eum %>% filter(species == 1), climate_eum, parameters_eum[,'sp1'], bias_eum[,'sp1'],
  list(light_model = 2L, phys_model = 2L, correct_bias = 0L)) %>%
  transf_out(.,  day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
v_sel <- c('biom_stem', 'biom_foliage', 'biom_root')

data.df %>%
  # filter(variable %in% c('canopy_cover') ) %>%
  # filter(year(date) %in% c(2002:2002))  %>%
  # filter(group %in% g_sel) %>%
  filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = obs))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Fagus pjs')

