
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 1419, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns11.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

parameters_eum$sp1[11] <- 5
r.df <- run_3PG(site_eum, species_eum, climate_eum, parameters_eum[,-1], bias_eum[,-1],
  list(light_model = 1L, phys_model = 1L, correct_bias = 1L)) %>%
  transf_out(day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(species = factor(species, labels = c('Fagus', 'Pinus')),
    obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)

g_sel <- unique(r.df$group)
g_sel <- c("climate","stand","canopy","stocks","modifiers","production" ,"mortality","water_use" )
g_sel <- 'stand'
v_sel <- c('f_phys', 'crown_width', 'crown_length')
v_sel <- c('biom_stem', 'biom_foliage', 'biom_root')
v_sel <- c('biom_tree', 'biom_root', 'biom_foliage', 'volume', 'volume_mai', 'stems_n')

data.df %>%
  # filter(variable %in% 'lai_above') %>%
  # filter(year(date) %in% c(2002:2003))  %>%
  filter(group %in% g_sel) %>%
  # filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = species))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('EuMixtfor bias correction')



options(digits=10)

data.df %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'f_age') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head(10)
