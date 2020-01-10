
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 688, n_m = 123, f = '../3PG_examples/3PGmix/ExampleMixtureRuns7.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

r.df <- run_3PG(site_shi23, species_shi23, climate_shi23, parameters_shi23[,-1], bias_shi23[,-1], list(f_dbh_dist = 0L)) %>%
  transf_out() %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(species = factor(species, labels = c('Castanopsis', 'Cunninghamia')),
    obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)


g_sel <- unique(r.df$group)
g_sel <- c("climate","stand","canopy","stocks","modifiers","production" ,"mortality","water_use" )
g_sel <- 'stand'
v_sel <- c('volume_mai')

data.df %>%
  filter(variable %in% 'canopy_cover') %>%
  # filter(year(date) %in% c(2002:2002))  %>%
  # filter(group %in% g_sel) %>%
  # filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = species))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Shitai23 no Bias correction')


data.df %>%
  filter(variable %in% 'height') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head()
