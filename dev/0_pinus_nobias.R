
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 1268, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns11.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

r.df <- run_3PG(
  site_eum, # iMonth indicate first month when simulation is done
  species_eum %>% filter(species == 2),
  climate_eum,
  parameters_eum[,'sp2'],
  bias_eum[,'sp2'],
  list(light_model = 1L, phys_model = 1L, correct_bias = 0L)) %>%
  transf_out(.,  day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)

g_sel <- unique(r.df$group)
g_sel <- c('stand', 'canopy', 'stocks', 'production')
g_sel <- 'wood_delta'
v_sel <- c('biom_tree', 'biom_root', 'biom_foliage', 'volume', 'volume_mai')

data.df %>%
  # filter(variable %in% 'lai_above') %>%
  # filter(year(date) %in% c(2002:2002))  %>%
  # filter(group %in% g_sel) %>%
  filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = obs))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Pinus no Bias correction')



options(digits=10)

data.df %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'alpha_c') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head(10)



r.df[1:5,,4,14]
r.df[1:5,,4,15]


parameters_eum[3:4,'sp2'] %>% as.data.frame()
