
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 1117, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns7.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

parameters_eum$sp1[11] <- 5
r.df <- run_3PG(site_eum, species_eum %>% filter(species == 1), climate_eum, parameters_eum[,'sp1'], bias_eum[,'sp1'], list(f_dbh_dist = 0L)) %>%
  transf_out(.,  day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)

g_sel <- unique(r.df$group)
g_sel <- c('stand', 'canopy', 'stocks', 'production', 'modifiers')
g_sel <- 'wood_delta'
v_sel <- c('aero_resist', 'asw', 'f_sw', 'f_phys', 'alpha_c', 'epsilon_gpp', 'par', 'gpp', 'transp_veg', 'evapotra_soil', 'conduct_canopy', 'lai')

data.df %>%
  # filter(variable %in% c('canopy_cover') ) %>%
  # filter(year(date) %in% c(2002:2002))  %>%
  filter(group %in% g_sel) %>%
  # filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = obs))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Fagus no Bias correction') +
  geom_vline(xintercept = as.numeric(as.Date('2002-02-28')), linetype = 2, colour = 'grey50')


options(digits=10)

data.df %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'Gc_mol') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head(10)

0.0129196256648872 * 24 * 2.3

r.df[1:5,,4,14]
r.df[1:5,,4,15]


parameters_eum[3:4,'sp2'] %>% as.data.frame()
