
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 815, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns8.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

parameters_eum$sp1[11] <- 5
r.df <- run_3PG(site_eum, species_eum %>% filter(species == 1), climate_eum, parameters_eum[,'sp1'], bias_eum[,'sp1'], list(f_dbh_dist = 1L)) %>%
  transf_out(.,  day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r')

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)

g_sel <- unique(r.df$group)
g_sel <- c('stand', 'canopy', 'stocks', 'production')
g_sel <- 'weibull'
v_sel <- c('asw', 'f_sw', 'f_phys', 'alpha_c', 'epsilon_gpp', 'par', 'gpp', 'transp_veg', 'evapotra_soil', 'conduct_canopy', 'lai')

v_sel <- c('transp_veg', 'evapotra_soil', 'conduct_canopy', 'lai', 'conduct_soil','fi')
v_sel <- c('biom_stem', 'biom_foliage', 'biom_root')

data.df %>%
  # filter(variable %in% 'vpd_day') %>%
  # filter(year(date) %in% c(2002:2002))  %>%
  # filter(group %in% g_sel) %>%
  filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = obs))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Fagus Bias correction')
  # geom_vline(xintercept = as.numeric(as.Date('2002-02-28')), linetype = 2, colour = 'grey50')


options(digits=10)

data.df %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'biom_foliage') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head(10)

0.0129196256648872 * 24 * 2.3

r.df[1:5,,4,14]
r.df[1:5,,4,15]


parameters_eum[3:4,'sp2'] %>% as.data.frame()
