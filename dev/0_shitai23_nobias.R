
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PGmix)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 688, n_m = 123, f = '../3PG_examples/3PGmix/ExampleMixtureRuns9.xls', s = 'Shitaioutput' ) %>%
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
g_sel <- c('stand', "stocks")
v_sel <- c('f_sw', 'asw', 'evapo_transp', 'transp_veg', 'height', 'fracBB')

data.df %>%
  # filter(variable %in% 'canopy_cover') %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(group %in% g_sel) %>%
  # filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = species))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('Shitai23 no Bias correction')



options(digits=10)

data.df %>%
  # filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'gammaF') %>%
  # filter(date %in% as.Date('2010-02-28')) %>%
  spread(obs, value) %>%
  # arrange(group, variable, date, species) %>%
  as.data.frame() %>%
  # readr::write_csv('dev/shitai23_noBias.csv')
  head(20)
