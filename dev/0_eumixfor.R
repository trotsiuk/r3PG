
# 0. Libraries ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(scales)

library(r3PG)

source('dev/functions.R')

# 1. Run the simulations --------------------------------------------------
vba.df <- tranf_vba(sk = 1419, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns13.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

vba.df <- tranf_vba(sk = 5, n_m = 147, f = '../3PG_examples/3PGmix/ExampleMixtureRuns16.xls', s = 'Shitaioutput' ) %>%
  mutate(obs = 'vba')

thin_eum <- data.frame(species = c('Fagus sylvatica', 'Picea abies', 'Picea abies'), age = c(48, 47, 50), n_trees = c(500, 600, 400), foliage = 1, root = 1, stem = 1)


# parameters_eum$sp1[11] <- 5
r.df <- run_3PG(site_eum, species_eum, climate_eum, thin_eum, parameters_eum, bias_eum,
  list(light_model = 2, transp_model = 2, phys_model = 2, correct_bias = 1)) %>%
  # transf_out( ) %>%
  # as_tibble() %>%
  mutate(obs = 'r') %>%
  mutate(species = if_else(species %in% 'Fagus sylvatica', 'sp_1', 'sp_2'))
  # mutate(species = gsub('sp', 'sp_', species))

data.df <- bind_rows(vba.df, r.df) %>%
  mutate(species = factor(species, labels = c('Fagus', 'Pinus')),
    obs = factor(obs, levels = c('r', 'vba')))

# 2. Explore the output ---------------------------------------------------
unique(r.df$group)

g_sel <- unique(r.df$group)
g_sel <- c("climate","stand","canopy","stocks","modifiers","production" ,"mortality","water_use" )
g_sel <- 'modifiers'
v_sel <- c('f_phys', 'crown_width', 'crown_length')
v_sel <- c('biom_stem', 'biom_foliage', 'biom_root', 'stems_n')
v_sel <- c('epsilon_npp', 'epsilon_biom_stem', 'epsilon_gpp', 'volume_cum', 'gpp', 'par', 'alpha_c')

data.df %>%
  # filter(variable %in% 'day_length') %>%
  # filter(year(date) %in% c(2002:2003))  %>%
  # filter(group %in% g_sel) %>%
  filter(variable %in% v_sel) %>%
  ggplot()+
  geom_line( aes(date, value, color = obs, linetype = species))+
  facet_wrap( ~ variable, scales = 'free_y') +
  scale_color_discrete(drop=FALSE) +
  theme_classic() +
  ggtitle('EuMixtfor bias correction')



options(digits=10)

data.df %>%
  filter(year(date) %in% c(2010:2010))  %>%
  filter(variable %in% 'epsilon_gpp') %>%
  spread(obs, value) %>%
  as.data.frame() %>%
  head(20)


