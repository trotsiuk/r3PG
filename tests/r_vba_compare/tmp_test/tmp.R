library(r3PG)
library(ggplot2)
library(readxl)
library(dplyr)


Path1 <- "tests/r_vba_compare/tmp_test/"


f_loc <- paste(Path1,"test_Pinus_sylvestris.xlsx", sep="")
out_3PG <- run_3PG(site        = read_xlsx(f_loc, 'site'),
                   species     = read_xlsx(f_loc, 'species'),
                   climate     = prepare_climate(climate = read_xlsx(f_loc, 'climate'),
                                                 from = read_xlsx(f_loc, sheet = 'site')$from,
                                                 to = read_xlsx(f_loc, sheet = 'site')$to),
                   thinning    = read_xlsx(f_loc, 'thinning'),
                   parameters  = read_xlsx(f_loc, 'parameters'),
                   size_dist   = read_xlsx(f_loc, 'sizeDist'),
                   settings = list(light_model = 2,      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                   transp_model = 2,     # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                   phys_model = 2,       # '1' - 3-PGpjs (default); '2' - 3-PGmix
                                   height_model = 1,     # '1' - linear (default); '2' - non-linear
                                   correct_bias = 0,       # '0' - no (default); '1' - yes
                                   calculate_d13c = 0,   # '0' - no (default); '1' - yes
                                   mort_model = 2),      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                   check_input = TRUE, df_out = TRUE)

sel_var <- c('mort_thinn','mort_thinn_total','ave_lt_fN','ave_lt_fT','ave_lt_fPhys','lt_fN','lt_fT','lt_fPhys','dbh_total','stems_n_total')

out_3PG %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_classic()



out_3PG %>%
  filter( variable %in% 'lt_fN', date %in% '1900-01-31')
