library(r3PG)
library(ggplot2)
library(readxl)
library(dplyr)


Path1 <- "tests/r_vba_compare/tmp_test/"
# f_loc <- paste(Path1,"test_add_cohort_between_start_and_endtimes_2cohorts.xlsx", sep="")
f_loc <- paste(Path1,"test_add_cohort_between_start_and_endtimes_3cohorts.xlsx", sep="")

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
                                   height_model = 2,     # '1' - linear (default); '2' - non-linear
                                   correct_bias = 0,       # '0' - no (default); '1' - yes
                                   calculate_d13c = 0,   # '0' - no (default); '1' - yes
                                   mort_model = 2),      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                   check_input = TRUE, df_out = TRUE)

sel_var <- c('stems_n', 'biom_tree', 'biom_stem', 'dbh', 'basal_area', 'height') # sort(unique(out_3PG$variable))

out_3PG %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  theme_classic()




out_3PG %>%
  filter(variable %in% 'biom_foliage') %>%
  tidyr::pivot_wider( names_from = 'species', values_from = 'value') %>%
  dplyr::filter( date > as.Date('2001-01-01'))



out_3PG %>%
  dplyr::filter( date %in% as.Date('2001-06-30'))



wood_density = c(0.37, 0.37, 0.37)
basal_area = c(96.76254721, 47.93019208, 15.36179815)

competition_total = sum( wood_density * basal_area )
59.22018

crown_length(:) = 1.3d0 + aHL(:) * exp(1.d0)**(-nHLB(:)/dbh(:)) + nHLC(:) * competition_total(:) * dbh(:)
crown_length = 1.3 + 24.93 * exp(1)^(-25.090/197.13973) + -0.002 * 59.22018 * 197.13973
-0.0985406




ast <- out_3PG %>%
  dplyr::filter( date %in% c(as.Date('2001-06-30'), as.Date('2001-07-31'), as.Date('2001-08-31'))) %>%
  tidyr::pivot_wider( names_from = 'date', values_from = 'value')

out_3PG %>%
  dplyr::filter( variable %in% 'crown_length', value <=0)




# This run ends prematurely when a thinning event occurs that removes all trees for 1 cohort (2 cohorts should remain)
f_loc <- paste(Path1,"test_remove_cohort_before_endtime.xlsx", sep="")


# This run has unexpected output when the "planted" date is after the "from" start date.
# However, if this is changed so the planted date is before the start date (e.g. cell B4 changed from 1970-01 to 1870-01), the
# run ends unexpectedly, presumably because one species self thins to about 0 stems_n.
f_loc <- paste(Path1,"test_add_cohort_between_start_and_endtimes.xlsx", sep="")




system.time({
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
                                     height_model = 2,     # '1' - linear (default); '2' - non-linear
                                     correct_bias=0,       # '0' - no (default); '1' - yes
                                     calculate_d13c = 0,   # '0' - no (default); '1' - yes
                                     mort_model = 2),      # '1' - 3-PGpjs (default); '2' - 3-PGmix
                     check_input = TRUE, df_out = TRUE)
})





sel_var <- c('stems_n', 'biom_tree', 'biom_stem', 'dbh', 'basal_area', 'height') # sort(unique(out_3PG$variable))
#sel_var <- c('test_output')


out_3PG %>%
  filter( variable %in% sel_var ) %>%
  ggplot( aes(date, value, color = species) ) +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_classic()


ast <- out_3PG %>%
  filter(variable %in% 'stems_n') %>%
  tidyr::pivot_wider( names_from = 'species', values_from = 'value')

out_3PG %>%
  filter(variable %in% 'age') %>%
  tidyr::pivot_wider( names_from = 'species', values_from = 'value')

# Check some variables
out_3PG[,,2, c(5)] #dbh
out_3PG[,,4, c(4)] #biom_tree,

