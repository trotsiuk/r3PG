out <- run_3PG(
  site = d_mixture$site,
  species = d_mixture$species,
  climate = d_mixture$climate,
  thinning = d_mixture$thinning,
  parameters = d_mixture$parameters,
  size_dist = d_mixture$sizeDist,
  settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                  correct_bias = 1, calculate_d13c = 0, mort_model = 1),
  check_input = TRUE, df_out = TRUE) # note that default is TRUE

str(out) # List output format
