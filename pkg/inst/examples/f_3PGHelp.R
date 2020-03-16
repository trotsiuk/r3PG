f_site = as.matrix( site_eum )
f_species = as.matrix( species_eum[,-1] )
f_clim = as.matrix( climate_eum )
f_param = as.matrix( parameters_eum[,-1] )
f_bias = as.matrix( bias_eum[,-1] )

n_sp = nrow( f_species )
n_m = nrow( f_clim )
t_t = 0L
n_man = 1L
f_management = array(NA_real_, dim = c(1,5,n_sp))


out <- f_3PG(
  siteInputs = f_site,
  speciesInputs = f_species,
  forcingInputs = f_clim,
  managementInputs = f_management,
  parameterInputs = f_param,
  biasInputs = f_bias,
  n_sp = n_sp,
  n_m = n_m,
  n_man = n_man,
  t_t = t_t,
  settings = c(2L,2L,2L,0L,0L),
  sp_names = species_eum$species)


out_long <- transf_out( out )
