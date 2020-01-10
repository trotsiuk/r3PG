#' Trying to calculate Biass for the first month
#'
options(digits=10)

parameters_eum$sp1[11] <- 5
r.df <- run_3PG(site_eum, species_eum %>% filter(species == 1), climate_eum, parameters_eum[,'sp1'], bias_eum[,'sp1'], list(f_dbh_dist = 1L)) %>%
  transf_out(.,  day_start = as.Date('2002-01-31')) %>%
  as_tibble() %>%
  mutate(obs = 'r') %>%
  filter(date %in% min(date))

f_get_var <- function( v ){
  filter(r.df, variable %in% v) %>% pull(value)
}

f_get_par <- function( v ){
  filter(parameters_eum, parameter %in% v) %>% pull(sp1)
}

f_get_bias <- function(v){
  filter(bias_eum, parameter %in% v) %>% pull(sp1)
}

# variables
s_age <- f_get_var('age')
stems_n <- f_get_var('stems_n')
biom_stem <- f_get_var('biom_stem')
biom_foliage <- f_get_var('biom_foliage')
sla <- f_get_var('sla')
wood_density <- f_get_var('wood_density')

# parameters
aWs <- f_get_par('aWS')
nWs <- f_get_par('nWS')

# Biasses
aH  <- f_get_bias('aH')
nHB <- f_get_bias('nHB')
nHC <- f_get_bias('nHC')

Dlocation0  <- f_get_bias('Dlocation0')
DlocationB  <- f_get_bias('DlocationB')
Dlocationrh  <- f_get_bias('Dlocationrh')
Dlocationt  <- f_get_bias('Dlocationt')
DlocationC  <- f_get_bias('DlocationC')

Dshape0  <- f_get_bias('Dshape0')
DshapeB  <- f_get_bias('DshapeB')
Dshaperh  <- f_get_bias('Dshaperh')
Dshapet  <- f_get_bias('Dshapet')
DshapeC  <- f_get_bias('DshapeC')


# start the calculations
biom_tree = biom_stem * 1000 / stems_n
dbh = ( biom_tree / aWs ) ^ (1 / nWs )
basal_area = dbh ^ 2 / 4 * pi * stems_n / 10000
lai =  biom_foliage * sla * 0.1
competition_total = sum( wood_density*basal_area)
height = aH * dbh^nHB * competition_total^nHC


#' Start the bias calcualtion
height_rel = height / ( sum( height * stems_n ) / sum( stems_n ) )


DWeibullShape = exp( Dshape0 + DshapeB*log( dbh ) + Dshaperh*log(height_rel) + Dshapet*log(s_age) + DshapeC*log(competition_total))



out <- run_3PG(site_eum, species_eum %>% filter(species == 1), climate_eum, parameters_eum[,'sp1'], bias_eum[,'sp1'], list(f_dbh_dist = 1L))

out[1,,10,2]
