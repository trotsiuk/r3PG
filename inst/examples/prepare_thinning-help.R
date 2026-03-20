# Example: Preparing thinning data
# r3PG default data
prepare_thinning( thinning = NULL, sp_names = c('Quercus', 'Abies'))

prepare_thinning(thinning = d_mixture$thinning, sp_names = c('Fagus sylvatica', 'Pinus sylvestris'))

# example data
thinning_data <- data.frame(
  species = c("Fagus sylvatica", "Pinus sylvestris"),
  age = c(10, 15),
  stems_n = c(500, 400),
  stem = c(1, 1),
  root = c(1, 1),
  foliage = c(1, 1)
)

species_names <- c("Fagus sylvatica", "Pinus sylvestris")

# Prepare thinning
prepared_thinning <- prepare_thinning(thinning = thinning_data, sp_names = species_names)
print(prepared_thinning)
