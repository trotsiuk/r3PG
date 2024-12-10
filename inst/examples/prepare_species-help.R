# Example: Valid species data
# r3PG default data
prepare_species( species = d_species)

# example data
d_species <- data.frame(
  species = c("Pine", "Oak"),
  planted = c("2000-01", "1995-06"),
  fertility = c(0.8, 0.6),
  stems_n = c(500, 300),
  biom_stem = c(120, 90),
  biom_root = c(30, 25),
  biom_foliage = c(15, 10)
)

# Check species data
prepared_species <- prepare_species(species = d_species)

# Print the prepared species data
print(prepared_species)
