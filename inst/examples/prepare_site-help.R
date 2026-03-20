# Example: Valid site data
# r3PG default data
prepare_site(site = d_mixture$site)

# example data
d_site <- data.frame(
  latitude = 46.8,
  elevation = 1200,
  soil_class = 2,
  asw_i = 150,
  asw_min = 100,
  asw_max = 200,
  from = "2000-01",
  to = "2009-12"
)

# Validate and prepare site data
prepared_site <- prepare_site(d_site)

# View the prepared site data
print(prepared_site)