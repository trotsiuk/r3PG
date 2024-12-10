# Example: Climate data preparation
# r3PG default data
prepare_climate( climate = d_climate, from = '2003-04', to = '2010-11')

d_climate <- data.frame(
  tmp_min = runif(12, -5, 5),
  tmp_max = runif(12, 10, 20),
  prcp = runif(12, 50, 150),
  srad = runif(12, 15, 25),
  frost_days = runif(12, 0, 10)
)

# Prepare climate for the period 2000-01 to 2005-12
prepared_climate <- prepare_climate(d_climate, from = "2000-01", to = "2005-12")
print(head(prepared_climate))
