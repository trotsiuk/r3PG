climate = matrix(rnorm(60), ncol = 5)
colnames(climate) = c("tmp_min", "tmp_max", "prcp", "srad", "frost_days")

gen_climate( climate, 3, 2)