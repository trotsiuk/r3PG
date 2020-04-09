climate = matrix(rnorm(60), ncol = 5)
colnames(climate) = c("tmp_min", "tmp_max", "prcp", "srad", "frost_days")

sub_climate( climate, from = '2000-04', to = '2010-11')
