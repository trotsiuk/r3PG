# sub-setting climate data
prepare_climate( climate = d_climate, from = '2003-04', to = '2010-11')

# replicating climate data
set.seed(1)
climate = data.frame( tmp_min = rnorm(12, mean = 10),
                      tmp_max = rnorm(12, mean = 20),
                      prcp = sample(c(0:200), 12),
                      srad = sample(c(1:100), 12),
                      frost_days = sample(c(0:30), 12))

prepare_climate( climate = climate, from = '2000-04', to = '2010-11')

