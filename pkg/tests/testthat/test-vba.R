library(r3PG)
library(testthat)

context("Unit test to check if the output of r3PG is identical to 3-PG VBA")

test_that("Evergreen 3-PGpjs check", {
  out <- run_3PG(
    site = d_site,
    species = d_species[2,],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[,c(1,3)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
      height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE)

  testthat::expect_true( all( round( out[120,,4,1:3], 3) == c(125.401, 39.128, 3.843) ) )
})

test_that("Evergreen 3-PGmix check", {
  out <- run_3PG(
    site = d_site,
    species = d_species[2,],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[,c(1,3)],
    size_dist = d_sizeDist[,c(1,3)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
      height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE)

  testthat::expect_true( all( floor( out[120,,4,1:3]) == c(127, 41, 3) ) )
})


test_that("Broadleaf 3-PGpjs check", {
  out <- run_3PG(
    site = d_site,
    species = d_species[1,],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[,c(1,2)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
      height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE)

  testthat::expect_true( all( round( out[120,,4,1:3], 3) == c(129.985, 31.348, 0.000) ) )
})

test_that("Broadleaf 3-PGmix check", {
  out <- run_3PG(
    site = d_site,
    species = d_species[1,],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[,c(1,2)],
    size_dist = d_sizeDist[,c(1,2)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
      height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE)

  testthat::expect_true( all( round( out[120,,4,1:3], 3) == c(132.313, 32.994, 0.000) ) )
})


test_that("Mixtures 3-PGmix check", {
  out <- run_3PG(
    site = d_site,
    species = d_species,
    climate = d_climate,
    thinning = d_thinning,
    parameters = d_parameters,
    size_dist = d_sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
      height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE)

  testthat::expect_true( all( round( out[120,1,4,1:3], 3) == c(85.934, 19.359, 0.000) ) )

  testthat::expect_true( all( round( out[120,2,4,1:3], 3) == c(59.909, 17.670, 1.741) ) )
})