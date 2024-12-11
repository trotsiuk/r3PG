library(r3PG)
library(testthat)

# Basic model runs
test_that("Basic model run returns an array", {
  result <- run_3PG(
    site = d_site,
    species = d_species,
    climate = d_climate,
    thinning = d_thinning,
    parameters = d_parameters,
    size_dist = d_sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    correct_bias = 1, calculate_d13c = 0, mort_model = 1),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal( class(result), "array")
})

test_that("run_3PG returns a data frame with the correct structure", {
  result <- run_3PG(
    site = d_site,
    species = d_species,
    climate = d_climate,
    thinning = d_thinning,
    parameters = d_parameters,
    size_dist = d_sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    correct_bias = 1, calculate_d13c = 0, mort_model = 1),
    check_input = TRUE, df_out = TRUE
  )
  expect_equal( class(result), "data.frame")
  expect_true(all(c("date", "species", "group", "variable", "value") %in% colnames(result)))
})

# Evergreen model checks
test_that("Evergreen 3-PGpjs produces expected output", {
  out <- run_3PG(
    site = d_site,
    species = d_species[2, ],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[, c(1, 3)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, , 4, 1:3], 3), c(125.423, 39.146, 3.845))
})

test_that("Evergreen 3-PGmix produces expected output", {
  out <- run_3PG(
    site = d_site,
    species = d_species[2, ],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[, c(1, 3)],
    size_dist = d_sizeDist[, c(1, 3)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(floor(out[120, , 4, 1:3]), c(127, 41, 3))
})

# Broadleaf model checks
test_that("Broadleaf 3-PGpjs produces expected output", {
  out <- run_3PG(
    site = d_site,
    species = d_species[1, ],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[, c(1, 2)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, , 4, 1:3], 3), c(129.988, 31.350, 0.000))
})

test_that("Broadleaf 3-PGmix produces expected output", {
  out <- run_3PG(
    site = d_site,
    species = d_species[1, ],
    climate = d_climate,
    thinning = NULL,
    parameters = d_parameters[, c(1, 2)],
    size_dist = d_sizeDist[, c(1, 2)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, , 4, 1:3], 3), c(132.316, 32.996, 0.000))
})

# Mixed-species model check
test_that("Mixed-species 3-PGmix produces expected output", {
  out <- run_3PG(
    site = d_site,
    species = d_species,
    climate = d_climate,
    thinning = d_thinning,
    parameters = d_parameters,
    size_dist = d_sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, 1, 4, 1:3], 3), c(85.939, 19.359, 0.000))
  expect_equal(round(out[120, 2, 4, 1:3], 3), c(59.917, 17.677, 1.742))
})


# Regeneration
test_that("Development test for mortality model", {

  result <- run_3PG(
    site = d_site_r,
    species = d_species_r,
    climate = d_climate_r,
    thinning = d_thinning_r,
    parameters = d_parameters_r,
    size_dist = d_sizeDist_r,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 2),
    check_input = TRUE, df_out = FALSE
  )


  expect_equal(round(result[120, 1, 4, 1:3], 3), c(73.814, 31.426, 4.455))
  expect_equal(round(result[120, 2, 4, 1:3], 3), c(46.255, 14.359,  1.977))

  expect_equal(round(result[5000, 20:24, 4, 1], 3), c(131.198,  61.939,  78.341,  17.080,  11.148))
  expect_equal(round(result[5000, 20:24, 4, 2], 3), c(37.647, 25.722, 34.888,  9.554,  9.856))

  expect_equal(round(result[1000, 4, 8, 5], 3), c(0.694))
  expect_equal(round(result[5000, 20, 8, 5], 3), c(2.409))

})
