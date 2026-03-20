library(r3PG)
library(testthat)

# Basic model runs
test_that("Basic model run returns an array", {

  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species,
    climate = d_mixture$climate,
    thinning = d_mixture$thinning,
    parameters = d_mixture$parameters,
    size_dist = d_mixture$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    correct_bias = 1, calculate_d13c = 0, mort_model = 1),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal( class(result), "array")
})

test_that("run_3PG returns a data frame with the correct structure", {
  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species,
    climate = d_mixture$climate,
    thinning = d_mixture$thinning,
    parameters = d_mixture$parameters,
    size_dist = d_mixture$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    correct_bias = 1, calculate_d13c = 0, mort_model = 1),
    check_input = TRUE, df_out = TRUE
  )
  expect_equal( class(result), "data.frame")
  expect_true(all(c("date", "species", "group", "variable", "value") %in% colnames(result)))
})

# Evergreen model checks
test_that("Evergreen 3-PGpjs produces expected output", {
  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species[2, ],
    climate = d_mixture$climate,
    thinning = NULL,
    parameters = d_mixture$parameters[, c(1, 3)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(result[120, , 4, 1:3], 3), c(125.423, 39.146, 3.845))
})

test_that("Evergreen 3-PGmix produces expected output", {
  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species[2, ],
    climate = d_mixture$climate,
    thinning = NULL,
    parameters = d_mixture$parameters[, c(1, 3)],
    size_dist = d_mixture$sizeDist[, c(1, 3)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(result[120, , 4, 1:3], 3), c(123.357, 37.610, 3.625))
})

# Broadleaf model checks
test_that("Broadleaf 3-PGpjs produces expected output", {
  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species[1, ],
    climate = d_mixture$climate,
    thinning = NULL,
    parameters = d_mixture$parameters[, c(1, 2)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(result[120, , 4, 1:3], 3), c(140.249, 38.082, 0.000))
})

test_that("Broadleaf 3-PGmix produces expected output", {
  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species[1, ],
    climate = d_mixture$climate,
    thinning = NULL,
    parameters = d_mixture$parameters[, c(1, 2)],
    size_dist = d_mixture$sizeDist[, c(1, 2)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(result[120, , 4, 1:3], 3), c(144.273, 40.554, 0.000))
})

# Mixed-species model check
test_that("Mixed-species 3-PGmix produces expected output", {
  result <- run_3PG(
    site = d_mixture$site,
    species = d_mixture$species,
    climate = d_mixture$climate,
    thinning = d_mixture$thinning,
    parameters = d_mixture$parameters,
    size_dist = d_mixture$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(result[120, 1, 4, 1:3], 3), c(95.157, 24.911, 0.000))
  expect_equal(round(result[120, 2, 4, 1:3], 3), c(57.192, 15.105,  1.415))
})


# Regeneration
test_that("Mortality model", {

  result <- run_3PG(
    site = d_regeneration$site,
    species = d_regeneration$species,
    climate = d_regeneration$climate,
    thinning = d_regeneration$thinning,
    parameters = d_regeneration$parameters,
    size_dist = d_regeneration$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 2),
    check_input = TRUE, df_out = FALSE
  )


  expect_equal(round(result[120, 1, 4, 1:3], 3), c(85.191, 32.973, 4.729))
  expect_equal(round(result[120, 2, 4, 1:3], 3), c(53.333, 13.914, 1.892))

  expect_equal(round(result[5000, 20:24, 4, 1], 3), c(102.407, 66.012, 45.368, 27.786, 9.498))
  expect_equal(round(result[5000, 20:24, 4, 2], 3), c(37.307, 24.127, 18.552, 14.909, 8.230))

  expect_equal(round(result[1000, 4, 8, 5], 3), c(0.738))
  expect_equal(round(result[5000, 20, 8, 5], 3), c(0.738))

})



test_that("Mixed-species management based on biomass", {
  result <- run_3PG(
    site = d_regeneration$site,
    species = d_regeneration$species,
    climate = d_regeneration$climate,
    thinning = d_regeneration$thinning,
    parameters = d_regeneration$parameters,
    size_dist = d_regeneration$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 2, manag_model = 2),
    check_input = TRUE, df_out = FALSE
  )

  expect_equal(round(result[120, 1, 4, 1:3], 3), c(85.191, 32.973, 4.729))
  expect_equal(round(result[120, 2, 4, 1:3], 3), c(53.333, 13.914, 1.892))
})

test_that("mort_model = 2 ignores beta* site parameters", {
  site_base <- d_regeneration$site
  site_beta_changed <- site_base

  site_beta_changed$beta0 <- -50
  site_beta_changed$betaB <- 2.5
  site_beta_changed$betaN <- 1.2
  site_beta_changed$betafN <- -5
  site_beta_changed$betafT <- -5
  site_beta_changed$betafPhys <- -5

  result_base <- run_3PG(
    site = site_base,
    species = d_regeneration$species,
    climate = d_regeneration$climate,
    thinning = d_regeneration$thinning,
    parameters = d_regeneration$parameters,
    size_dist = d_regeneration$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 2),
    check_input = TRUE, df_out = FALSE
  )

  result_beta_changed <- run_3PG(
    site = site_beta_changed,
    species = d_regeneration$species,
    climate = d_regeneration$climate,
    thinning = d_regeneration$thinning,
    parameters = d_regeneration$parameters,
    size_dist = d_regeneration$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 2),
    check_input = TRUE, df_out = FALSE
  )

  expect_equal(result_base, result_beta_changed)
})

test_that("mort_model = 3 ignores st_* site parameters", {
  site_base <- d_regeneration$site
  site_st_changed <- site_base

  site_st_changed$st_Power <- 10
  site_st_changed$st_Intercept <- 10
  site_st_changed$st_fN <- 10
  site_st_changed$st_fT <- 10
  site_st_changed$st_fPhys <- 10

  result_base <- run_3PG(
    site = site_base,
    species = d_regeneration$species,
    climate = d_regeneration$climate,
    thinning = d_regeneration$thinning,
    parameters = d_regeneration$parameters,
    size_dist = d_regeneration$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 3),
    check_input = TRUE, df_out = FALSE
  )

  result_st_changed <- run_3PG(
    site = site_st_changed,
    species = d_regeneration$species,
    climate = d_regeneration$climate,
    thinning = d_regeneration$thinning,
    parameters = d_regeneration$parameters,
    size_dist = d_regeneration$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0,
                    mort_model = 3),
    check_input = TRUE, df_out = FALSE
  )

  expect_equal(result_base, result_st_changed)
})




# test_that("Evergreen defoliation", {
#   result <- run_3PG(
#     site = d_defoliation$site,
#     species = d_defoliation$species,
#     climate = d_defoliation$climate,
#     thinning = d_defoliation$thinning,
#     defoliation = d_defoliation$defoliation,
#     parameters = d_defoliation$parameters,
#     size_dist = d_defoliation$sizeDist,
#     settings = list(light_model = 2, transp_model = 2, phys_model = 2,
#                     height_model = 1, correct_bias = 0, crown_width_model = 1,
#                     calculate_d13c = 0, mort_model = 2, manag_model = 2),
#     check_input = TRUE, df_out = FALSE
#   )

#   expect_equal(round(result[601, , 4, 1:3], 3), c(225.131, 82.174, 4.686))
# })

