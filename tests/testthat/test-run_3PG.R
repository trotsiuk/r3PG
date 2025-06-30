library(r3PG)
library(testthat)

# Basic model runs
test_that("Basic model run returns an array", {

  result <- run_3PG(
    site = d_input$site,
    species = d_input$species,
    climate = d_input$climate,
    thinning = d_input$thinning,
    parameters = d_input$parameters,
    size_dist = d_input$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    correct_bias = 1, calculate_d13c = 0, mort_model = 1),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal( class(result), "array")
})

test_that("run_3PG returns a data frame with the correct structure", {
  result <- run_3PG(
    site = d_input$site,
    species = d_input$species,
    climate = d_input$climate,
    thinning = d_input$thinning,
    parameters = d_input$parameters,
    size_dist = d_input$sizeDist,
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
    site = d_input$site,
    species = d_input$species[2, ],
    climate = d_input$climate,
    thinning = NULL,
    parameters = d_input$parameters[, c(1, 3)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, , 4, 1:3], 3), c(125.423, 39.146, 3.845))
})

test_that("Evergreen 3-PGmix produces expected output", {
  out <- run_3PG(
    site = d_input$site,
    species = d_input$species[2, ],
    climate = d_input$climate,
    thinning = NULL,
    parameters = d_input$parameters[, c(1, 3)],
    size_dist = d_input$sizeDist[, c(1, 3)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(floor(out[120, , 4, 1:3]), c(127, 41, 3))
})

# Broadleaf model checks
test_that("Broadleaf 3-PGpjs produces expected output", {
  out <- run_3PG(
    site = d_input$site,
    species = d_input$species[1, ],
    climate = d_input$climate,
    thinning = NULL,
    parameters = d_input$parameters[, c(1, 2)],
    size_dist = NULL,
    settings = list(light_model = 1, transp_model = 1, phys_model = 1,
                    height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, , 4, 1:3], 3), c(140.099, 38.013, 0.000))
})

test_that("Broadleaf 3-PGmix produces expected output", {
  out <- run_3PG(
    site = d_input$site,
    species = d_input$species[1, ],
    climate = d_input$climate,
    thinning = NULL,
    parameters = d_input$parameters[, c(1, 2)],
    size_dist = d_input$sizeDist[, c(1, 2)],
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, , 4, 1:3], 3), c(142.919,  40.056, 0.000))
})

# Mixed-species model check
test_that("Mixed-species 3-PGmix produces expected output", {
  out <- run_3PG(
    site = d_input$site,
    species = d_input$species,
    climate = d_input$climate,
    thinning = d_input$thinning,
    parameters = d_input$parameters,
    size_dist = d_input$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 1, calculate_d13c = 0),
    check_input = TRUE, df_out = FALSE
  )
  expect_equal(round(out[120, 1, 4, 1:3], 3), c(94.901, 25.239, 0.000))
  expect_equal(round(out[120, 2, 4, 1:3], 3), c(59.262, 17.022,  1.659))
})


# Regeneration
test_that("Development test for mortality model", {

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


  expect_equal(round(result[120, 1, 4, 1:3], 3), c(73.814, 31.426, 4.455))
  expect_equal(round(result[120, 2, 4, 1:3], 3), c(46.255, 14.359,  1.977))

  expect_equal(round(result[5000, 20:24, 4, 1], 3), c(131.198,  61.939,  78.341,  17.080,  11.148))
  expect_equal(round(result[5000, 20:24, 4, 2], 3), c(37.647, 25.722, 34.888,  9.554,  9.856))

  expect_equal(round(result[1000, 4, 8, 5], 3), c(0.694))
  expect_equal(round(result[5000, 20, 8, 5], 3), c(2.409))

})
