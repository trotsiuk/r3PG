library(r3PG)
library(testthat)

test_that("prepare_input processes all inputs correctly", {

  # Run the function
  result <- prepare_input(
    site = d_input$site,
    species = d_input$species,
    climate = d_input$climate,
    thinning = d_input$thinning
  )

  # Check structure of result
  expect_type(result, "list")
  expect_named(result, c("site", "species", "climate", "thinning", "defoliation","parameters", "size_dist", "settings"))

  # Check species table
  expect_equal(nrow(result$species), 2)
  expect_equal(result$species$species, c("Fagus sylvatica", "Pinus sylvestris"))

  # Check climate table
  expect_true(all(c("tmp_min", "tmp_max", "prcp", "srad", "frost_days") %in% colnames(result$climate)))

  # Check thinning table
  expect_equal(nrow(result$thinning), 2)

  # Check settings
  expect_equal(result$settings$light_model, 1)
  expect_equal(result$settings$transp_model, 1)
})
