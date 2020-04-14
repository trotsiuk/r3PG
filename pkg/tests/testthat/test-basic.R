library(r3PG)
library(testthat)

context("Basic Model runs work")

test_that("basic model run", {
    out <- run_3PG(
      site = d_site,
      species = d_species,
      climate = d_climate,
      thinning = d_thinning,
      parameters = d_parameters,
      size_dist = d_sizeDist,
      settings = list(light_model = 2, transp_model = 2, phys_model = 2,
        correct_sizeDist = 1, calculate_d13c = 0),
      check_input = TRUE, df_out = FALSE)

    testthat::expect_true(class(out) == "array")
})


