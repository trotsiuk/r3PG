library(r3PG)
library(testthat)

context("Basic Model runs work")

test_that("basic model run", {
  out <- run_3PG(
    siteInputs = site_eum,
    speciesInputs = species_eum,
    forcingInputs = climate_eum,
    managementInputs = NULL,
    parameterInputs = parameters_eum,
    sizeDistInputs = sizeDist_eum,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    correct_sizeDist = 0, calculate_d13c = 0),
    df_out = F)


  out_long <- transf_out( out )
})


