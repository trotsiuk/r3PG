library(r3PG)
library(testthat)


# ---------------------------------------------------------------------------
# Helper: run a defoliation scenario from an .rds fixture
# ---------------------------------------------------------------------------
run_defoliation_scenario <- function(rds_path, mort_model = 2) {
  d <- readRDS(rds_path)
  run_3PG(
    site = d$site, species = d$species, climate = d$climate,
    defoliation = d$defoliation, parameters = d$parameters,
    size_dist = d$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, crown_width_model = 1,
                    calculate_d13c = 0, mort_model = mort_model, manag_model = 1),
    check_input = TRUE, df_out = FALSE
  )
}


# ===========================================================================
# 1. Basic model runs
# ===========================================================================

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
  expect_equal(class(result), "array")
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
  expect_equal(class(result), "data.frame")
  expect_true(all(c("date", "species", "group", "variable", "value") %in% colnames(result)))
})


# ===========================================================================
# 2. Single-species: Evergreen
# ===========================================================================

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
  expect_equal(result[120, , 4, 1:3], c(125.42308, 39.14578, 3.84459), tolerance = 5e-6)
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
  expect_equal(result[120, , 4, 1:3], c(123.35742, 37.61049, 3.62474), tolerance = 5e-6)
})


# ===========================================================================
# 3. Single-species: Broadleaf
# ===========================================================================

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
  expect_equal(result[120, , 4, 1:3], c(140.24858, 38.08248, 0.00000), tolerance = 5e-6)
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
  expect_equal(result[120, , 4, 1:3], c(144.27255, 40.55360, 0.00000), tolerance = 5e-6)
})


# ===========================================================================
# 4. Mixed-species
# ===========================================================================

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
  expect_equal(result[120, 1, 4, 1:3], c(95.15685, 24.91093, 0.00000), tolerance = 5e-6)
  expect_equal(result[120, 2, 4, 1:3], c(57.19164, 15.10513, 1.41455), tolerance = 5e-6)
})


# ===========================================================================
# 5. Mortality models
# ===========================================================================

test_that("Mortality model 2 produces expected biomass", {
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

  expect_equal(result[120, 1, 4, 1:3], c(85.19058, 32.97327, 4.72888), tolerance = 5e-6)
  expect_equal(result[120, 2, 4, 1:3], c(53.33273, 13.91404, 1.89198), tolerance = 5e-6)

  expect_equal(result[5000, 20:24, 4, 1], c(102.40746, 66.01177, 45.36782, 27.78603, 9.49833), tolerance = 5e-6)
  expect_equal(result[5000, 20:24, 4, 2], c(37.30721, 24.12720, 18.55220, 14.90944, 8.22990), tolerance = 5e-6)

  expect_equal(result[1000, 4, 8, 5], 0.73787, tolerance = 5e-6)
  expect_equal(result[5000, 20, 8, 5], 0.73792, tolerance = 5e-6)
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


# ===========================================================================
# 6. Management
# ===========================================================================

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

  expect_equal(result[120, 1, 4, 1:3], c(85.19058, 32.97327, 4.72888), tolerance = 5e-6)
  expect_equal(result[120, 2, 4, 1:3], c(53.33273, 13.91404, 1.89198), tolerance = 5e-6)
})


# ===========================================================================
# 7. Defoliation — package built-in data
# ===========================================================================

test_that("Evergreen defoliation with d_defoliation", {
  result <- run_3PG(
    site = d_defoliation$site,
    species = d_defoliation$species,
    climate = d_defoliation$climate,
    thinning = d_defoliation$thinning,
    defoliation = d_defoliation$defoliation,
    parameters = d_defoliation$parameters,
    size_dist = d_defoliation$sizeDist,
    settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                    height_model = 1, correct_bias = 0, crown_width_model = 1,
                    calculate_d13c = 0, mort_model = 2, manag_model = 2),
    check_input = TRUE, df_out = FALSE
  )

  expect_equal(result[601, , 4, 1:3], c(225.14222, 89.82836, 5.47746), tolerance = 5e-6)
})


# ===========================================================================
# 8. Defoliation — scenario fixtures (tests/testthat/fixtures)
# ===========================================================================

test_that("Coppice defoliation resets height and biomass", {
  result <- run_defoliation_scenario(test_path("fixtures", "d_coppice.rds"))

  # Array: 601 months × 1 species × 11 groups × 20 variables
  expect_equal(dim(result)[1], 601L)

  # Final biomass: biom_stem, biom_root, biom_foliage (group 4, vars 1-3)
  expect_equal(result[601, 1, 4, 1:3], c(165.66733, 107.33562, 10.46060), tolerance = 5e-6)

  # Coppice event at month 181 (age 20):
  #   height resets from ~18.2 m to ~1.5 m (coppice regrowth height)
  expect_equal(result[180, 1, 2, 6], 18.21505, tolerance = 5e-6)
  expect_equal(result[181, 1, 2, 6], 1.54070, tolerance = 5e-6)

  # Stem and foliage biomass zeroed at the coppice event
  expect_equal(result[181, 1, 4, 1], 0.0, tolerance = 5e-6)
  expect_equal(result[181, 1, 4, 3], 0.0, tolerance = 5e-6)

  # def_type flag recorded in output (group 11, var 17)
  expect_equal(result[180, 1, 11, 17], 0)
  expect_equal(result[181, 1, 11, 17], 2)
})

test_that("Epicormic defoliation kills stems and reduces foliage", {
  result <- run_defoliation_scenario(test_path("fixtures", "d_epicormic.rds"))

  # Final biomass
  expect_equal(result[601, 1, 4, 1:3], c(267.53013, 93.39865, 5.75085), tolerance = 5e-6)

  # Epicormic event at month 121 (age 40, stem_retained = 0.5):
  #   stems_n drops from 200 to 100 (group 2, var 2)
  expect_equal(result[120, 1, 2, 2], 200, tolerance = 5e-6)
  expect_equal(result[121, 1, 2, 2], 100, tolerance = 5e-6)

  # Defoliation stem losses recorded (group 11, var 13)
  expect_equal(result[121, 1, 11, 13], 100, tolerance = 5e-6)

  # Height is preserved (epicormic, not coppice)
  expect_true(abs(result[121, 1, 2, 6] - result[120, 1, 2, 6]) < 0.1)
})

test_that("Pruning with physiological assistance produces higher biomass", {
  result_phys    <- run_defoliation_scenario(test_path("fixtures", "d_pruning.rds"))
  result_no_phys <- run_defoliation_scenario(test_path("fixtures", "d_pruning_no_phys.rds"))

  # Reference final biomass values
  expect_equal(result_phys[601, 1, 4, 1:3],    c(271.87322, 92.40271, 6.09444), tolerance = 5e-6)
  expect_equal(result_no_phys[601, 1, 4, 1:3], c(266.59911, 90.50186, 6.13864), tolerance = 5e-6)

  # Physiological assistance (prop_carbs > 0, prop_npp > 0) yields more stem biomass
  expect_gt(result_phys[601, 1, 4, 1], result_no_phys[601, 1, 4, 1])
})

test_that("Pruning without physiological assistance produces expected output", {
  result <- run_defoliation_scenario(test_path("fixtures", "d_pruning_no_phys.rds"))

  # Final stand: height (group 2, var 6)
  expect_equal(result[601, 1, 2, 6], 34.61828, tolerance = 5e-6)

  # Final biomass
  expect_equal(result[601, 1, 4, 1:3], c(266.59911, 90.50186, 6.13864), tolerance = 5e-6)
})

