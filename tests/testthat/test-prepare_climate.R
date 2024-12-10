library(r3PG)
library(testthat)

test_that("prepare_climate replicates average climate", {
  d_climate <- data.frame(
    tmp_min = runif(12, -5, 5),
    tmp_max = runif(12, 10, 20),
    prcp = runif(12, 50, 150),
    srad = runif(12, 15, 25),
    frost_days = runif(12, 0, 10)
  )

  result <- prepare_climate(d_climate, from = "2000-01", to = "2005-12")

  expect_equal(nrow(result), 72) # 6 years * 12 months
  expect_equal(unique(result$tmp_min), d_climate$tmp_min)
  expect_true(all(result$tmp_min >= -5 & result$tmp_max <= 20)) # Valid range
})

test_that("prepare_climate subsets time-series data", {
  d_climate <- data.frame(
    year = rep(1990:2010, each = 12),
    month = rep(1:12, times = 21),
    tmp_min = runif(21 * 12, -5, 5),
    tmp_max = runif(21 * 12, 10, 20),
    prcp = runif(21 * 12, 50, 150),
    srad = runif(21 * 12, 15, 25),
    frost_days = runif(21 * 12, 0, 10)
  )

  result <- prepare_climate(d_climate, from = "2000-01", to = "2005-12")

  expect_equal(nrow(result), 72) # 6 years * 12 months
  expect_equal(min(result$year), 2000)
  expect_equal(max(result$year), 2005)
  expect_true(all(result$tmp_min >= -5 & result$tmp_max <= 20)) # Valid range
})

test_that("prepare_climate throws error for missing required columns", {
  d_climate <- data.frame(
    tmp_min = runif(12, -5, 5),
    tmp_max = runif(12, 10, 20)
  )

  expect_error(
    prepare_climate(d_climate, from = "2000-01", to = "2005-12"),
    regexp = "Climate table must include the following columns: tmp_min, tmp_max, prcp, srad, frost_days"
  )
})


test_that("prepare_climate handles precipitation out-of-range errors", {
  d_climate <- data.frame(
    tmp_min = runif(3, -5, 5),
    tmp_max = runif(3, 10, 20),
    prcp = c(-10, 50, 100),  # Invalid
    srad = runif(3, 15, 25),
    frost_days = runif(3, 0, 10),
    vpd_day = runif(3, 0, 10),
    year = rep(2000, 3),
    month = 1:3
  )

  expect_error(prepare_climate(d_climate, from = "2000-01", to = "2000-03"),
               regexp = "Precipitation contains negative values")
})

test_that("prepare_climate handles frost days out-of-range errors", {
  d_climate <- data.frame(
    tmp_min = runif(3, -5, 5),
    tmp_max = runif(3, 10, 20),
    prcp = runif(3, 50, 100),
    srad = runif(3, 15, 25),
    frost_days = c(-5, 5, 30), # Invalid
    vpd_day = runif(3, 0, 10),
    year = rep(2000, 3),
    month = 1:3
  )

  expect_error(prepare_climate(d_climate, from = "2000-01", to = "2000-03"),
               regexp = "Frost days contain negative values")
})

test_that("prepare_climate handles VPD out-of-range errors", {
  d_climate <- data.frame(
    tmp_min = runif(3, -5, 5),
    tmp_max = runif(3, 10, 20),
    prcp = runif(3, 50, 100),
    srad = runif(3, 15, 25),
    frost_days = runif(3, 0, 10),
    vpd_day = c(-1, 10, 15), # Invalid
    year = rep(2000, 3),
    month = 1:3
  )

  expect_error(prepare_climate(d_climate, from = "2000-01", to = "2000-03"),
               regexp = "VPD contains negative values")
})

test_that("prepare_climate triggers warnings for out-of-range values", {
  d_climate <- data.frame(
    tmp_min = c(-60, -5, 0), # Invalid
    tmp_max = c(55, 20, 25), # Invalid
    prcp = runif(3, 50, 100),
    srad = c(120, 15, 20),   # Invalid
    frost_days = runif(3, 0, 10),
    vpd_day = runif(3, 0, 10),
    year = rep(2000, 3),
    month = 1:3
  )

  expect_warning(prepare_climate(d_climate, from = "2000-01", to = "2000-03"),
                 regexp = "Temperature is outside the limits")
  expect_warning(prepare_climate(d_climate, from = "2000-01", to = "2000-03"),
                 regexp = "Solar radiation is outside the plausible range")
})



test_that("prepare_climate adds derived columns when missing", {
  d_climate <- data.frame(
    year = rep(2000:2001, each = 12),
    month = rep(1:12, times = 2),
    tmp_min = runif(24, -5, 5),
    tmp_max = runif(24, 10, 20),
    prcp = runif(24, 50, 150),
    srad = runif(24, 15, 25),
    frost_days = runif(24, 0, 10)
  )

  result <- prepare_climate(d_climate, from = "2000-01", to = "2001-12")

  expect_true("tmp_ave" %in% colnames(result))
  expect_true("vpd_day" %in% colnames(result))
  expect_true("co2" %in% colnames(result))
  expect_true("d13catm" %in% colnames(result))

  expect_equal(result$co2, rep(350, nrow(result)))
  expect_equal(result$d13catm, rep(-7.1, nrow(result)))
})

