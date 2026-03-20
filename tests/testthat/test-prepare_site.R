library(r3PG)
library(testthat)

test_that("prepare_site works with valid input", {
  d_site <- data.frame(
    latitude = 46.8,
    elevation = 1200,
    soil_class = 2,
    asw_i = 150,
    asw_min = 100,
    asw_max = 200,
    from = "2000-01",
    to = "2009-12"
  )
  result <- prepare_site(d_site)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$latitude, 46.8)
})

test_that("prepare_site detects multiple rows", {
  d_site <- data.frame(
    latitude = c(46.8, 47.0),
    elevation = c(1200, 1300),
    soil_class = c(2, 3),
    asw_i = c(150, 200),
    asw_min = c(100, 150),
    asw_max = c(200, 250),
    from = c("2000-01", "2001-01"),
    to = c("2009-12", "2010-12")
  )
  expect_error(prepare_site(d_site), "The 'site' table must contain exactly one row.")
})

test_that("prepare_site detects missing columns", {
  d_site <- data.frame(
    latitude = 46.8,
    elevation = 1200
  )
  expect_error(prepare_site(d_site), "The 'site' table must contain the following columns in order:")
})

test_that("prepare_site validates latitude range", {
  d_site <- data.frame(
    latitude = 100, # Invalid
    elevation = 1200,
    soil_class = 2,
    asw_i = 150,
    asw_min = 100,
    asw_max = 200,
    from = "2000-01",
    to = "2009-12"
  )
  expect_error( prepare_site(d_site), regexp = "Latitude must be within the range \\[-90, 90\\]." )
})

test_that("prepare_site validates elevation range", {
  d_site <- data.frame(
    latitude = 46.8,
    elevation = 5000, # Invalid
    soil_class = 2,
    asw_i = 150,
    asw_min = 100,
    asw_max = 200,
    from = "2000-01",
    to = "2009-12"
  )
  expect_error( prepare_site(d_site), regexp = "Elevation must be within the range \\[0, 4000\\].")
})


test_that("prepare_site validates date format and range", {
  d_site <- data.frame(
    latitude = 46.8,
    elevation = 1200,
    soil_class = 2,
    asw_i = 150,
    asw_min = 100,
    asw_max = 200,
    from = "2009-01",
    to = "2000-12" # Invalid range
  )
  expect_error(prepare_site(d_site), "The 'from' date must be earlier than the 'to' date.")
})

test_that("prepare_site warns and converts legacy altitude column", {
  d_site <- data.frame(
    latitude = 46.8,
    altitude = 1200,
    soil_class = 2,
    asw_i = 150,
    asw_min = 100,
    asw_max = 200,
    from = "2000-01",
    to = "2009-12"
  )

  expect_warning(
    result <- prepare_site(d_site),
    "Deprecated site column 'altitude' detected; converting to 'elevation'"
  )

  expect_true("elevation" %in% names(result))
  expect_equal(result$elevation, 1200)
})
