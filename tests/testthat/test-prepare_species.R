library(r3PG)
library(testthat)

test_that("prepare_species works with valid input", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", "1995-06"),
    fertility = c(0.8, 0.6),
    stems_n = c(500, 300),
    biom_stem = c(120, 90),
    biom_root = c(30, 25),
    biom_foliage = c(15, 10)
  )

  result <- prepare_species(d_species)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(d_species))
  expect_true(all(c("lt_fN", "lt_fT", "lt_fPhys") %in% colnames(result)))
  expect_true(all(is.na(result$lt_fN))) # Optional columns should be NA by default
})

test_that("prepare_species detects missing compulsory columns", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", "1995-06"),
    fertility = c(0.8, 0.6),
    stems_n = c(500, 300)
  )

  expect_error(
    prepare_species(d_species),
    "The 'species' table is missing the following compulsory columns:"
  )
})

test_that("prepare_species detects NA values in compulsory columns", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", NA),
    fertility = c(0.8, 0.6),
    stems_n = c(500, 300),
    biom_stem = c(120, 90),
    biom_root = c(30, 25),
    biom_foliage = c(15, 10)
  )

  expect_error(
    prepare_species(d_species),
    "The 'species' table contains NA values in compulsory columns."
  )
})

test_that("prepare_species validates fertility range", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", "1995-06"),
    fertility = c(1.2, 0.6), # Invalid value
    stems_n = c(500, 300),
    biom_stem = c(120, 90),
    biom_root = c(30, 25),
    biom_foliage = c(15, 10)
  )

  expect_error(
    prepare_species(d_species),
    "The 'fertility' column must contain values between 0 and 1."
  )
})

test_that("prepare_species validates non-negative biomass and stem numbers", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", "1995-06"),
    fertility = c(0.8, 0.6),
    stems_n = c(-500, 300), # Invalid value
    biom_stem = c(120, 90),
    biom_root = c(30, 25),
    biom_foliage = c(15, 10)
  )

  expect_error(
    prepare_species(d_species),
    "The 'stems_n' column must contain non-negative values."
  )
})

test_that("prepare_species handles long-term modifier validation", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", "1995-06"),
    fertility = c(0.8, 0.6),
    stems_n = c(500, 300),
    biom_stem = c(120, 90),
    biom_root = c(30, 25),
    biom_foliage = c(15, 10),
    lt_fN = c(-0.5, NA), # Invalid value
    lt_fT = c(0.3, NA),
    lt_fPhys = c(NA, NA)
  )

  expect_error(
    prepare_species(d_species),
    "Long-term modifiers \\(lt_fN, lt_fT, lt_fPhys\\) must contain non-negative values."
  )
})

test_that("prepare_species warns for high biomass values", {
  d_species <- data.frame(
    species = c("Pine", "Oak"),
    planted = c("2000-01", "1995-06"),
    fertility = c(0.8, 0.6),
    stems_n = c(500, 300),
    biom_stem = c(12000, 90), # Unreasonably high value
    biom_root = c(30, 25),
    biom_foliage = c(15, 10)
  )

  expect_warning(
    prepare_species(d_species),
    "Some values in 'biom_stem' are greater than 10000. Please verify the input data."
  )
})
