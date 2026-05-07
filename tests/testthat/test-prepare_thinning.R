library(r3PG)
library(testthat)

test_that("prepare_thinning handles NULL input", {
  sp_names <- c("Fagus sylvatica", "Pinus sylvestris")
  result <- prepare_thinning(thinning = NULL, sp_names = sp_names)
  expect_equal(dim(result), c(1, 7, length(sp_names)))
  expect_true(all(is.na(result)))
})

test_that("prepare_thinning validates species names", {
  expect_error(
    prepare_thinning(thinning = NULL, sp_names = NULL),
    "sp_names must be provided and correspond to the species table"
  )
})

test_that("prepare_thinning validates thinning table column names", {
  thinning_data <- data.frame(
    species = c("Fagus sylvatica"),
    age = c(10),
    stems_n = c(500)
  )
  sp_names <- c("Fagus sylvatica")
  expect_error(
    prepare_thinning(thinning = thinning_data, sp_names = sp_names),
    "The 'thinning' table is missing the following compulsory columns:"
  )
})

test_that("prepare_thinning validates thinning proportions", {
  thinning_data <- data.frame(
    species = c("Fagus sylvatica"),
    age = c(10),
    stems_n = c(500),
    stem = c(10), # Invalid
    root = c(1),
    foliage = c(1)
  )
  sp_names <- c("Fagus sylvatica")

  expect_error(
    prepare_thinning(thinning = thinning_data, sp_names = sp_names),
    regexp = "Thinning values for stem, root, and foliage must be in the range"
  )
})


test_that("prepare_thinning processes thinning data correctly", {
  thinning_data <- data.frame(
    species = c("Fagus sylvatica", "Pinus sylvestris"),
    age = c(10, 15),
    stems_n = c(500, 400),
    stem = c(1, 1),
    root = c(1, 1),
    foliage = c(1, 1)
  )
  sp_names <- c("Fagus sylvatica", "Pinus sylvestris")
  result <- prepare_thinning(thinning = thinning_data, sp_names = sp_names)
  expect_equal(dim(result), c(1, 7, 2))
  expect_equal(dimnames(result)[[3]], sp_names)
})

test_that("prepare_thinning keeps age-based ordering", {
  thinning_data <- data.frame(
    species = rep("Fagus sylvatica", 2),
    age = c(30, 10),
    stems_n = c(200, 100),
    stem = c(1, 1),
    root = c(1, 1),
    foliage = c(1, 1)
  )
  sp_names <- "Fagus sylvatica"
  result <- prepare_thinning(thinning = thinning_data, sp_names = sp_names)

  # Ordered by age: 10 first, 30 second
  expect_equal(result[1, 1, 1], 10)
  expect_equal(result[2, 1, 1], 30)
})

test_that("prepare_thinning validates integer order_coppice_events", {
  thinning_data <- data.frame(
    species = "Fagus sylvatica",
    age = 10,
    stems_n = 500,
    stem = 1,
    root = 1,
    foliage = 1,
    order_coppice_events = 2.5
  )

  expect_error(
    prepare_thinning(thinning = thinning_data, sp_names = "Fagus sylvatica"),
    "positive integers"
  )
})

test_that("prepare_thinning requires order_coppice_events for duplicate species-age events", {
  thinning_data <- data.frame(
    species = c("Fagus sylvatica", "Fagus sylvatica"),
    age = c(10, 10),
    stems_n = c(500, 400),
    stem = c(1, 1),
    root = c(1, 1),
    foliage = c(1, 1)
  )

  expect_error(
    prepare_thinning(thinning = thinning_data, sp_names = "Fagus sylvatica"),
    "order_coppice_events"
  )
})
