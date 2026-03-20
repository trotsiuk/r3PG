library(r3PG)
library(testthat)

test_that("prepare_parameters handles NULL input", {

  sp_names <- c("Fagus sylvatica", "Pinus sylvestris")

  result <- prepare_parameters(parameters = NULL, sp_names = sp_names)

  expect_equal(nrow(result), 86)
  expect_equal(ncol(result), length(sp_names) + 1)
})

test_that("prepare_parameters validates species names", {
  expect_error(
    prepare_parameters(parameters = NULL, sp_names = NULL),
    regexp = "sp_names must be provided"
  )
})

test_that("prepare_parameters validates parameters table column names", {
  parameters_input <- data.frame(
    wrong_column = c("pFS2", "pFS20"),
    `Fagus sylvatica` = c(1.5, 2.5)
  )
  sp_names <- c("Fagus sylvatica")

  expect_error(
    prepare_parameters(parameters = parameters_input, sp_names = sp_names),
    regexp = "The first column name of the parameters table must be 'parameter'"
  )
})

test_that("prepare_parameters validates parameter names", {
  parameters_input <- data.frame(
    parameter = c("pFS2", "wrong_param"),
    `Fagus sylvatica` = c(1.5, 3.5)
  )
  sp_names <- c("Fagus sylvatica")

  expect_error(
    prepare_parameters(parameters = parameters_input, sp_names = sp_names),
    regexp = "Parameter input table must only contain parameters present in `i_parameters`"
  )
})

test_that("prepare_parameters updates parameters correctly", {
  sp_names <- c("Fagus sylvatica", "Pinus sylvestris")

  parameters_input <- data.frame(
    parameter = c("pFS2", "aWS"),
    `Fagus sylvatica` = c(1.5, 3.5),
    `Pinus sylvestris` = c(1.2, NA)
  )

  colnames(parameters_input) <- c("parameter", sp_names)

  result <- prepare_parameters(parameters = parameters_input, sp_names = sp_names)

  # Correctly extract scalar values for testing
  expect_equal(result[result$parameter == "pFS2", "Fagus sylvatica"][[1]], 1.5)
  expect_equal(result[result$parameter == "aWS", "Pinus sylvestris"][[1]], NA_real_)
})

