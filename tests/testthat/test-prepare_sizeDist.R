library(r3PG)
library(testthat)

test_that("prepare_sizeDist handles NULL input", {

  sp_names <- c("Fagus sylvatica", "Pinus sylvestris")

  result <- prepare_sizeDist(size_dist = NULL, sp_names = sp_names)

  expect_equal(nrow(result), 30)
  expect_equal(ncol(result), length(sp_names) + 1)
})

test_that("prepare_sizeDist validates species names", {
  expect_error(
    prepare_sizeDist(size_dist = NULL, sp_names = NULL),
    regexp = "sp_names must be provided"
  )
})

test_that("prepare_sizeDist validates size_dist table column names", {
  size_dist_input <- data.frame(
    wrong_column = c("Dscale0", "DscaleB"),
    `Fagus sylvatica` = c(1.5, 2.5)
  )
  sp_names <- c("Fagus sylvatica")

  expect_error(
    prepare_sizeDist(size_dist = size_dist_input, sp_names = sp_names),
    regexp = "The first column name of the size_dist table must be 'parameter'."
  )
})

test_that("prepare_sizeDist validates parameter names", {
  size_dist_input <- data.frame(
    parameter = c("Dscale0", "wrong_param"),
    `Fagus sylvatica` = c(1.5, 3.5)
  )
  sp_names <- c("Fagus sylvatica")

  expect_error(
    prepare_sizeDist(size_dist = size_dist_input, sp_names = sp_names),
    regexp = "size_dist input table must only contain parameters present in `i_sizeDist`. Check `param_info` for more details."
  )
})

test_that("prepare_sizeDist updates size_dist correctly", {
  sp_names <- c("Fagus sylvatica", "Pinus sylvestris")

  size_dist_input <- data.frame(
    parameter = c("Dscale0", "DscaleB"),
    `Fagus sylvatica` = c(1.5, 3.5),
    `Pinus sylvestris` = c(1.2, NA)
  )

  colnames(size_dist_input) <- c("parameter", sp_names)

  result <- prepare_sizeDist(size_dist = size_dist_input, sp_names = sp_names)

  # Correctly extract scalar values for testing
  expect_equal(result[result$parameter == "Dscale0", "Fagus sylvatica"][[1]], 1.5)
  expect_equal(result[result$parameter == "DscaleB", "Pinus sylvestris"][[1]], NA_real_)
})

