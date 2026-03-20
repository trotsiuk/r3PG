library(r3PG)
library(testthat)

test_that("prepare_defoliation accepts valid coppice event", {
  defoliation <- data.frame(
    species = "Pinus sylvestris_1",
    age = 20,
    def_type = 2,
    stem_retained = 0,
    foliage_retained = 0,
    root_retained = 0.3,
    stem = 1,
    def_recover_t = 12,
    prop_carbs = 0.4,
    prop_npp = 0.7
  )

  result <- prepare_defoliation(
    defoliation = defoliation,
    sp_names = "Pinus sylvestris_1"
  )

  expect_equal(dim(result), c(1, 9, 1))
})

test_that("prepare_defoliation rejects invalid epicormic constraints", {
  defoliation <- data.frame(
    species = "Pinus sylvestris_1",
    age = 20,
    def_type = 3,
    stem_retained = 0.5,
    foliage_retained = 0.5,
    root_retained = 0.1,
    stem = 1,
    def_recover_t = 12,
    prop_carbs = 0.4,
    prop_npp = 0.7
  )

  expect_error(
    prepare_defoliation(defoliation = defoliation, sp_names = "Pinus sylvestris_1"),
    "Defoliation input error"
  )
})

test_that("prepare_defoliation rejects too-short recovery time", {
  defoliation <- data.frame(
    species = "Pinus sylvestris_1",
    age = 20,
    def_type = 1,
    stem_retained = 1,
    foliage_retained = 0.5,
    root_retained = 1,
    stem = 1,
    def_recover_t = 1,
    prop_carbs = 0.4,
    prop_npp = 0.7
  )

  expect_error(
    prepare_defoliation(defoliation = defoliation, sp_names = "Pinus sylvestris_1"),
    "def_recover_t"
  )
})
