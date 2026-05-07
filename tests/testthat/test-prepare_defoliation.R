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
    prop_npp = 0.7,
    order_coppice_events = 1
  )

  result <- prepare_defoliation(
    defoliation = defoliation,
    sp_names = "Pinus sylvestris_1"
  )

  expect_equal(dim(result), c(1, 10, 1))
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

test_that("prepare_defoliation keeps age-based ordering with coppice events", {
  defoliation <- data.frame(
    species = rep("Sp1", 2),
    age = c(20, 5),
    def_type = c(2, 1),
    stem_retained = c(0, 1),
    foliage_retained = c(0, 0.5),
    root_retained = c(0.3, 1),
    stem = c(1, 1),
    def_recover_t = c(12, 6),
    prop_carbs = c(0.4, 0),
    prop_npp = c(0.7, 0),
    order_coppice_events = c(1, 2)
  )

  result <- prepare_defoliation(defoliation = defoliation, sp_names = "Sp1")

  # Ordered by age: pruning (5) before coppice (20)
  expect_equal(result[1, 2, 1], 1)
  expect_equal(result[2, 2, 1], 2)
  expect_equal(result[1, 1, 1], 5)
  expect_equal(result[2, 1, 1], 20)
})

test_that("prepare_defoliation requires order_coppice_events for coppice events", {
  defoliation <- data.frame(
    species = "Sp1",
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

  expect_error(
    prepare_defoliation(defoliation = defoliation, sp_names = "Sp1"),
    "order_coppice_events"
  )
})

test_that("prepare_defoliation validates integer order_coppice_events", {
  defoliation <- data.frame(
    species = "Sp1",
    age = 20,
    def_type = 2,
    stem_retained = 0,
    foliage_retained = 0,
    root_retained = 0.3,
    stem = 1,
    def_recover_t = 12,
    prop_carbs = 0.4,
    prop_npp = 0.7,
    order_coppice_events = 1.5
  )

  expect_error(
    prepare_defoliation(defoliation = defoliation, sp_names = "Sp1"),
    "positive integers"
  )
})

test_that("prepare_defoliation sorts by age for non-coppice species", {
  # Without coppice, events should still be sorted by age
  defoliation <- data.frame(
    species = rep("Sp1", 2),
    age = c(30, 10),
    def_type = c(1, 1),
    stem_retained = c(1, 1),
    foliage_retained = c(0.5, 0.5),
    root_retained = c(1, 1),
    stem = c(1, 1),
    def_recover_t = c(6, 6),
    prop_carbs = c(0, 0),
    prop_npp = c(0, 0)
  )

  result <- prepare_defoliation(defoliation = defoliation, sp_names = "Sp1")

  # Sorted by age: 10 first, 30 second
  expect_equal(result[1, 1, 1], 10)
  expect_equal(result[2, 1, 1], 30)
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
