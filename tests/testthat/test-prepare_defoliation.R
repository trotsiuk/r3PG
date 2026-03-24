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

test_that("prepare_defoliation preserves row order for coppice species", {
  # Coppice at age 20 resets age to 0; a pruning at age 5 post-coppice must

  # remain *after* the coppice in the prepared array, not be sorted before it.
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
    prop_npp = c(0.7, 0)
  )

  result <- prepare_defoliation(defoliation = defoliation, sp_names = "Sp1")

  # First event should be coppice (def_type 2), second should be pruning (def_type 1)
  expect_equal(result[1, 2, 1], 2)  # def_type of first event
  expect_equal(result[2, 2, 1], 1)  # def_type of second event
  # Ages: coppice at 20 first, pruning at 5 second (NOT sorted by age)
  expect_equal(result[1, 1, 1], 20)
  expect_equal(result[2, 1, 1], 5)
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
