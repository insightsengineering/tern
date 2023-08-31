testthat::context("Utilities for list of defaults stats/formats/labels")

testthat::test_that("get_stats works as expected", {
  res <- testthat::expect_silent()
  testthat::expect_snapshot(res)

  testthat::expect_error(get_stats("dont_exist"), regexp = "dont_exist is a method*")
})

testthat::test_that("get_format_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_format_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_error(get_stats("dont_exist"), regexp = "dont_exist is a method*")
})

testthat::test_that("get_label_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_label_from_stats(sts))
  testthat::expect_snapshot(res)

  # testthat::expect_error(get_label_from_stats("Maas"), regexp = "dont_exist is a method*")
})
