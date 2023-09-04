testthat::context("Utilities for list of defaults stats/formats/labels")

testthat::test_that("get_stats works as expected for defaults", {
  # Defaults are not changing
  res <- testthat::expect_silent(get_stats("count_occurrences"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("summarize_num_patients"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("analyze_vars", type = "counts"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("analyze_vars", type = "numeric"))
  testthat::expect_snapshot(res)

  # pval is added correctly
  testthat::expect_contains(get_stats("analyze_vars", type = "numeric", add_pval = TRUE), "pval")

  testthat::expect_error(get_stats("dont_exist"), regexp = "dont_exist is a method*")

  # Type affects only counts and numeric for analyze_vars
  testthat::expect_identical(get_stats("count_occurrences", type = "counts"),
                             get_stats("count_occurrences", type = "numeric"))

  # Here they are different, and overlap only for n
  testthat::expect_identical(intersect(get_stats("analyze_vars", type = "counts"),
            get_stats("analyze_vars", type = "numeric")), "n")
})

testthat::test_that("get_stats works as expected for selection of stats", {
  sts_in <- c("mean", "n")
  res <- testthat::expect_silent(get_stats("analyze_vars", stats_in = sts_in))
  testthat::expect_identical(res, sts_in)

  # False insertion
  testthat::expect_error(get_stats("analyze_vars", stats_in = "unique"),
                         regexp = "*unique")
})

testthat::test_that("get_format_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_format_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_null(get_format_from_stats(c("nothing", "n"))[["nothing"]])

  # list check
  stats_to_do <- c("not_a_stat" = function(x) as.character(x), "mean" = "xx.")
  testthat::expect_equal(get_format_from_stats(names(stats_to_do), formats_in = stats_to_do),
                         stats_to_do)

  testthat::expect_error(get_format_from_stats(names(stats_to_do),
                                               formats_in = c(stats_to_do, "catch_me" = "xx")),
                         regexp = "*catch_me")

  # character vector
  stats_to_do <- c("not_a_stat" = "xx", "mean" = "xx")
  testthat::expect_error(get_format_from_stats(names(stats_to_do),
                        formats_in = c(stats_to_do,
                                       "catch_me" = "xx")),
                        regexp = "*catch_me")
})

testthat::test_that("get_label_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_label_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_identical(get_label_from_stats(c("nothing", "unique"))[["unique"]],
                             tern_default_labels()[["unique"]])
})
