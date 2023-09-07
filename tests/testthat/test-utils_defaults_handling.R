testthat::test_that("get_stats works as expected for defaults", {
  # Defaults are not changing
  res <- testthat::expect_silent(get_stats("count_occurrences"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("summarize_num_patients"))
  testthat::expect_snapshot(res)

  # Change depending on type
  res <- testthat::expect_silent(get_stats("analyze_vars", type = "counts"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("analyze_vars", type = "numeric"))
  testthat::expect_snapshot(res)

  testthat::expect_error(
    get_stats("dont_exist"),
    regexp = "The selected method group \\(dont_exist\\) has no default statistical method."
  )

  # Type affects only counts and numeric for analyze_vars
  testthat::expect_identical(
    get_stats("count_occurrences", type = "counts"),
    get_stats("count_occurrences", type = "numeric")
  )

  # Here they are different, and overlap only for n
  testthat::expect_identical(intersect(
    get_stats("analyze_vars", type = "counts"),
    get_stats("analyze_vars", type = "numeric")
  ), "n")

  # Test multiples
  testthat::expect_identical(
    get_stats(c("count_occurrences", "analyze_vars"), type = c("numeric", "counts")),
    unique(c(
      get_stats("count_occurrences"),
      get_stats("analyze_vars", type = "numeric"),
      get_stats("analyze_vars", type = "counts")
    ))
  )
})
testthat::test_that("get_stats works well with pval", {
  # pval is added correctly
  testthat::expect_contains(get_stats("analyze_vars", type = "numeric", add_pval = TRUE), "pval")
  testthat::expect_contains(get_stats("analyze_vars", type = "counts", add_pval = TRUE), "pval_counts")
  testthat::expect_identical(
    get_stats("count_occurrences", type = "numeric", add_pval = TRUE),
    get_stats("count_occurrences", type = NULL, add_pval = TRUE)
  )
  testthat::expect_contains(get_stats("count_occurrences", type = NULL, add_pval = TRUE), "pval")
  testthat::expect_contains(get_stats("count_occurrences", type = "counts", add_pval = TRUE), "pval_counts")

  # Errors
  testthat::expect_error(get_stats("analyze_vars", type = "counts", stats_in = c("pval", "pval_counts")))
  testthat::expect_error(
    get_stats("analyze_vars", type = c("counts", "numeric"), stats_in = c("pval")),
    "Only one type*"
  )
  testthat::expect_error(
    get_stats("analyze_vars", type = "counts", stats_in = c("n", "pval")),
    "Inserted p-value \\(pval\\) is not valid for type counts*"
  )
  testthat::expect_error(
    get_stats("analyze_vars", type = "numeric", stats_in = c("n", "pval_counts")),
    "Inserted p-value \\(pval_counts\\) is not valid for type numeric*"
  )

  testthat::expect_error(
    get_stats("analyze_vars", type = c("counts", "numeric"), add_pval = TRUE),
    "Only one type is allowed when add_pval = TRUE."
  )
})

testthat::test_that("get_stats works as expected for selection of stats", {
  sts_in <- c("mean", "n")
  res <- testthat::expect_silent(get_stats("analyze_vars", stats_in = sts_in))
  testthat::expect_identical(res, sts_in)

  # False insertion
  testthat::expect_error(get_stats("analyze_vars", stats_in = "unique"),
    regexp = "*unique"
  )

  # False insertion
  testthat::expect_error(get_stats("count_occurrences", stats_in = "unique"),
    regexp = "*unique"
  )
})

testthat::test_that("get_formats_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_formats_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_null(get_formats_from_stats(c("nothing", "n"))[["nothing"]])

  # list check
  stats_to_do <- c("not_a_stat" = function(x) as.character(x), "mean" = "xx.")
  testthat::expect_equal(
    get_formats_from_stats(names(stats_to_do), formats_in = stats_to_do),
    stats_to_do
  )

  # Works also if we had a not present format
  testthat::expect_identical(
    get_formats_from_stats(names(stats_to_do),
      formats_in = c(stats_to_do, "catch_me" = "xx")
    ),
    stats_to_do
  )

  # character vector is the same -> default have functions, so it is casted to list
  stats_to_do <- c("not_a_stat" = "xx", "mean" = "xx")
  testthat::expect_identical(
    get_formats_from_stats(names(stats_to_do),
      formats_in = c(stats_to_do,
        "catch_me" = "xx"
      )
    ),
    as.list(stats_to_do)
  )
})

testthat::test_that("get_labels_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_labels_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_identical(get_labels_from_stats(c("nothing", "n"))[["nothing"]], "")

  testthat::expect_identical(
    get_labels_from_stats(c("nothing", "unique"))[["unique"]],
    tern_default_labels[["unique"]]
  )

  # list check
  stats_to_do <- c("not_a_stat" = function(x) as.character(x), "mean" = "xx.")
  testthat::expect_equal(
    get_labels_from_stats(names(stats_to_do), labels_in = stats_to_do),
    stats_to_do
  )

  testthat::expect_identical(
    get_labels_from_stats(names(stats_to_do),
      labels_in = c(stats_to_do, "catch_me" = "xx")
    ),
    stats_to_do
  )

  # character vector
  stats_to_do <- c("not_a_stat" = "xx", "mean" = "xx")
  testthat::expect_identical(
    get_labels_from_stats(names(stats_to_do),
      labels_in = c(stats_to_do,
        "catch_me" = "xx"
      )
    ),
    stats_to_do
  )
})

testthat::test_that("summary_formats works as expected", {
  result <- summary_formats() %>%
    unlist() # More compact fruition
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- summary_formats(type = "counts", include_pval = TRUE)
  testthat::expect_true(all(result[c("n", "count", "n_blq")] == "xx."))
  testthat::expect_identical(result[["pval_counts"]], "x.xxxx | (<0.0001)")
  expect_identical(result[["count_fraction"]], format_count_fraction)
})

testthat::test_that("summary_labels works as expected", {
  result <- summary_labels()
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- summary_labels(type = "counts", include_pval = TRUE)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# Deprecated
testthat::test_that("summary_custom works as expected", {
  testthat::expect_warning(res <- summary_custom())
  res$formats <- unlist(res$formats)
  testthat::expect_snapshot(res)

  testthat::expect_warning(res <- summary_custom(
    type = "counts", stats_custom = c("n", "count"),
    formats_custom = c(n = "xx.xx"), labels_custom = c(count = "#"), indent_mods_custom = 2L
  ))

  testthat::expect_snapshot(res)
})

