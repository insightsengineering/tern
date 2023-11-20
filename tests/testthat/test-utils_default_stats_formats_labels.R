testthat::test_that("get_stats works as expected for defaults", {
  # Defaults are not changing
  res <- testthat::expect_silent(get_stats("count_occurrences"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("summarize_num_patients"))
  testthat::expect_snapshot(res)

  # Change depending on type
  res <- testthat::expect_silent(get_stats("analyze_vars_counts"))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(get_stats("analyze_vars_numeric"))
  testthat::expect_snapshot(res)

  testthat::expect_error(
    get_stats("dont_exist"),
    regexp = "The selected method group \\(dont_exist\\) has no default statistical method."
  )

  # Here they are different, and overlap only for n
  testthat::expect_identical(intersect(
    get_stats("analyze_vars_counts"),
    get_stats("analyze_vars_numeric")
  ), "n")

  # Test multiples
  testthat::expect_identical(
    get_stats(c("count_occurrences", "analyze_vars")),
    unique(c(
      get_stats("count_occurrences"),
      get_stats("analyze_vars_numeric")
    ))
  )
})
testthat::test_that("get_stats works well with pval", {
  # pval is added correctly
  testthat::expect_true("pval" %in% get_stats("analyze_vars_numeric", add_pval = TRUE))
  testthat::expect_true("pval_counts" %in% get_stats("analyze_vars_counts", add_pval = TRUE))
  testthat::expect_true("pval" %in% get_stats("count_occurrences", add_pval = TRUE))

  # Errors
  testthat::expect_error(get_stats("analyze_vars_counts", stats_in = c("pval", "pval_counts")))
  testthat::expect_error(
    get_stats("analyze_vars_counts", stats_in = c("n", "pval")),
    "Inserted p-value \\(pval\\) is not valid for type counts*"
  )
  testthat::expect_error(
    get_stats("analyze_vars_numeric", stats_in = c("n", "pval_counts")),
    "Inserted p-value \\(pval_counts\\) is not valid for type numeric*"
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
  testthat::expect_equal(names(res), sts)
  testthat::expect_equal(res[[1]], "xx.")

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

  # with row_nms
  testthat::expect_identical(
    get_labels_from_stats(
      stats = c("count", "count_fraction"),
      labels_in = c("c" = "Lvl c:", "count_fraction.a" = "CF: A", "count.b" = "Count of b"),
      row_nms = c("a", "b", "c")
    ),
    c(
      count.a = "a", count.b = "Count of b", count.c = "Lvl c:",
      count_fraction.a = "CF: A", count_fraction.b = "b", count_fraction.c = "Lvl c:"
    )
  )
})

testthat::test_that("get_indents_from_stats works as expected", {
  sts <- get_stats("count_occurrences")
  res <- testthat::expect_silent(get_indents_from_stats(sts))
  testthat::expect_snapshot(res)

  testthat::expect_identical(get_indents_from_stats("count", NULL)[["count"]], 0L)
  testthat::expect_identical(get_indents_from_stats(c("count"), 3L), 3L)

  # integer vector
  stats_to_do <- c("count" = 3L, "mean" = 6L)
  testthat::expect_identical(
    get_indents_from_stats(c(names(stats_to_do), "n"),
      indents_in = stats_to_do
    ),
    c(stats_to_do, n = 0L)
  )

  # with row_nms
  testthat::expect_identical(
    get_indents_from_stats(
      stats = c("count", "count_fraction"),
      indents_in = c("c" = 3L, "count_fraction.a" = 1L, "count.b" = 2L),
      row_nms = c("a", "b", "c")
    ),
    c(
      count.a = 0L, count.b = 2L, count.c = 3L,
      count_fraction.a = 1L, count_fraction.b = 0L, count_fraction.c = 3L
    )
  )
})

testthat::test_that("labels_use_control works as expected", {
  stats <- get_stats(stats_in = c("mean_ci", "mean_pval", "median_ci", "quantiles", "geom_mean_ci"))
  control <- list("conf_level" = 0.34, quantiles = c(0.24, 0.86), test_mean = 0.47)
  custom_labels <- c(mean_ci = "mean ci", quantiles = "my quantiles")

  lbls <- get_labels_from_stats(stats)
  res <- lbls %>% labels_use_control(control)
  testthat::expect_snapshot(res)

  lbls <- get_labels_from_stats(stats, labels_in = custom_labels)
  res <- lbls %>% labels_use_control(control, custom_labels)
  testthat::expect_snapshot(res)
})

testthat::test_that("summary_formats works as expected", {
  result <- summary_formats() %>%
    unlist() # More compact fruition
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- summary_formats(type = "counts", include_pval = TRUE)
  testthat::expect_true(all(result[c("n", "count", "n_blq")] == "xx."))
  testthat::expect_identical(result[["pval_counts"]], "x.xxxx | (<0.0001)")
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
