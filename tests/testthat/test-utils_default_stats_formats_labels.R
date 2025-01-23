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

  testthat::expect_identical(get_labels_from_stats(c("nothing", "n"))[["nothing"]], NA_character_)

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

testthat::test_that("get_labels_from_stats with labels in works when adding levels to stats", {
  labels_custom <- c("c" = "Lvl c:", "a" = "any A", "count" = "COUNT", "count_fraction.b" = "CF: B")
  levels_per_stats <- list(
    count = c("a", "b", "c"),
    count_fraction = c("a", "b", "c")
  )

  # with levels_per_stats
  testthat::expect_equal(
    get_labels_from_stats(
      stats = c("count", "count_fraction"),
      labels_in = labels_custom,
      levels_per_stats = levels_per_stats
    ),
    c(
      "count.a" = "any A", "count.b" = "COUNT", "count.c" = "Lvl c:",
      "count_fraction.a" = "any A", "count_fraction.b" = "CF: B", "count_fraction.c" = "Lvl c:"
    )
  )
})

testthat::test_that("get_labels_from_stats works fine for cases with levels", {
  x_stats <- list(
    n = list(
      n = c(n = 5)
    ),
    count_fraction = list(
      a = c(count = 1.0, p = 0.2),
      b = c(count = 1.0, p = 0.2),
      c = c(count = 1.0, p = 0.2),
      d = c(count = 1.0, p = 0.2),
      e = c(count = 1.0, p = 0.2)
    ),
    a_zero = 0,
    a_null = NULL
  )
  .stats <- names(x_stats)
  .labels <- list("n" = "N=", "a" = "AAAA", "a_zero" = "A_ZERO")

  out <- get_labels_from_stats(.stats, .labels, levels_per_stats = lapply(x_stats, names))

  testthat::expect_equal(
    .unlist_keep_nulls(out),
    c(
      n.n = "N=",
      count_fraction.a = "AAAA",
      count_fraction.b = "b",
      count_fraction.c = "c",
      count_fraction.d = "d",
      count_fraction.e = "e",
      a_zero.a_zero = "A_ZERO",
      a_null.a_null = "a_null"
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
  result <- summary_formats() %>% unlist() # More compact fruition
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

testthat::test_that("get_stat_names works fine", {
  stat_results <- list(
    "n" = list("M" = c(n = 1), "F" = c(n = 2)),
    "count_fraction" = list("M" = c(n = 1, p = 0.2), "F" = c(n = 2, p = 0.1))
  )
  out <- get_stat_names(.unlist_keep_nulls(stat_results))

  testthat::expect_equal(out[1], list("n.M" = "n"))
  testthat::expect_equal(out[4], list("count_fraction.F" = c("n", "p")))

  out <- get_stat_names(stat_results, list("n" = "argh"))
  testthat::expect_equal(out[1], list("n" = "argh"))
})
