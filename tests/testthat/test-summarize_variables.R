library(dplyr)

testthat::test_that("control_summarize_vars works with customized parameters", {
  result <- control_summarize_vars(
    conf_level = 0.9,
    quantiles = c(0.1, 0.9)
  )
  expected <- list(
    conf_level = 0.9,
    quantiles = c(0.1, 0.9),
    quantile_type = 2
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("control_summarize_vars fails wrong inputs", {
  testthat::expect_error(control_summarize_vars(quantiles = c(25, 75)))
  testthat::expect_error(control_summarize_vars(conf_level = 95))
})

testthat::test_that("s_summary return NA for x length 0L", {
  x <- numeric()

  result <- s_summary(x)
  expected <- list(
    n = c(n = 0),
    sum = c(sum=NA_real_),
    mean = c(mean = NA_real_),
    sd = c(sd = NA_real_),
    se = c(se = NA_real_),
    mean_sd = c(mean = NA_real_, sd = NA_real_),
    mean_ci = formatters::with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Mean 95% CI"),
    mean_sei = formatters::with_label(c(mean_sei_lwr = NA_real_, mean_sei_upr = NA_real_), "Mean -/+ 1xSE"),
    mean_sdi = formatters::with_label(c(mean_sdi_lwr = NA_real_, mean_sdi_upr = NA_real_), "Mean -/+ 1xSD"),
    median = c(median = NA_real_),
    mad = c(mad = NA_real_),
    median_ci = formatters::with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI"),
    quantiles = formatters::with_label(c(quantile_0.25 = NA_real_, quantile_0.75 = NA_real_), "25% and 75%-ile"),
    iqr = c(iqr = NA_real_),
    range = c(min = NA_real_, max = NA_real_),
    min = c(min = NA_real_),
    max = c(max = NA_real_),
    cv = c(cv.sd = NA_real_),
    geom_mean = c(geom_mean = NA_real_),
    geom_mean_ci = formatters::with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Geometric Mean 95% CI"),
    geom_cv = c(geom_cv = NA_real_)
  )
  testthat::expect_equivalent(result, expected)
})

testthat::test_that("s_summary handles NA", {
  x <- c(NA_real_, 1)

  # With `na.rm = TRUE`.
  result <- s_summary(x)
  expected <- list(
    n = 1,
    sum  = c(sum=NA_real_),
    mean = c(mean = 1),
    sd = c(sd = NA_real_),
    se = c(se = NA_real_),
    mean_sd = c(mean = 1, sd = NA_real_),
    mean_ci = formatters::with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Mean 95% CI"),
    mean_sei = formatters::with_label(c(mean_sei_lwr = NA_real_, mean_sei_upr = NA_real_), "Mean -/+ 1xSE"),
    mean_sdi = formatters::with_label(c(mean_sdi_lwr = NA_real_, mean_sdi_upr = NA_real_), "Mean -/+ 1xSD"),
    median = c(median = 1),
    mad = c(mad = 0),
    median_ci = formatters::with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI"),
    quantiles = formatters::with_label(c(quantile_0.25 = 1, quantile_0.75 = 1), "25% and 75%-ile"),
    iqr = c(iqr = 0),
    range = c(min = 1, max = 1),
    min = c(min = 1),
    max = c(max = 1),
    cv = c(cv.sd = NA_real_),
    geom_mean = c(geom_mean = 1),
    geom_mean_ci = formatters::with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Geometric Mean 95% CI"),
    geom_cv = c(geom_cv = NA_real_)
  )
  testthat::expect_equivalent(result, expected)

  # With `na.rm = FALSE`.
  result <- s_summary(x, na.rm = FALSE)
  expected <- list(
    n = 2,
    sum = c(sum = NA_real_),
    mean = c(mean = NA_real_),
    sd = c(sd = NA_real_),
    se = c(se = NA_real_),
    mean_sd = c(mean = NA_real_, sd = NA_real_),
    mean_ci = formatters::with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Mean 95% CI"),
    mean_sei = formatters::with_label(c(mean_sei_lwr = NA_real_, mean_sei_upr = NA_real_), "Mean -/+ 1xSE"),
    mean_sdi = formatters::with_label(c(mean_sdi_lwr = NA_real_, mean_sdi_upr = NA_real_), "Mean -/+ 1xSD"),
    median = c(median = NA_real_),
    mad = c(mad = NA_real_),
    median_ci = formatters::with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI"),
    quantiles = formatters::with_label(c(quantile_0.25 = NA_real_, quantile_0.75 = NA_real_), "25% and 75%-ile"),
    iqr = c(iqr = NA_real_),
    range = c(min = NA_real_, max = NA_real_),
    min = c(min = NA_real_),
    max = c(max = NA_real_),
    cv = c(cv.sd = NA_real_),
    geom_mean = c(geom_mean = NA_real_),
    geom_mean_ci = formatters::with_label(c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_), "Geometric Mean 95% CI"),
    geom_cv = c(geom_cv = NA_real_)
  )
  testthat::expect_equivalent(result, expected)
})

testthat::test_that("s_summary returns right results for n = 2", {
  x <- c(NA_real_, 1, 2)
  result <- s_summary(x)
  expected <- list(
    n = 2,
    sum = c(sum = 3),
    mean = c(mean = 1.5),
    sd = c(sd = 0.7071068),
    se = c(se = 0.5),
    mean_sd = c(mean = 1.5, sd = 0.7071068),
    mean_ci = formatters::with_label(c(mean_ci_lwr = -4.853102, mean_ci_upr = 7.853102), "Mean 95% CI"),
    mean_sei = formatters::with_label(c(mean_sei_lwr = 1, mean_sei_upr = 2), "Mean -/+ 1xSE"),
    mean_sdi = formatters::with_label(c(mean_sdi_lwr = 0.7928932, mean_sdi_upr = 2.2071068), "Mean -/+ 1xSD"),
    median = c(median = 1.5),
    mad = c(mad = 0),
    median_ci = formatters::with_label(c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_), "Median 95% CI"),
    quantiles = formatters::with_label(c(quantile_0.25 = 1, quantile_0.75 = 2), "25% and 75%-ile"),
    iqr = c(iqr = 1),
    range = c(min = 1, max = 2),
    min = c(min = 1),
    max = c(max = 2),
    cv = c(cv.sd = 47.14045),
    geom_mean = c(geom_mean = 1.414214),
    geom_mean_ci = formatters::with_label(
      c(mean_ci_lwr = 0.01729978, mean_ci_upr = 115.60839614),
      "Geometric Mean 95% CI"
    ),
    geom_cv = c(geom_cv = 52.10922)
  )
  testthat::expect_equivalent(result, expected, tolerance = .00001)
})

testthat::test_that("s_summary returns right results for n = 8", {
  x <- c(NA_real_, 1, 2, 5, 6, 7, 8, 9, 10)
  result <- s_summary(x)
  expected <- list(
    n = 8,
    sum = c(sum = 48),
    mean = c(mean = 6),
    sd = c(sd = 3.207135),
    se = c(se = 1.133893),
    mean_sd = c(mean = 6, sd = 3.207135),
    mean_ci = formatters::with_label(c(mean_ci_lwr = 3.318768, mean_ci_upr = 8.681232), "Mean 95% CI"),
    mean_sei = formatters::with_label(c(mean_sei_lwr = 4.866107, mean_sei_upr = 7.133893), "Mean -/+ 1xSE"),
    mean_sdi = formatters::with_label(c(mean_sdi_lwr = 2.792865, mean_sdi_upr = 9.207135), "Mean -/+ 1xSD"),
    median = c(median = 6.5),
    mad = c(mad = 0),
    median_ci = formatters::with_label(c(median_ci_lwr = 1, median_ci_upr = 10), "Median 95% CI"),
    quantiles = formatters::with_label(c(quantile_0.25 = 3.5, quantile_0.75 = 8.5), "25% and 75%-ile"),
    iqr = c(iqr = 5),
    range = c(min = 1, max = 10),
    min = c(min = 1),
    max = c(max = 10),
    cv = c(cv.sd = 53.45225),
    geom_mean = c(geom_mean = 4.842534),
    geom_mean_ci = formatters::with_label(c(mean_ci_lwr = 2.456211, mean_ci_upr = 9.547283), "Geometric Mean 95% CI"),
    geom_cv = c(geom_cv = 96.61307)
  )
  testthat::expect_equivalent(result, expected, tolerance = .00001)
})

testthat::test_that("s_summary works with factors", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown"))

  result <- s_summary(x)
  expected <- list(
    n = 9L,
    count = list(
      Female = 2L,
      Male = 3L,
      Unknown = 4L
    ),
    count_fraction = list(
      Female = c(2, 2 / 9),
      Male = c(3, 3 / 9),
      Unknown = c(4, 4 / 9)
    ),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary fails with factors that have no levels or have empty string levels", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown", ""))
  testthat::expect_error(
    s_summary(x),
    "x is not a valid factor, please check the factor levels (no empty strings allowed)",
    fixed = TRUE
  )

  x <- factor()
  testthat::expect_error(
    s_summary(x),
    "x is not a valid factor, please check the factor levels (no empty strings allowed)",
    fixed = TRUE
  )
})

testthat::test_that("s_summary fails when factors have NA levels", {
  x <- factor(c("Female", "Male", "Female", "Male", "Unknown", "Unknown", NA))
  testthat::expect_error(
    s_summary(x, na.rm = FALSE),
    "NA in x has not been conveyed to na_level, please use explicit factor levels.",
    fixed = TRUE
  )
})

testthat::test_that("s_summary works with factors with NA values handled and correctly removes them by default", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown", NA))
  x <- explicit_na(x)

  result <- s_summary(x)
  expected <- list(
    n = 9L,
    count = list(
      Female = 2L,
      Male = 3L,
      Unknown = 4L
    ),
    count_fraction = list(
      Female = c(2, 2 / 9),
      Male = c(3, 3 / 9),
      Unknown = c(4, 4 / 9)
    ),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with length 0 factors that have levels", {
  x <- factor(levels = c("a", "b", "c"))

  result <- s_summary(x)
  expected <- list(
    n = 0L,
    count = list(
      a = 0L,
      b = 0L,
      c = 0L
    ),
    count_fraction = list(
      a = c(0L, 0),
      b = c(0L, 0),
      c = c(0L, 0)
    ),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with factors and different denominator choices", {
  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown"))

  result <- s_summary(x, denom = "N_row", .N_row = 20)
  expected <- list(
    n = 9L,
    count = list(
      Female = 2L,
      Male = 3L,
      Unknown = 4L
    ),
    count_fraction = list(
      Female = c(2, 2 / 20),
      Male = c(3, 3 / 20),
      Unknown = c(4, 4 / 20)
    ),
    n_blq = 0L
  )
  testthat::expect_identical(result, expected)

  result <- s_summary(x, denom = "N_col", .N_col = 30)
  expected <- list(
    n = 9L,
    count = list(
      Female = 2L,
      Male = 3L,
      Unknown = 4L
    ),
    count_fraction = list(
      Female = c(2, 2 / 30),
      Male = c(3, 3 / 30),
      Unknown = c(4, 4 / 30)
    ),
    n_blq = 0L
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with characters by converting to character", {
  x <- c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown")

  result <- testthat::expect_warning(s_summary(x, denom = "N_row", .N_row = 20, .var = "SEX"))
  expected <- s_summary(factor(x), denom = "N_row", .N_row = 20)

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with characters by converting to character and handling empty strings", {
  x <- c("Female", "Male", "Female", "Male", "Male", "", "Unknown", "Unknown", "Unknown", "Unknown")

  result <- testthat::expect_warning(s_summary(x, .var = "foo", na.rm = FALSE, denom = "N_row", .N_row = 10))
  expected <- list(
    n = 10L,
    count = list(
      Female = 2L,
      Male = 3L,
      Unknown = 4L,
      "<Missing>" = 1L
    ),
    count_fraction = list(
      Female = c(2, 2 / 10),
      Male = c(3, 3 / 10),
      Unknown = c(4, 4 / 10),
      "<Missing>" = c(1, 1 / 10)
    ),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary does not work for length 0 character vectors", {
  x <- character()
  testthat::expect_warning(testthat::expect_error(s_summary(x, denom = "n", .var = "foo")))
})

testthat::test_that("s_summary works with logical vectors", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)

  result <- s_summary(x)
  expected <- list(
    n = 6L,
    count = 4L,
    count_fraction = c(4, 4 / 6),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with logical vectors and by default removes NA", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, NA, NA)

  result <- s_summary(x)
  expected <- list(
    n = 6L,
    count = 4L,
    count_fraction = c(4, 4 / 6),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("s_summary works with logical vectors and by if requested does not remove NA from n", {
  x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, NA, NA)

  result <- s_summary(x, na.rm = FALSE)
  expected <- list(
    n = 8L,
    count = 4L,
    count_fraction = c(4, 4 / 8),
    n_blq = 0L
  )

  testthat::expect_identical(result, expected)
})

testthat::test_that("create_afun_summary creates an `afun` that works", {
  afun <- create_afun_summary(
    .stats = c("n", "count_fraction", "median", "range", "mean_ci"),
    .formats = c(median = "xx."),
    .labels = c(median = "My median"),
    .indent_mods = c(median = 1L)
  )
  dta_test <- data.frame(
    USUBJID = rep(1:6, each = 3),
    PARAMCD = rep("lab", 6 * 3),
    AVISIT = rep(paste0("V", 1:3), 6),
    ARM = rep(LETTERS[1:3], rep(6, 3)),
    AVAL = c(9:1, rep(NA, 9)),
    stringsAsFactors = TRUE
  )

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    analyze(vars = c("AVAL", "ARM"), afun = afun)

  result <- build_table(l, df = dta_test)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "V1", "AVAL", "n", "My median", "Min - Max",
      "Mean 95% CI", "ARM", "n", "A", "B", "C", "V2", "AVAL", "n",
      "My median", "Min - Max", "Mean 95% CI", "ARM", "n", "A", "B",
      "C", "V3", "AVAL", "n", "My median", "Min - Max", "Mean 95% CI",
      "ARM", "n", "A", "B", "C", "A", "", "", "2", "8", "6.0 - 9.0", "(-11.56, 26.56)",
      "", "2", "2 (100%)", "0", "0", "", "", "2", "6", "5.0 - 8.0", "(-12.56, 25.56)",
      "", "2", "2 (100%)", "0", "0", "", "", "2", "6", "4.0 - 7.0", "(-13.56, 24.56)",
      "", "2", "2 (100%)", "0", "0", "B", "", "", "1", "3", "3.0 - 3.0",
      "(NA, NA)", "", "2", "0", "2 (100%)", "0", "", "", "1", "2",
      "2.0 - 2.0", "(NA, NA)", "", "2", "0", "2 (100%)", "0", "", "", "1",
      "1", "1.0 - 1.0", "(NA, NA)", "", "2", "0", "2 (100%)", "0", "C",
      "", "", "0", "NA", "NA - NA", "(NA, NA)", "", "2", "0", "0",
      "2 (100%)", "", "", "0", "NA", "NA - NA", "(NA, NA)", "", "2",
      "0", "0", "2 (100%)", "", "", "0", "NA", "NA - NA", "(NA, NA)",
      "", "2", "0", "0", "2 (100%)"
    ),
    .Dim = c(34L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("`summarize_vars` works with healthy input, default `na.rm = TRUE`.", {
  dta_test <- data.frame(AVAL = c(1:4, NA, NA))

  l <- basic_table() %>%
    summarize_vars(vars = "AVAL")
  result <- build_table(l, df = dta_test)

  expected <- structure(
    c(
      "", "n", "Mean (SD)", "Median", "Min - Max", "all obs",
      "4", "2.5 (1.3)", "2.5", "1.0 - 4.0"
    ),
    .Dim = c(5L, 2L)
  )

  testthat::expect_identical(to_string_matrix(result), expected)
})

testthat::test_that("`summarize_vars` works with healthy input, and control function.", {
  dta_test <- data.frame(AVAL = c(1:9))

  l <- basic_table() %>%
    summarize_vars(
      vars = "AVAL",
      control = control_summarize_vars(quantiles = c(0.1, 0.9), conf_level = 0.9),
      .stats = c("n", "mean_sd", "mean_ci", "quantiles")
    )
  result <- build_table(l, df = dta_test)

  expected <- structure(
    c(
      "", "n", "Mean (SD)", "Mean 90% CI", "10% and 90%-ile", "all obs",
      "9", "5.0 (2.7)", "(3.30, 6.70)", "1.0 - 9.0"
    ),
    .Dim = c(5L, 2L)
  )

  testthat::expect_identical(to_string_matrix(result), expected)
})

testthat::test_that("`summarize_vars` works with healthy input, alternative `na.rm = FALSE`", {
  dta_test <- data.frame(AVAL = c(1:4, NA, NA))

  l <- basic_table() %>%
    summarize_vars(vars = "AVAL", na.rm = FALSE)
  result <- build_table(l, df = dta_test)

  expected <- structure(
    c(
      "", "n", "Mean (SD)", "Median", "Min - Max", "all obs",
      "6", "NA (NA)", "NA", "NA - NA"
    ),
    .Dim = c(5L, 2L)
  )

  testthat::expect_identical(to_string_matrix(result), expected)
})

testthat::test_that("`summarize_vars` works with healthy factor input", {
  dta <- data.frame(foo = factor(c("a", "b", "a")))

  result <- basic_table() %>%
    summarize_vars(vars = "foo") %>%
    build_table(dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "n", "a", "b", "all obs", "3", "2 (66.7%)", "1 (33.3%)"),
    .Dim = c(4L, 2L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("`summarize_vars` works with healthy factor input, alternative `na.rm = FALSE`", {
  dta <- data.frame(foo = factor(c("a", NA, "b", "a", NA)))
  dta <- df_explicit_na(dta)

  result <- basic_table() %>%
    summarize_vars(vars = "foo", na.rm = FALSE) %>%
    build_table(dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "n", "a", "b", "<Missing>", "all obs", "5", "2 (40%)", "1 (20%)", "2 (40%)"),
    .Dim = c(5L, 2L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("`summarize_vars` works with factors and different denominators", {
  start <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts()

  result_n <- start %>%
    summarize_vars("RACE", denom = "n") %>%
    build_table(DM)

  result_ncol <- start %>%
    summarize_vars("RACE", denom = "N_col") %>%
    build_table(DM)

  result <- start %>%
    summarize_vars("RACE", denom = "N_row") %>%
    build_table(DM)

  testthat::expect_false(identical(result_n, result_ncol))
  testthat::expect_false(identical(result_n, result))
  testthat::expect_false(identical(result_ncol, result))

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "F (N=187)", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "OTHER", "UNKNOWN", "M (N=169)", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "OTHER", "UNKNOWN", "A: Drug X", "(N=121)", "", "70", "44 (23.5%)",
      "18 (9.6%)", "8 (4.3%)", "0", "0", "0", "0",
      "0", "", "51", "35 (20.7%)", "10 (5.9%)", "6 (3.6%)", "0",
      "0", "0", "0", "0", "B: Placebo", "(N=106)",
      "", "56", "37 (19.8%)", "12 (6.4%)", "7 (3.7%)", "0", "0",
      "0", "0", "0", "", "50", "31 (18.3%)", "12 (7.1%)",
      "7 (4.1%)", "0", "0", "0", "0", "0",
      "C: Combination", "(N=129)", "", "61", "40 (21.4%)", "13 (7%)",
      "8 (4.3%)", "0", "0", "0", "0", "0",
      "", "68", "44 (26%)", "14 (8.3%)", "10 (5.9%)", "0", "0",
      "0", "0", "0"
    ),
    .Dim = c(22L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_vars works in demographic table example", {
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    summarize_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM)
  testthat::expect_silent(to_string_matrix(result))
})

testthat::test_that("`summarize_vars` works with character input and gives the same result as with factor", {
  dta <- data.frame(
    foo = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )

  l <- basic_table() %>%
    summarize_vars(vars = "foo")
  result <- testthat::expect_warning(build_table(l, dta))

  dta_factor <- dta %>%
    dplyr::mutate(foo = factor(foo))
  expected <- build_table(l, dta_factor)

  testthat::expect_identical(result, expected)
})

testthat::test_that("`summarize_vars` does not work with sparse character input due to missing statistics", {
  dta <- data.frame(
    foo = c("a", "b", "a"),
    boo = c("e", "e", "f"),
    stringsAsFactors = FALSE
  )

  l <- basic_table() %>%
    split_cols_by("boo") %>%
    summarize_vars(vars = "foo")
  testthat::expect_error(testthat::expect_warning(build_table(l, dta)))

  # But when converting to factor, it works because we keep the levels information across columns.
  dta_factor <- dta %>%
    dplyr::mutate(foo = factor(foo))
  testthat::expect_silent(build_table(l, dta_factor))
})

testthat::test_that("`summarize_vars` works with logical input", {
  dta <- data.frame(
    boo = c(TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  result <- basic_table() %>%
    summarize_vars(vars = "boo") %>%
    build_table(dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "n", "count_fraction", "all obs", "5", "3 (60%)"),
    .Dim = 3:2
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("`summarize_vars` works with empty named numeric variables", {
  dta <- tibble::tibble(
    foo = factor(c("a", "a", "b", "b", "c", "c"), levels = c("a", "b", "c")),
    boo = 1:6
  )
  dta <- dta %>% dplyr::filter(foo != "a")
  names(dta$boo) <- dta$foo

  result <- basic_table() %>%
    split_cols_by("foo") %>%
    summarize_vars(vars = "boo") %>%
    build_table(dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "n", "Mean (SD)", "Median", "Min - Max", "a", "0", "NA (NA)", "NA", "NA - NA",
      "b", "2", "3.5 (0.7)", "3.5", "3.0 - 4.0", "c", "2", "5.5 (0.7)", "5.5", "5.0 - 6.0"
    ),
    .Dim = c(5:4)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
