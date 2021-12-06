test_that("s_compare works for numeric", {
  result <- expect_silent(s_compare(
    rnorm(10, 5, 1),
    .ref_group = rnorm(5, -5, 1),
    .in_ref_col = FALSE
  ))
  expect_named(result, c(
    "n", "mean", "sd", "mean_sd", "mean_ci", "mean_sei", "mean_sdi",
    "median", "mad", "median_ci", "quantiles", "iqr", "range",
    "min", "max", "cv", "geom_mean", "geom_cv", "pval"
  ))
})

test_that("s_compare for numeric does not give p-value when not at least 2 values in each group", {
  result <- expect_silent(s_compare(
    rnorm(10, 5, 1),
    .ref_group = 1,
    .in_ref_col = FALSE
  ))
  expect_identical(result$pval, character())
})

test_that("s_compare for factor works in usual case", {
  x <- factor(c("a", "a", "b", "c", "a"))
  y <- factor(c("a", "b", "c"))
  result <- expect_silent(s_compare(x = x, .ref_group = y, .in_ref_col = FALSE))
  expect_named(result, c("n", "count", "count_fraction", "pval"))
  expect_equal(result$pval, 0.7659, tol = 1e-4)
})

test_that("s_compare for factor handles explicit NAs as expected", {
  x <- explicit_na(factor(c("a", "a", "b", "c", "a", NA, NA)))
  y <- explicit_na(factor(c("a", "b", "c", NA)))

  result_without_na <- expect_silent(s_compare(
    x = x,
    .ref_group = y,
    .in_ref_col = FALSE,
    na.rm = TRUE
  ))
  expect_equal(result_without_na$pval, 0.7659, tol = 1e-4)

  result_with_na <- expect_silent(s_compare(
    x = x,
    .ref_group = y,
    .in_ref_col = FALSE,
    na.rm = FALSE
  ))
  expect_equal(result_with_na$pval, 0.9063, tol = 1e-4)
})

test_that("s_compare for character works as expected", {
  x <- c("a", "a", "b", "c", "a")
  y <- c("a", "b", "c")
  result <- expect_warning(
    s_compare(x, .ref_group = y, .in_ref_col = FALSE, .var = "x"),
    "automatically converting character variable x to factor"
  )
  expected <- list(
    n = 5L,
    count = list(a = 3L, b = 1L, c = 1L),
    count_fraction = list(a = c(3, 0.6), b = c(1, 0.2), c = c(1, 0.2)),
    pval = 0.7659
  )
  expect_equal(result, expected, tol = 1e-4)
})

test_that("s_compare for logical works as expected", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  y <- c(FALSE, FALSE, TRUE)
  result <- expect_silent(s_compare(x, .ref_group = y, .in_ref_col = FALSE))
  expect_equal(result$pval, 0.2702, tol = 1e-4)
})

test_that("s_compare for logical handles NAs as FALSE if not removed", {
  x <- c(NA, TRUE, FALSE)
  y <- c(NA, NA, NA, NA, FALSE)
  result <- expect_silent(s_compare(x, .ref_group = y, .in_ref_col = FALSE, na.rm = FALSE))
  expected <- s_compare(
    x = replace(x, is.na(x), FALSE),
    .ref_group = replace(y, is.na(y), FALSE),
    .in_ref_col = FALSE
  )
  expect_identical(result, expected)
})

test_that("compare_vars works with default settings in rtables layout pipeline", {
  lyt <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM B") %>%
    compare_vars(c("AGE", "SEX"))
  result <- build_table(lyt, ex_adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "AGE", "n", "Mean (SD)", "p-value (t-test)",
      "SEX", "n", "F", "M", "U", "UNDIFFERENTIATED", "p-value (chi-squared test)",
      "ARM B", "", "134", "35.4 (7.9)", "", "", "134", "77 (57.5%)",
      "55 (41%)", "2 (1.5%)", "0", "", "ARM A", "", "134", "33.8 (6.6)",
      "0.0616", "", "134", "79 (59%)", "51 (38.1%)", "3 (2.2%)", "1 (0.7%)",
      "0.7110", "ARM C", "", "132", "35.4 (7.7)", "0.9992", "", "132",
      "66 (50%)", "60 (45.5%)", "4 (3%)", "2 (1.5%)", "0.2939"),
    .Dim = c(12L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("compare_vars works with custom settings", {
  lyt <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM C") %>%
    compare_vars(
      c("AGE", "SEX"),
      .stats = c("mean_sd", "count_fraction", "pval"),
      .formats = c(mean_sd = "xx.x, xx.x", count_fraction = "xx.xx (xx.xx%)"),
      .labels = c(mean_sd = "Mean, SD")
    )
  result <- build_table(lyt, df = ex_adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "AGE", "Mean, SD", "p-value (t-test)", "SEX",
      "F", "M", "U", "UNDIFFERENTIATED", "p-value (chi-squared test)",
      "ARM C", "", "35.4, 7.7", "", "", "66 (50%)", "60 (45.45%)",
      "4 (3.03%)", "2 (1.52%)", "", "ARM A", "", "33.8, 6.6", "0.0595",
      "", "79 (58.96%)", "51 (38.06%)", "3 (2.24%)", "1 (0.75%)", "0.5018",
      "ARM B", "", "35.4, 7.9", "0.9992", "", "77 (57.46%)", "55 (41.04%)",
      "2 (1.49%)", "0 (0%)", "0.2939"),
    .Dim = c(10L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
