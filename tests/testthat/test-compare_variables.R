testthat::test_that("s_compare works for numeric", {
  result <- testthat::expect_silent(s_compare(
    stats::rnorm(10, 5, 1),
    .ref_group = stats::rnorm(5, -5, 1),
    .in_ref_col = FALSE
  ))

  res <- testthat::expect_silent(names(result))
  testthat::expect_snapshot(res)
})

testthat::test_that("s_compare for numeric does not give p-value when not at least 2 values in each group", {
  result <- testthat::expect_silent(s_compare(
    stats::rnorm(10, 5, 1),
    .ref_group = 1,
    .in_ref_col = FALSE
  ))

  res <- testthat::expect_silent(result$pval)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_compare for factor works in usual case", {
  x <- factor(c("a", "a", "b", "c", "a"))
  y <- factor(c("a", "b", "c"))
  result <- testthat::expect_silent(s_compare(x = x, .ref_group = y, .in_ref_col = FALSE))

  res <- testthat::expect_silent(names(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(result$pval)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_compare for factor handles explicit NAs as expected", {
  x <- explicit_na(factor(c("a", "a", "b", "c", "a", NA, NA)))
  y <- explicit_na(factor(c("a", "b", "c", NA)))

  result_without_na <- testthat::expect_silent(s_compare(
    x = x,
    .ref_group = y,
    .in_ref_col = FALSE,
    na.rm = TRUE
  ))

  res <- testthat::expect_silent(result_without_na$pval)
  testthat::expect_snapshot(res)

  result_with_na <- testthat::expect_silent(s_compare(
    x = x,
    .ref_group = y,
    .in_ref_col = FALSE,
    na.rm = FALSE
  ))

  res <- testthat::expect_silent(result_with_na$pval)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_compare for character works as expected", {
  x <- c("a", "a", "b", "c", "a")
  y <- c("a", "b", "c")
  suppressWarnings(testthat::expect_warning(
    result <- s_compare(x, .ref_group = y, .in_ref_col = FALSE, .var = "x"),
    "automatically converting character variable x to factor"
  ))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_compare for logical works as expected", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  y <- c(FALSE, FALSE, TRUE)
  result <- testthat::expect_silent(s_compare(x, .ref_group = y, .in_ref_col = FALSE))

  res <- testthat::expect_silent(result$pval)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_compare for logical handles NAs as FALSE if not removed", {
  x <- c(NA, TRUE, FALSE)
  y <- c(NA, NA, NA, NA, FALSE)
  result <- testthat::expect_silent(s_compare(x, .ref_group = y, .in_ref_col = FALSE, na.rm = FALSE))
  expected <- s_compare(
    x = replace(x, is.na(x), FALSE),
    .ref_group = replace(y, is.na(y), FALSE),
    .in_ref_col = FALSE
  )
  testthat::expect_identical(result, expected, tolerance = 1e-4)
})

testthat::test_that("compare_vars works with default settings in rtables layout pipeline", {
  lyt <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM B", split_fun = ref_group_position("first")) %>%
    compare_vars(c("AGE", "SEX"))
  result <- build_table(lyt, tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("compare_vars works with custom settings", {
  lyt <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM C", split_fun = ref_group_position("first")) %>%
    compare_vars(
      c("AGE", "SEX"),
      .stats = c("mean_sd", "count_fraction", "pval"),
      .formats = c(mean_sd = "xx.x, xx.x", count_fraction = "xx.xx (xx.xx%)"),
      .labels = c(mean_sd = "Mean, SD")
    )
  result <- build_table(lyt, df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("compare_vars 'na_str' argument works as expected", {
  result <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM B", split_fun = ref_group_position("first")) %>%
    compare_vars("ARM", na_str = "-") %>%
    build_table(tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# Deprecated functions

testthat::test_that("create_afun_compare returns error message", {
  testthat::expect_warning(create_afun_compare()) # before It was not covered directly
})

testthat::test_that("a_compare returns correct output and warning message", {
  testthat::expect_warning({
    result <- a_compare(rnorm(10, 5, 1), .ref_group = rnorm(20, -5, 1), .stats = c("n", "pval"))
  })
  expected <- a_summary(rnorm(10, 5, 1), .ref_group = rnorm(20, -5, 1), .stats = c("n", "pval"), compare = TRUE)

  testthat::expect_equal(result, expected)
})
