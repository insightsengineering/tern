adpp <- adpp_raw %>% h_pkparam_sort()

testthat::test_that("summary_in_cols works with numeric input", {
  result <- summary_in_cols(adpp$AGE)
  expected_stats <- c(
    "n", "sum", "mean", "sd", "se", "mean_sd", "mean_se", "mean_ci", "mean_sei", "mean_sdi", "mean_pval", "median",
    "mad", "median_ci", "quantiles", "iqr", "range", "min", "max", "cv", "geom_mean", "geom_mean_ci", "geom_cv"
  )
  testthat::expect_identical(names(result), expected_stats)
})

testthat::test_that("summary_in_cols works with factor input", {
  result <- summary_in_cols(adpp$SEX)
  expected_stats <- c("n", "count", "count_fraction", "n_blq")
  testthat::expect_identical(names(result), expected_stats)
})

testthat::test_that("summary_in_cols works with character input", {
  adpp$REGIMEN <- as.character(adpp$REGIMEN)
  result <- summary_in_cols(adpp$REGIMEN)
  expected_stats <- c("n", "count", "count_fraction", "n_blq")
  testthat::expect_identical(names(result), expected_stats)
})

testthat::test_that("summary_in_cols works with logical input", {
  adpp$BMEASIFL <- adpp$BMEASIFL == "Y"
  result <- summary_in_cols(adpp$BMEASIFL)
  expected_stats <- c("n", "count", "count_fraction", "n_blq")
  testthat::expect_identical(names(result), expected_stats)
})

testthat::test_that("summarize_vars_in_cols works correctly", {
  lyt <- basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft") %>%
    summarize_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se"))

  result <- build_table(lyt = lyt, df = adpp)
  result_matrix <- to_string_matrix(result)
  expected <- matrix(
    c(
      "ARM", "  SEX", "A: Drug X", "F", " ", "M", " ", "B: Placebo", "F",
      " ", "M", " ", "C: Combination", "F", " ", "M", " ", "", "n", "", "",
      "5214", "", "3630", "", "", "0", "", "0", "", "", "9240", "", "8184",
      "", "Mean", "", "", "32.8", "", "35.2", "", "", "NA", "", "NA", "", "",
      "35.2", "", "35.7", "", "SE", "", "", "0.1", "", "0.1", "", "", "NA",
      "", "NA", "", "", "0.1", "", "0.1"
    ),
    ncol = 4
  )
  testthat::expect_identical(result_matrix, expected)
})

testthat::test_that("summarize_vars_in_cols throws error when vars and .stats lengths differ in len", {
  lyt <- basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft")
  testthat::expect_error(
    lyt %>%
    summarize_vars_in_cols(vars = c("AGE", "AGE"), .stats = c("n", "mean", "se"))
    )
})
