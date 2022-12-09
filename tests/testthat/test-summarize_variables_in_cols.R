adpp <- adpp_raw %>% h_pkparam_sort()

testthat::test_that("analyze_vars_in_cols works correctly", {
  lyt <- basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft") %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se"))

  result <- build_table(lyt = lyt, df = adpp)
  result_matrix <- to_string_matrix(result)
  expected <- structure(
    c(
      "ARM", "  SEX", "A: Drug X", "F", " ", "M", " ", "B: Placebo", "F",
      " ", "M", " ", "C: Combination", "F", " ", "M", " ", "", "n", "", "",
      "5214", "", "3630", "", "", "0", "", "0", "", "", "9240", "", "8184",
      "", "Mean", "", "", "32.8", "", "35.2", "", "", "NA", "", "NA", "", "",
      "35.2", "", "35.7", "", "SE", "", "", "0.1", "", "0.1", "", "", "NA",
      "", "NA", "", "", "0.1", "", "0.1"
    ),
    .Dim = c(17L, 4L)
  )
  testthat::expect_identical(result_matrix, expected)
})

testthat::test_that("analyze_vars_in_cols throws error when vars and .stats lengths differ in len", {
  lyt <- basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft")
  testthat::expect_error(
    lyt %>%
      analyze_vars_in_cols(vars = c("AGE", "AGE"), .stats = c("n", "mean", "se"))
  )
})
testthat::test_that("custom labels can be set with labelstr", {
  lbl <- "some custom label"
  lyt <- basic_table() %>%
    split_rows_by("SEX", child_labels = "hidden") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      labelstr = lbl
    )
  result <- build_table(lyt, df = adpp)
  testthat::expect_equal(to_string_matrix(result)[, 1][-1], rep(lbl, 2))
})
