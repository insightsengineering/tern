adpp <- tern_ex_adpp %>% h_pkparam_sort()

testthat::test_that("analyze_vars_in_cols works correctly", {
  lyt <- basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft") %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se"))
  result <- build_table(lyt = lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
