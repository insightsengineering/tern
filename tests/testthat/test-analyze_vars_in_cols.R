adpp <- tern_ex_adpp %>% h_pkparam_sort()

testthat::test_that("analyze_vars_in_cols works correctly", {
  lyt <- basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft", child_labels = "hidden") %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se"))
  result <- build_table(lyt = lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # It fails if called multiple times with identical col split
  testthat::expect_error(basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    split_rows_by(var = "SEX", label_pos = "topleft") %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se")) %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se")))

  # It fails if called multiple times with identical col split on different lines
  testthat::expect_error(basic_table() %>%
    split_rows_by(var = "ARM", label_pos = "topleft") %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se")) %>%
    split_rows_by(var = "SEX", label_pos = "topleft") %>%
    analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se")))
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
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      labelstr = lbl
    )
  result <- build_table(lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("custom labels can be set for all lines", {
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      var = c("AGE"),
      do_row_groups = TRUE
    ) %>%
    split_rows_by("RACE", child_labels = "hidden", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = c("AGE"),
      split_col_vars = FALSE
    )

  tbl <- testthat::expect_silent(build_table(lyt, df = tern_ex_adpp, alt_counts_df = tern_ex_adsl))

  # It really works if I can sort it
  scorefun <- function(col) {
    function(tt) {
      cell_values(tt)[[col]]
    }
  }

  testthat::expect_snapshot(sort_at_path(tbl, c("SEX", "*", "RACE"), scorefun(1)))
})
