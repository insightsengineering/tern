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

testthat::test_that("custom labels can be set with row_labels for analyze_colvars", {
  lbl <- "some custom label"
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl
    )
  result <- build_table(lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Changing specifically all to custom labels
  lbl <- c("F" = "Female Statistic", "M" = "Male Statistic")
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl
    )
  result <- build_table(lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Partial change does not work (stop needs to be as it is more informative)
  lbl <- c("ASIAN" = "Asian Statistic",
           "BLACK OR AFRICAN AMERICAN",
           "Black or African American Statistic")
  lyt <- basic_table() %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl
    )
  testthat::expect_error(er <- build_table(lyt, df = adpp))

  # Error if there is no representation of the label
  lbl <- c("A" = "Female Statistic", "M" = "Male Statistic")
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl
    )
  testthat::expect_error(er <- build_table(lyt, df = adpp))
})

testthat::test_that("custom labels can be set with row_labels and summarize", {
  lbl <- "some custom label"
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl,
      do_summarize_row_groups = TRUE
    )
  result <- build_table(lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Changing specifically all to custom labels
  lbl <- c("F" = "Female Statistic", "M" = "Male Statistic")
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl,
      do_summarize_row_groups = TRUE
    )
  result <- build_table(lyt, df = adpp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Partial change does not work (stop needs to be as it is more informative)
  lbl <- c("ASIAN" = "Asian Statistic",
           "BLACK OR AFRICAN AMERICAN",
           "Black or African American Statistic")
  lyt <- basic_table() %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl,
      do_summarize_row_groups = TRUE
    )
  testthat::expect_error(er <- build_table(lyt, df = adpp))

  # Error if there is no representation of the label
  lbl <- c("A" = "Female Statistic", "M" = "Male Statistic")
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      row_labels = lbl,
      do_summarize_row_groups = TRUE
    )
  testthat::expect_error(er <- build_table(lyt, df = adpp))
})

testthat::test_that("summarize works with nested analyze", {
  rl_tmp <- unique(adpp$RACE)
  row_labels_for_analyze <- tolower(rl_tmp)
  names(row_labels_for_analyze) <- rl_tmp

  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      do_summarize_row_groups = TRUE,
      .indent_mods = 1L,
      row_labels = c("F" = "Female", "M" = "Male")
    ) %>%
    append_topleft("  Sex") %>%
    split_rows_by("RACE", child_labels = "hidden", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = "AGE",
      split_col_vars = FALSE,
      .indent_mods = 4L,
      row_labels = row_labels_for_analyze
    ) %>%
    append_topleft("          Ethnicity")

  tbl <- testthat::expect_silent(build_table(lyt, df = tern_ex_adpp, alt_counts_df = tern_ex_adsl))

  # It really works if I can sort it
  scorefun <- function(col) {
    function(tt) {
      cell_values(tt)[[col]]
    }
  }

  testthat::expect_snapshot(sort_at_path(tbl, c("SEX", "*", "RACE"), scorefun(1)))

  # More nesting
  lyt <- basic_table() %>%
    split_rows_by("SEX") %>%
    analyze_vars_in_cols(
      vars = "AGE",
      do_summarize_row_groups = TRUE
    ) %>%
    split_rows_by("RACE", child_labels = "hidden", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = "AGE",
      split_col_vars = FALSE,
      do_summarize_row_groups = TRUE
    ) %>%
    split_rows_by("ARM", child_labels = "hidden", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = "AGE",
      split_col_vars = FALSE,
      do_summarize_row_groups = TRUE
    ) %>%
    split_rows_by("STRATA1", child_labels = "hidden", split_fun = drop_split_levels) %>%
    analyze_vars_in_cols(
      vars = "AGE",
      split_col_vars = FALSE
    )

  tbl <- testthat::expect_silent(build_table(lyt, df = tern_ex_adpp))

  # Again sorting works
  tbl_sorted <- sort_at_path(tbl, c("SEX", "*", "RACE"), cont_n_onecol(1))
  tbl_sorted <- sort_at_path(tbl_sorted, c("SEX", "*", "RACE", "*", "ARM", "*", "STRATA1"), scorefun(1))

  testthat::expect_snapshot(tbl_sorted)
})
