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
  testthat::expect_error(
    basic_table() %>%
      split_rows_by(var = "ARM", label_pos = "topleft") %>%
      split_rows_by(var = "SEX", label_pos = "topleft") %>%
      analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se")) %>%
      analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se"))
  )

  # It fails if called multiple times with identical col split on different lines
  testthat::expect_error(
    basic_table() %>%
      split_rows_by(var = "ARM", label_pos = "topleft") %>%
      analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se")) %>%
      split_rows_by(var = "SEX", label_pos = "topleft") %>%
      analyze_vars_in_cols(vars = "AGE", .stats = c("n", "mean", "se"))
  )
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
  lbl <- c(
    "ASIAN" = "Asian Statistic",
    "BLACK OR AFRICAN AMERICAN",
    "Black or African American Statistic"
  )
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
  lbl <- c(
    "ASIAN" = "Asian Statistic",
    "BLACK OR AFRICAN AMERICAN",
    "Black or African American Statistic"
  )
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

testthat::test_that("analyze_vars_in_cols works well with categorical data", {
  # Regression test after #1013
  adpp <- tern_ex_adpp %>% h_pkparam_sort()

  lyt <- basic_table() %>%
    split_rows_by(var = "STRATA1", label_pos = "topleft") %>%
    split_rows_by(
      var = "SEX",
      label_pos = "topleft",
      child_label = "hidden"
    ) %>%
    # split_cols_by("STRATA1") %>%
    analyze_vars_in_cols(
      vars = "ARM",
      .stats = c("n", "count_fraction"),
      .labels = c("count_fraction" = "argh")
    )
  testthat::expect_error(
    result <- build_table(lyt = lyt, df = adpp),
    "The analyzed column produced more than one category of results."
  )

  lyt <- basic_table() %>%
    split_rows_by(var = "STRATA1", label_pos = "topleft") %>%
    split_rows_by(
      var = "SEX",
      label_pos = "topleft",
      child_label = "hidden"
    ) %>%
    split_cols_by("ARM") %>%
    analyze_vars_in_cols(
      vars = "counter",
      .stats = c("count_fraction"),
      .labels = c("count_fraction" = " ")
    )
  testthat::expect_snapshot(build_table(
    lyt = lyt,
    df = adpp %>% mutate(counter = factor("n"))
  ))

  # Alternative to discuss (xxx)
  count_fraction <- function(x, .spl_context, .N_col) { # nolint
    ret_list <- as.list(table(x))
    if (length(x) == 0) {
      aform <- "xx"
    } else {
      ret_list <- lapply(ret_list, function(i) {
        c(i, i / .N_col)
      })
      aform <- "xx. (xx.%)"
    }
    in_rows(.list = ret_list, .formats = aform)
  }

  testthat::expect_snapshot(
    basic_table(show_colcounts = TRUE) %>%
      split_rows_by(var = "STRATA1", label_pos = "topleft") %>%
      split_cols_by("ARM") %>%
      analyze(vars = "SEX", afun = count_fraction) %>%
      append_topleft("  SEX") %>%
      build_table(adpp)
  )
})

testthat::test_that("analyze_vars_in_cols works with imputation rule", {
  set.seed(1)
  df <- data.frame(
    ARM = with_label(rep("A: Drug X", 162), "Arm"),
    AVAL = runif(162, 0, 100),
    AVALCAT1 = as.factor(sample(c(1, "BLQ"), 162, replace = TRUE)),
    AVALCAT2 = as.factor(sample(c(1, "BLQ"), 162, replace = TRUE, prob = c(0.25, 0.75))),
    VISIT = with_label(as.factor(rep(c(rep("Day 1", 5), rep("Day 2", 4)), 18)), "Visit"),
    NFRLT = with_label(as.factor(rep(c(0, seq(0, 42, 6)), 18)), "Nominal Time")
  )

  # 1/3 imputation rule
  lyt <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "NFRLT",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) %>%
    analyze_vars_in_cols(
      vars = c("AVAL", "AVALCAT1", rep("AVAL", 5)),
      .stats = c("n", "n_blq", "mean", "sd", "geom_mean", "min", "max"),
      .labels = c(
        n = "n", n_blq = "Number of BLQs", mean = "Mean", sd = "SD",
        geom_mean = "Geometric Mean", min = "Minimum", max = "Maximum"
      ),
      imp_rule = "1/3"
    )

  result <- build_table(lyt = lyt, df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  df$NFRLT <- as.character(df$NFRLT)

  # 1/3 imputation rule, custom avalcat_var
  lyt <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "NFRLT",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) %>%
    analyze_vars_in_cols(
      vars = c("AVAL", "AVALCAT2", rep("AVAL", 5)),
      .stats = c("n", "n_blq", "mean", "sd", "geom_mean", "min", "max"),
      .labels = c(
        n = "n", n_blq = "Number of BLQs", mean = "Mean", sd = "SD",
        geom_mean = "Geometric Mean", min = "Minimum", max = "Maximum"
      ),
      imp_rule = "1/3",
      avalcat_var = "AVALCAT2"
    )

  result <- build_table(lyt = lyt, df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # 1/2 imputation rule
  lyt <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "NFRLT",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) %>%
    analyze_vars_in_cols(
      vars = c("AVAL", "AVALCAT1", rep("AVAL", 5)),
      .stats = c("n", "n_blq", "mean", "sd", "geom_mean", "min", "max"),
      .labels = c(
        n = "n", n_blq = "Number of BLQs", mean = "Mean", sd = "SD",
        geom_mean = "Geometric Mean", min = "Minimum", max = "Maximum"
      ),
      imp_rule = "1/2"
    )

  result <- build_table(lyt = lyt, df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("analyze_vars_in_cols works with caching", {
  set.seed(1)
  df <- data.frame(
    ARM = with_label(rep("A: Drug X", 162), "Arm"),
    AVAL = runif(162, 0, 100),
    AVALCAT1 = as.factor(sample(c(1, "BLQ"), 162, replace = TRUE)),
    VISIT = with_label(as.factor(rep(c(rep("Day 1", 5), rep("Day 2", 4)), 18)), "Visit"),
    NFRLT = with_label(as.factor(rep(c(0, seq(0, 42, 6)), 18)), "Nominal Time")
  )

  lyt <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      var = "NFRLT",
      split_fun = drop_split_levels,
      child_labels = "hidden"
    ) %>%
    analyze_vars_in_cols(
      vars = c("AVAL", "AVALCAT1", rep("AVAL", 5)),
      .stats = c("n", "n_blq", "mean", "sd", "geom_mean", "min", "max"),
      .labels = c(
        n = "n", n_blq = "Number of BLQs", mean = "Mean", sd = "SD",
        geom_mean = "Geometric Mean", min = "Minimum", max = "Maximum"
      ),
      cache = TRUE
    )

  result <- build_table(lyt = lyt, df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
