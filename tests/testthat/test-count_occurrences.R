testthat::test_that("s_count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )

  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_occurrences drops non appearing levels by default", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df)
  testthat::expect_false("MHX" %in% c(names(result$count), names(result$count_fraction), names(result$fraction)))
})

testthat::test_that("s_count_occurrences keeps non appearing levels if requested", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df, drop = FALSE)
  testthat::expect_true("MHX" %in% names(result$count))
  testthat::expect_true("MHX" %in% names(result$count_fraction))
  testthat::expect_true("MHX" %in% names(result$fraction))
})

testthat::test_that("s_count_occurrences fails when it receives empty .df_row and drop = TRUE", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  df_sub <- df[df$USUBJID == "5", ]
  testthat::expect_error(s_count_occurrences(
    df = df_sub,
    .N_col = 4L,
    .df_row = df_sub,
    drop = TRUE
  ))
})

testthat::test_that("s_count_occurrences functions as expected when requesting different denominator", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )

  result <- s_count_occurrences(df = df, denom = "n", .N_col = 4L, .df_row = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_count_occurrences works with healthy input.", {
  options("width" = 100)

  # factor input
  df <- data.frame(
    id = factor(1:5),
    x = factor(c("a", "a", "b", "c", "a"))
  )
  result <- a_count_occurrences(
    df = df, .N_col = 10, .stats = get_stats("count_occurrences"), .var = "x", id = "id", .df_row = df
  )
  res_out <- testthat::expect_silent(result)

  # character input
  df <- data.frame(
    id = factor(1:5),
    x = c("a", "a", "b", "c", "a")
  )
  result <- a_count_occurrences(
    df = df, .N_col = 10, .stats = get_stats("count_occurrences"), .var = "x", id = "id", .df_row = df
  )
  res_out <- testthat::expect_silent(result)
})

testthat::test_that("a_count_occurrences works with custom input.", {
  options("width" = 100)

  df <- data.frame(
    id = factor(1:5),
    x = factor(c("a", "a", "b", "c", "a"), levels = c("a", "b", "c", "d"))
  )

  result <- a_count_occurrences(
    df = df, .df_row = df, .var = "x", id = "id", .N_col = 5,
    .stats = c("count", "count_fraction"), drop = FALSE,
    .formats = c(count_fraction = "xx (xx%)"),
    .labels = c(a = "Level: a", b = "LVL B", count.c = "Count of c", d = "Missing D"),
    .indent_mods = list(a = 1L, b = 2L, count.d = 3L)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4, 6, 6, 6, 7, 7, 8)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3", "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"),
      levels = c("MH1", "MH2", "MH3", "MH4", "MHX")
    ),
    ARM = rep(c("A", "B"), each = 6)
  )
  df_adsl <- data.frame(
    USUBJID = 1:9,
    ARM = rep(c("A", "B"), c(5, 4))
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences(vars = "MHDECOD")

  result <- lyt %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences functions as expected with label row specified", {
  df <- data.frame(
    USUBJID = as.character(c(1, 4, 4, 6, 6, 6, 7, 7, 8)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH1", "MH2", "MH1", "MH2"),
      levels = c("MH1", "MH2")
    )
  )
  df_adsl <- data.frame(
    USUBJID = 1:9
  )

  lyt <- basic_table() %>%
    count_occurrences(
      vars = "MHDECOD",
      var_labels = "MH Term",
      show_labels = "visible"
    )

  result <- lyt %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences works as expected with risk difference column", {
  # One statistic
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_occurrences(
      vars = "AEDECOD",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple statistics, different id var
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_occurrences(
      vars = "AEDECOD",
      riskdiff = TRUE,
      .stats = c("count", "count_fraction_fixed_dp", "fraction"),
      id = "SITEID"
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Nested column splits
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("SEX") %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_occurrences(
      vars = "AEDECOD",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4, 6, 6, 6, 7, 7, 8)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3", "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"),
      levels = c("MH1", "MH2", "MH3", "MH4", "MHX")
    ),
    ARM = rep(c("A", "B"), each = 6),
    SEX = c("F", "F", "M", "M", "M", "M", "F", "F", "F", "M", "M", "F")
  )
  df_adsl <- data.frame(
    USUBJID = 1:9,
    ARM = rep(c("A", "B"), c(5, 4))
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", child_labels = "visible") %>%
    add_colcounts() %>%
    summarize_occurrences(var = "MHDECOD")

  result <- lyt %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_occurrences works as expected with risk difference column", {
  # One statistic
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    split_rows_by("SEX", child_labels = "visible") %>%
    summarize_occurrences(
      var = "BMRKR2",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple statistics, different id var
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    split_rows_by("SEX", child_labels = "visible") %>%
    summarize_occurrences(
      var = "BMRKR2",
      riskdiff = TRUE,
      .stats = c("count", "count_fraction_fixed_dp", "fraction"),
      id = "SITEID"
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Nested column splits
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("STRATA1") %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    split_rows_by("SEX", child_labels = "visible") %>%
    summarize_occurrences(
      var = "BMRKR2",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple comparison groups
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", c("B: Placebo", "C: Combination"))) %>%
    split_rows_by("SEX", child_labels = "visible") %>%
    summarize_occurrences(
      var = "BMRKR2",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
