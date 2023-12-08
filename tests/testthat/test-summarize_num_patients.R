testthat::test_that("s_num_patients works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, NA))
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients works as expected with empty input", {
  x <- as.character()
  result <- s_num_patients(x = x, labelstr = "", .N_col = 0)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients works as expected with unique_count_suffix = FALSE", {
  x <- as.character(c(1, 2, 1, 4, NA))
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, unique_count_suffix = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients_content works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = c(10, 15, 10, 17, 8)
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_num_patients works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    AGE = c(10, 15, 10, 17, 8, 11, 11, 19, 17)
  )

  # Check with both output
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID") %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with number of unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", .stats = c("unique")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with number of non-unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", .stats = c("nonunique")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with number of unique patients count only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", .stats = c("unique_count")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients count_by works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, 1))
  y <- c(6, 7, 8, 9, 6)
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, count_by = y)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients count_by with missing works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, NA))
  y <- c(6, 7, 8, 9, 6)
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, count_by = y)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients count_by with missing case 2 works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, 1))
  y <- c(6, 7, NA, 9, 6)
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, count_by = y)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients_content with count_by works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = c(10, 15, 10, 17, 8)
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID", count_by = "AGE")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients_content with count_by case 2 works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = c(10, 15, 11, 17, 8)
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID", count_by = "AGE")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_num_patients_content with count_by trivial cases, identical to without count_by", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, 9)),
    AGE = c(10, 15, 11, 17, 8)
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID", count_by = "USUBJID")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_num_patients with count_by works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    BY = c(10, 15, 10, 17, 8, 11, 11, 19, 17)
  )

  # Check with both output
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY") %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with number of unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY", .stats = c("unique")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with number of non-unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY", .stats = c("nonunique")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with number of unique patients count only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY", .stats = c("unique_count")) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "summarize_num_patients with count_by different combinations works as expected with healthy input",
  {
    df <- data.frame(
      USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
      ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
      BY = as.character(c(10, 15, 11, 17, 8, 11, 11, 19, 17))
    )

    # Check with both output
    result <- basic_table() %>%
      split_cols_by("ARM") %>%
      add_colcounts() %>%
      summarize_num_patients("USUBJID", count_by = "BY") %>%
      build_table(df)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("analyze_num_patients works well for pagination", {
  set.seed(1)
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    BY = as.character(c(10, 15, 11, 17, 8, 11, 11, 19, 17)),
    AE = paste0(sample(letters[5:6], 9, TRUE), " 1.1")
  )

  # Check a standard
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze_num_patients("USUBJID", .stats = c("unique", "nonunique")) %>%
    split_rows_by("AE",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients("USUBJID", .stats = c("unique", "nonunique")) %>%
    count_occurrences(vars = "BY", .indent_mods = -1L) %>%
    add_overall_col(label = "A+B") %>%
    build_table(df) %>%
    prune_table()

  # Sorting
  result <- result %>%
    sort_at_path(path = "AE", cont_n_onecol(2)) %>%
    sort_at_path(path = c("AE", "*", "BY"), score_occurrences_cols(col_indices = 2L))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Pagination tests (no repetition of the first lines)
  pag_result <- paginate_table(result, lpp = 10)
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]], with_spaces = FALSE, print_txt_to_copy = FALSE)[3:4, 1],
    c(
      "Number of patients with at least one event",
      "Number of events"
    )
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[3]], with_spaces = FALSE, print_txt_to_copy = FALSE)[6, 1],
    "  17"
  )
})

testthat::test_that("summarize_num_patients works as expected with risk difference column", {
  # One statistic
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    split_rows_by("AESOC", child_labels = "visible") %>%
    summarize_num_patients(
      "USUBJID",
      .stats = "unique",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple statistics
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    split_rows_by("AESOC", child_labels = "visible") %>%
    summarize_num_patients(
      "USUBJID",
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("analyze_num_patients works as expected with risk difference column", {
  # One statistic
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Any SAE"),
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple statistics
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    analyze_num_patients(
      vars = "USUBJID",
      .labels = c(unique = "Any SAE"),
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
