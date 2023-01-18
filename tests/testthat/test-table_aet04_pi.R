full_table_aet04_pi <- function(adsl, adae_max) {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )

  col_counts <- rep(table(adsl$ACTARM), each = length(grade_groups))
  basic_table() %>%
    split_cols_by("ACTARM") %>%
    split_cols_by_groups("MAXAETOXGR", groups = grade_groups) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible", nested = FALSE,
      indent_mod = -1L, split_fun = trim_levels_in_group("AEDECOD")
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    summarize_vars(
      "AEDECOD",
      na.rm = TRUE,
      denom = "N_col",
      .stats = "count_fraction",
      .formats = c(count_fraction = format_fraction_threshold(0.01))
    ) %>%
    build_table(adae_max, col_counts = col_counts)
}

criteria_fun <- function(tr) {
  inherits(tr, "ContentRow")
}

adsl <- adsl_raw
adae_max <- adae_raw %>%
  dplyr::group_by(ACTARM, USUBJID, AEBODSYS, AEDECOD) %>%
  dplyr::summarize(
    MAXAETOXGR = max(as.numeric(AETOXGR))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    MAXAETOXGR = factor(MAXAETOXGR),
    AEDECOD = droplevels(AEDECOD)
  )

testthat::test_that("AET04_PI full table is produced correctly", {
  result <- full_table_aet04_pi(adsl, adae_max) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 1 is produced correctly", {
  full_table <- full_table_aet04_pi(adsl, adae_max) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )
  at_least_10percent_any <- has_fraction_in_any_col(atleast = 0.1, col_indices = c(1, 4, 7))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_10percent_any))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 2 is produced correctly", {
  full_table <- full_table_aet04_pi(adsl, adae_max) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  at_least_37percent_any_drugx <- has_fraction_in_cols(atleast = 0.37, col_indices = 1)

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_37percent_any_drugx))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 3 is produced correctly", {
  full_table <- full_table_aet04_pi(adsl, adae_max) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  at_least_40percent_any <- has_fraction_in_any_col(atleast = 0.40, col_indices = c(1, 4, 7))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_40percent_any))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 4 is produced correctly", {
  full_table <- full_table_aet04_pi(adsl, adae_max) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  at_least_30percent_any <- has_fraction_in_any_col(atleast = 0.3, col_indices = c(1, 4, 7))
  at_least_15percent_diff <- has_fractions_difference(atleast = 0.15, col_indices = c(1, 4, 7))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_30percent_any & at_least_15percent_diff))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 5 is produced correctly", {
  full_table <- full_table_aet04_pi(adsl, adae_max) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  at_least_40percent_any <- has_fraction_in_any_col(atleast = 0.4, col_indices = c(1, 4, 7))
  at_least_20percent_g5 <- has_fraction_in_any_col(atleast = 0.20, col_indices = c(3, 6, 9))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_40percent_any | at_least_20percent_g5))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 6 is produced correctly", {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 1-2 (%)" = c("1", "2"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )

  col_counts <- rep(table(adsl$ACTARM), each = length(grade_groups))
  full_table <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    split_cols_by_groups("MAXAETOXGR", groups = grade_groups) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible", nested = FALSE,
      indent_mod = -1L, split_fun = trim_levels_in_group("AEDECOD")
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    summarize_vars(
      "AEDECOD",
      na.rm = TRUE,
      denom = "N_col",
      .stats = "count_fraction",
      .formats = c(count_fraction = format_fraction_threshold(0.01))
    ) %>%
    build_table(adae_max, col_counts = col_counts) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 5, 9)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 5, 9)),
      decreasing = TRUE
    )

  at_least_40percent_any <- has_fraction_in_any_col(atleast = 0.4, col_indices = c(1, 5, 9))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_40percent_any))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 7 is produced correctly", {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 3-5 (%)" = c("3", "4", "5"),
    "Grade 5 (%)" = "5"
  )

  col_counts <- rep(table(adsl$ACTARM), each = length(grade_groups))
  full_table <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    split_cols_by_groups("MAXAETOXGR", groups = grade_groups) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible", nested = FALSE,
      indent_mod = -1L, split_fun = trim_levels_in_group("AEDECOD")
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    summarize_vars(
      "AEDECOD",
      na.rm = TRUE,
      denom = "N_col",
      .stats = "count_fraction",
      .formats = c(count_fraction = format_fraction_threshold(0.01))
    ) %>%
    build_table(adae_max, col_counts = col_counts) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 5, 9)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 5, 9)),
      decreasing = TRUE
    )

  at_least_40percent_any <- has_fraction_in_any_col(atleast = 0.4, col_indices = c(1, 5, 9))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_40percent_any))

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET04_PI variant 8 is produced correctly", {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )

  col_counts <- rep(table(adsl$ACTARM), each = length(grade_groups))
  full_table <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    split_cols_by_groups("MAXAETOXGR", groups = grade_groups) %>%
    summarize_vars(
      "AEDECOD",
      na.rm = TRUE,
      denom = "N_col",
      .stats = "count_fraction",
      .formats = c(count_fraction = format_fraction_threshold(0.01))
    ) %>%
    build_table(adae_max, col_counts = col_counts)

  at_least_20percent_any <- has_fraction_in_any_col(atleast = 0.2, col_indices = c(1, 4, 7))

  result <- full_table %>%
    prune_table(keep_rows(at_least_20percent_any)) %>%
    sort_at_path(
      path = c("AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  res <- expect_silent(result)
  expect_snapshot(res)
})
