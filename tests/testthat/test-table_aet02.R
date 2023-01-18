# Test all variants of AET02

adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("AET02 variant 1 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_onecol(4)) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_matrix <- to_string_matrix(result)

  # Testing pagination with not repeated Total number of patients
  pag_result <- paginate_table(result, lpp = 20)
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3, 1],
    "Total number of patients with at least one adverse event"
  )
  testthat::expect_identical(to_string_matrix(pag_result[[2]])[3, 1], "cl D.2")
})

testthat::test_that("AET02 variant 2 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total number of events"),
      .indent_mods = c(count = -1L)
    )

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_onecol(4))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 3 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 4 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      ),
      show_labels = "hidden",
      indent_mod = -1L
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 5 is produced correctly", {
  adae_5 <- adae %>% dplyr::filter(ARM != "C: Combination")

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))

  result <- build_table(lyt, adae_5, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 6 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  criteria_fun <- function(tr) {
    inherits(tr, "ContentRow")
  }
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_any_col(atleast = 0.40, col_names = names(table(adsl$ARM)))
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 7 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -2L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = 1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD")

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols, decreasing = TRUE) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT"), scorefun = cont_n_allcols, decreasing = TRUE) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"), scorefun = score_occurrences, decreasing = TRUE)

  criteria_fun <- function(tr) {
    inherits(tr, "ContentRow")
  }
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_any_col(atleast = 0.40, col_names = names(table(adsl$ARM)))
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 8 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  criteria_fun <- function(tr) {
    inherits(tr, "ContentRow")
  }
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_any_col(atleast = 0.40, col_names = names(table(adsl$ARM)))
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 9 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  criteria_fun <- function(tr) {
    inherits(tr, "ContentRow")
  }
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_count_in_any_col(atleast = 52, col_names = names(table(adsl$ARM)))
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 10 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  criteria_fun <- function(tr) {
    inherits(tr, "ContentRow")
  }
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fractions_difference(atleast = 0.05, col_names = names(table(adsl$ARM)))
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 11 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  criteria_fun <- function(tr) {
    inherits(tr, "ContentRow")
  }
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition <- has_fraction_in_cols(atleast = 0.40, col_names = c("B: Placebo"))
  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET02 variant 12 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    count_occurrences(
      vars = "AEDECOD",
      .indent_mods = c(count_fraction = 1L)
    )

  result <- build_table(
    lyt,
    df = adae,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  criteria_fun <- function(tr) is(tr, "ContentRow")
  result <- trim_rows(result, criteria = criteria_fun)

  row_condition1 <- has_fractions_difference(atleast = 0.05, col_names = c("A: Drug X", "B: Placebo"))
  row_condition2 <- has_fractions_difference(atleast = 0.05, col_names = c("A: Drug X", "C: Combination"))
  row_condition <- row_condition1 | row_condition2

  result <- prune_table(result, keep_rows(row_condition))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
