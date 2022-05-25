library(scda)
library(dplyr)

preprocess_adae <- function(adae) {
  adae %>%
    dplyr::group_by(ACTARM, USUBJID, AEBODSYS, AEDECOD) %>%
    dplyr::summarize(
      MAXAETOXGR = max(as.numeric(AETOXGR))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      MAXAETOXGR = factor(MAXAETOXGR),
      AEDECOD = droplevels(AEDECOD)
    )
}

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

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

testthat::test_that("AET04_PI full table is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "Total number of patients with at least one adverse event",
      "dcd A.1.1.1.1", "dcd A.1.1.1.2", "cl B.2", "Total number of patients with at least one adverse event",
      "dcd B.2.2.3.1", "dcd B.2.1.2.1", "cl D.1", "Total number of patients with at least one adverse event",
      "dcd D.1.1.1.1", "dcd D.1.1.4.2", "cl D.2", "Total number of patients with at least one adverse event",
      "dcd D.2.1.5.3", "cl B.1", "Total number of patients with at least one adverse event",
      "dcd B.1.1.1.1", "cl C.2", "Total number of patients with at least one adverse event",
      "dcd C.2.1.2.1", "cl C.1", "Total number of patients with at least one adverse event",
      "dcd C.1.1.1.3", "A: Drug X", "Any Grade (%)", "(N=134)", "",
      "78 (58.2%)", "37", "36", "", "79 (59%)", "36", "37", "", "79 (59%)",
      "37", "36", "", "47 (35.1%)", "35", "", "47 (35.1%)", "35", "",
      "35 (26.1%)", "26", "", "43 (32.1%)", "32", "A: Drug X", "Grade 3-4 (%)",
      "(N=134)", "", "0", "0", "0", "", "49 (36.6%)", "0", "37", "",
      "48 (35.8%)", "0", "36", "", "0", "0", "", "0", "0", "", "0",
      "0", "", "43 (32.1%)", "32", "A: Drug X", "Grade 5 (%)", "(N=134)",
      "", "0", "0", "0", "", "0", "0", "0", "", "50 (37.3%)", "37",
      "0", "", "0", "0", "", "47 (35.1%)", "35", "", "0", "0", "",
      "0", "0", "B: Placebo", "Any Grade (%)", "(N=134)", "", "75 (56%)",
      "34", "36", "", "74 (55.2%)", "40", "33", "", "67 (50%)", "31",
      "31", "", "58 (43.3%)", "43", "", "49 (36.6%)", "37", "", "48 (35.8%)",
      "36", "", "46 (34.3%)", "34", "B: Placebo", "Grade 3-4 (%)",
      "(N=134)", "", "0", "0", "0", "", "44 (32.8%)", "0", "33", "",
      "42 (31.3%)", "0", "31", "", "0", "0", "", "0", "0", "", "0",
      "0", "", "46 (34.3%)", "34", "B: Placebo", "Grade 5 (%)", "(N=134)",
      "", "0", "0", "0", "", "0", "0", "0", "", "42 (31.3%)", "31",
      "0", "", "0", "0", "", "49 (36.6%)", "37", "", "0", "0", "",
      "0", "0", "C: Combination", "Any Grade (%)", "(N=132)", "", "89 (67.4%)",
      "48", "38", "", "85 (64.4%)", "39", "39", "", "80 (60.6%)", "39",
      "38", "", "57 (43.2%)", "43", "", "43 (32.6%)", "33", "", "55 (41.7%)",
      "42", "", "43 (32.6%)", "33", "C: Combination", "Grade 3-4 (%)",
      "(N=132)", "", "0", "0", "0", "", "52 (39.4%)", "0", "39", "",
      "50 (37.9%)", "0", "38", "", "0", "0", "", "0", "0", "", "0",
      "0", "", "43 (32.6%)", "33", "C: Combination", "Grade 5 (%)",
      "(N=132)", "", "0", "0", "0", "", "0", "0", "0", "", "51 (38.6%)",
      "39", "0", "", "0", "0", "", "43 (32.6%)", "33", "", "0", "0",
      "", "0", "0"
    ),
    .Dim = c(27L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 1 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()
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
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "dcd A.1.1.1.1", "dcd A.1.1.1.2",
      "cl B.2", "dcd B.2.2.3.1", "dcd B.2.1.2.1", "cl D.1", "dcd D.1.1.1.1",
      "dcd D.1.1.4.2", "cl D.2", "dcd D.2.1.5.3", "cl B.1", "dcd B.1.1.1.1",
      "cl C.2", "dcd C.2.1.2.1", "cl C.1", "dcd C.1.1.1.3", "A: Drug X",
      "Any Grade (%)", "(N=134)", "", "37", "36", "", "36", "37", "",
      "37", "36", "", "35", "", "35", "", "26", "", "32", "A: Drug X",
      "Grade 3-4 (%)", "(N=134)", "", "0", "0", "", "0", "37", "",
      "0", "36", "", "0", "", "0", "", "0", "", "32", "A: Drug X",
      "Grade 5 (%)", "(N=134)", "", "0", "0", "", "0", "0", "", "37",
      "0", "", "0", "", "35", "", "0", "", "0", "B: Placebo", "Any Grade (%)",
      "(N=134)", "", "34", "36", "", "40", "33", "", "31", "31", "",
      "43", "", "37", "", "36", "", "34", "B: Placebo", "Grade 3-4 (%)",
      "(N=134)", "", "0", "0", "", "0", "33", "", "0", "31", "", "0",
      "", "0", "", "0", "", "34", "B: Placebo", "Grade 5 (%)", "(N=134)",
      "", "0", "0", "", "0", "0", "", "31", "0", "", "0", "", "37",
      "", "0", "", "0", "C: Combination", "Any Grade (%)", "(N=132)",
      "", "48", "38", "", "39", "39", "", "39", "38", "", "43", "",
      "33", "", "42", "", "33", "C: Combination", "Grade 3-4 (%)",
      "(N=132)", "", "0", "0", "", "0", "39", "", "0", "38", "", "0",
      "", "0", "", "0", "", "33", "C: Combination", "Grade 5 (%)",
      "(N=132)", "", "0", "0", "", "0", "0", "", "39", "0", "", "0",
      "", "33", "", "0", "", "0"
    ),
    .Dim = c(20L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 2 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "dcd A.1.1.1.1", "cl D.1",
      "dcd D.1.1.1.1", "A: Drug X", "Any Grade (%)", "(N=134)", "",
      "37", "", "37", "A: Drug X", "Grade 3-4 (%)", "(N=134)", "",
      "0", "", "0", "A: Drug X", "Grade 5 (%)", "(N=134)", "", "0",
      "", "37", "B: Placebo", "Any Grade (%)", "(N=134)", "", "34",
      "", "31", "B: Placebo", "Grade 3-4 (%)", "(N=134)", "", "0",
      "", "0", "B: Placebo", "Grade 5 (%)", "(N=134)", "", "0", "",
      "31", "C: Combination", "Any Grade (%)", "(N=132)", "", "48",
      "", "39", "C: Combination", "Grade 3-4 (%)", "(N=132)", "", "0",
      "", "0", "C: Combination", "Grade 5 (%)", "(N=132)", "", "0",
      "", "39"
    ),
    .Dim = c(7L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 3 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "dcd A.1.1.1.1", "cl B.2",
      "dcd B.2.2.3.1", "cl D.2", "dcd D.2.1.5.3", "cl C.2", "dcd C.2.1.2.1",
      "A: Drug X", "Any Grade (%)", "(N=134)", "", "37", "", "36",
      "", "35", "", "26", "A: Drug X", "Grade 3-4 (%)", "(N=134)",
      "", "0", "", "0", "", "0", "", "0", "A: Drug X", "Grade 5 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "B: Placebo",
      "Any Grade (%)", "(N=134)", "", "34", "", "40", "", "43", "",
      "36", "B: Placebo", "Grade 3-4 (%)", "(N=134)", "", "0", "",
      "0", "", "0", "", "0", "B: Placebo", "Grade 5 (%)", "(N=134)",
      "", "0", "", "0", "", "0", "", "0", "C: Combination", "Any Grade (%)",
      "(N=132)", "", "48", "", "39", "", "43", "", "42", "C: Combination",
      "Grade 3-4 (%)", "(N=132)", "", "0", "", "0", "", "0", "", "0",
      "C: Combination", "Grade 5 (%)", "(N=132)", "", "0", "", "0",
      "", "0", "", "0"
    ),
    .Dim = 11:10
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 4 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl C.2", "dcd C.2.1.2.1", "A: Drug X",
      "Any Grade (%)", "(N=134)", "", "26", "A: Drug X", "Grade 3-4 (%)",
      "(N=134)", "", "0", "A: Drug X", "Grade 5 (%)", "(N=134)", "",
      "0", "B: Placebo", "Any Grade (%)", "(N=134)", "", "36", "B: Placebo",
      "Grade 3-4 (%)", "(N=134)", "", "0", "B: Placebo", "Grade 5 (%)",
      "(N=134)", "", "0", "C: Combination", "Any Grade (%)", "(N=132)",
      "", "42", "C: Combination", "Grade 3-4 (%)", "(N=132)", "", "0",
      "C: Combination", "Grade 5 (%)", "(N=132)", "", "0"
    ),
    .Dim = c(5L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 5 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "dcd A.1.1.1.1", "cl B.2",
      "dcd B.2.2.3.1", "cl D.1", "dcd D.1.1.1.1", "cl D.2", "dcd D.2.1.5.3",
      "cl B.1", "dcd B.1.1.1.1", "cl C.2", "dcd C.2.1.2.1", "A: Drug X",
      "Any Grade (%)", "(N=134)", "", "37", "", "36", "", "37", "",
      "35", "", "35", "", "26", "A: Drug X", "Grade 3-4 (%)", "(N=134)",
      "", "0", "", "0", "", "0", "", "0", "", "0", "", "0", "A: Drug X",
      "Grade 5 (%)", "(N=134)", "", "0", "", "0", "", "37", "", "0",
      "", "35", "", "0", "B: Placebo", "Any Grade (%)", "(N=134)",
      "", "34", "", "40", "", "31", "", "43", "", "37", "", "36", "B: Placebo",
      "Grade 3-4 (%)", "(N=134)", "", "0", "", "0", "", "0", "", "0",
      "", "0", "", "0", "B: Placebo", "Grade 5 (%)", "(N=134)", "",
      "0", "", "0", "", "31", "", "0", "", "37", "", "0", "C: Combination",
      "Any Grade (%)", "(N=132)", "", "48", "", "39", "", "39", "",
      "43", "", "33", "", "42", "C: Combination", "Grade 3-4 (%)",
      "(N=132)", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0",
      "C: Combination", "Grade 5 (%)", "(N=132)", "", "0", "", "0",
      "", "39", "", "0", "", "33", "", "0"
    ),
    .Dim = c(15L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 6 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()

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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "dcd A.1.1.1.1", "cl B.2",
      "dcd B.2.2.3.1", "cl D.2", "dcd D.2.1.5.3", "cl C.2", "dcd C.2.1.2.1",
      "A: Drug X", "Any Grade (%)", "(N=134)", "", "37", "", "36",
      "", "35", "", "26", "A: Drug X", "Grade 1-2 (%)", "(N=134)",
      "", "37", "", "36", "", "35", "", "26", "A: Drug X", "Grade 3-4 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "A: Drug X", "Grade 5 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "B: Placebo",
      "Any Grade (%)", "(N=134)", "", "34", "", "40", "", "43", "",
      "36", "B: Placebo", "Grade 1-2 (%)", "(N=134)", "", "34", "",
      "40", "", "43", "", "36", "B: Placebo", "Grade 3-4 (%)", "(N=134)",
      "", "0", "", "0", "", "0", "", "0", "B: Placebo", "Grade 5 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "C: Combination",
      "Any Grade (%)", "(N=132)", "", "48", "", "39", "", "43", "",
      "42", "C: Combination", "Grade 1-2 (%)", "(N=132)", "", "48",
      "", "39", "", "43", "", "42", "C: Combination", "Grade 3-4 (%)",
      "(N=132)", "", "0", "", "0", "", "0", "", "0", "C: Combination",
      "Grade 5 (%)", "(N=132)", "", "0", "", "0", "", "0", "", "0"
    ),
    .Dim = c(11L, 13L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 7 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()

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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "cl A.1", "dcd A.1.1.1.1", "cl B.2",
      "dcd B.2.2.3.1", "cl D.2", "dcd D.2.1.5.3", "cl C.2", "dcd C.2.1.2.1",
      "A: Drug X", "Any Grade (%)", "(N=134)", "", "37", "", "36",
      "", "35", "", "26", "A: Drug X", "Grade 3-4 (%)", "(N=134)",
      "", "0", "", "0", "", "0", "", "0", "A: Drug X", "Grade 3-5 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "A: Drug X", "Grade 5 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "B: Placebo",
      "Any Grade (%)", "(N=134)", "", "34", "", "40", "", "43", "",
      "36", "B: Placebo", "Grade 3-4 (%)", "(N=134)", "", "0", "",
      "0", "", "0", "", "0", "B: Placebo", "Grade 3-5 (%)", "(N=134)",
      "", "0", "", "0", "", "0", "", "0", "B: Placebo", "Grade 5 (%)",
      "(N=134)", "", "0", "", "0", "", "0", "", "0", "C: Combination",
      "Any Grade (%)", "(N=132)", "", "48", "", "39", "", "43", "",
      "42", "C: Combination", "Grade 3-4 (%)", "(N=132)", "", "0",
      "", "0", "", "0", "", "0", "C: Combination", "Grade 3-5 (%)",
      "(N=132)", "", "0", "", "0", "", "0", "", "0", "C: Combination",
      "Grade 5 (%)", "(N=132)", "", "0", "", "0", "", "0", "", "0"
    ),
    .Dim = c(11L, 13L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04_PI variant 8 is produced correctly", {
  adae_max <- adae %>%
    preprocess_adae()

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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "dcd D.2.1.5.3", "dcd A.1.1.1.1", "dcd B.2.2.3.1",
      "dcd A.1.1.1.2", "dcd B.2.1.2.1", "dcd D.1.1.1.1", "dcd D.1.1.4.2",
      "dcd B.1.1.1.1", "dcd C.2.1.2.1", "dcd C.1.1.1.3", "A: Drug X",
      "Any Grade (%)", "(N=134)", "35", "37", "36", "36", "37", "37",
      "36", "35", "26", "32", "A: Drug X", "Grade 3-4 (%)", "(N=134)",
      "0", "0", "0", "0", "37", "0", "36", "0", "0", "32", "A: Drug X",
      "Grade 5 (%)", "(N=134)", "0", "0", "0", "0", "0", "37", "0",
      "35", "0", "0", "B: Placebo", "Any Grade (%)", "(N=134)", "43",
      "34", "40", "36", "33", "31", "31", "37", "36", "34", "B: Placebo",
      "Grade 3-4 (%)", "(N=134)", "0", "0", "0", "0", "33", "0", "31",
      "0", "0", "34", "B: Placebo", "Grade 5 (%)", "(N=134)", "0",
      "0", "0", "0", "0", "31", "0", "37", "0", "0", "C: Combination",
      "Any Grade (%)", "(N=132)", "43", "48", "39", "38", "39", "39",
      "38", "33", "42", "33", "C: Combination", "Grade 3-4 (%)", "(N=132)",
      "0", "0", "0", "0", "39", "0", "38", "0", "0", "33", "C: Combination",
      "Grade 5 (%)", "(N=132)", "0", "0", "0", "0", "0", "39", "0",
      "33", "0", "0"
    ),
    .Dim = c(13L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
