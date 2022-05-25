library(scda)

preproc_adae <- function(adae) {
  anl <- adae %>%
    dplyr::mutate(
      AEDECOD = as.character(AEDECOD),
      AEBODSYS = as.character(AEBODSYS),
    )

  anl
}

raw_table <- function(adae, adsl) {
  gr_grp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4"),
    "Grade 5" = "5"
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )
}

# Simple wrapper to return subset ADAE to a threshold of xx%.
get_adae_trimmed <- function(adsl, adae, cutoff_rate) {
  n_per_arm <- adsl %>%
    dplyr::count(ACTARM)

  anl_terms <- adae %>%
    dplyr::group_by(ACTARM, AEBODSYS, AEDECOD) %>%
    dplyr::summarise(
      unique_terms = dplyr::n_distinct(USUBJID)
    ) %>%
    dplyr::ungroup()

  anl_terms <- dplyr::left_join(
    anl_terms,
    n_per_arm,
    by = "ACTARM"
  ) %>%
    dplyr::mutate(
      ae_rate = unique_terms / n
    ) %>%
    dplyr::filter(ae_rate >= cutoff_rate) %>%
    dplyr::select(AEDECOD) %>%
    unique()

  anl <- dplyr::left_join(
    anl_terms,
    adae,
    by = "AEDECOD"
  )
  anl
}

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

testthat::test_that("AET04 variant 1 is produced correctly", {
  adae <- preproc_adae(adae)

  gr_grp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4"),
    "Grade 5" = "5"
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4",
      "3", "4", "Grade 5", "cl A.1", "- Any Grade -", "Grade 1-2",
      "1", "2", "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1",
      "dcd A.1.1.1.2", "- Any Grade -", "Grade 1-2", "2", "cl B.2",
      "- Any Grade -", "Grade 1-2", "1", "Grade 3-4", "3", "dcd B.2.2.3.1",
      "- Any Grade -", "Grade 1-2", "1", "dcd B.2.1.2.1", "- Any Grade -",
      "Grade 3-4", "3", "cl D.1", "- Any Grade -", "Grade 3-4", "3",
      "Grade 5", "dcd D.1.1.1.1", "- Any Grade -", "Grade 5", "dcd D.1.1.4.2",
      "- Any Grade -", "Grade 3-4", "3", "cl D.2", "- Any Grade -",
      "Grade 1-2", "1", "dcd D.2.1.5.3", "- Any Grade -", "Grade 1-2",
      "1", "cl B.1", "- Any Grade -", "Grade 5", "dcd B.1.1.1.1", "- Any Grade -",
      "Grade 5", "cl C.2", "- Any Grade -", "Grade 1-2", "2", "dcd C.2.1.2.1",
      "- Any Grade -", "Grade 1-2", "2", "cl C.1", "- Any Grade -",
      "Grade 3-4", "4", "dcd C.1.1.1.3", "- Any Grade -", "Grade 3-4",
      "4", "A: Drug X", "(N=134)", "122 (91%)", "13 (9.7%)", "7 (5.2%)",
      "6 (4.5%)", "33 (24.6%)", "18 (13.4%)", "15 (11.2%)", "76 (56.7%)",
      "", "78 (58.2%)", "78 (58.2%)", "30 (22.4%)", "48 (35.8%)", "",
      "50 (37.3%)", "50 (37.3%)", "50 (37.3%)", "", "48 (35.8%)", "48 (35.8%)",
      "48 (35.8%)", "", "79 (59%)", "30 (22.4%)", "30 (22.4%)", "49 (36.6%)",
      "49 (36.6%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "",
      "49 (36.6%)", "49 (36.6%)", "49 (36.6%)", "", "79 (59%)", "29 (21.6%)",
      "29 (21.6%)", "50 (37.3%)", "", "50 (37.3%)", "50 (37.3%)", "",
      "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "47 (35.1%)", "47 (35.1%)",
      "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "",
      "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)", "",
      "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "", "35 (26.1%)", "35 (26.1%)",
      "35 (26.1%)", "", "43 (32.1%)", "43 (32.1%)", "43 (32.1%)", "",
      "43 (32.1%)", "43 (32.1%)", "43 (32.1%)", "B: Placebo", "(N=134)",
      "123 (91.8%)", "19 (14.2%)", "9 (6.7%)", "10 (7.5%)", "34 (25.4%)",
      "14 (10.4%)", "20 (14.9%)", "70 (52.2%)", "", "75 (56%)", "75 (56%)",
      "27 (20.1%)", "48 (35.8%)", "", "45 (33.6%)", "45 (33.6%)", "45 (33.6%)",
      "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "74 (55.2%)",
      "30 (22.4%)", "30 (22.4%)", "44 (32.8%)", "44 (32.8%)", "", "54 (40.3%)",
      "54 (40.3%)", "54 (40.3%)", "", "44 (32.8%)", "44 (32.8%)", "44 (32.8%)",
      "", "67 (50%)", "25 (18.7%)", "25 (18.7%)", "42 (31.3%)", "",
      "42 (31.3%)", "42 (31.3%)", "", "42 (31.3%)", "42 (31.3%)", "42 (31.3%)",
      "", "58 (43.3%)", "58 (43.3%)", "58 (43.3%)", "", "58 (43.3%)",
      "58 (43.3%)", "58 (43.3%)", "", "49 (36.6%)", "49 (36.6%)", "",
      "49 (36.6%)", "49 (36.6%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)",
      "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "46 (34.3%)",
      "46 (34.3%)", "46 (34.3%)", "", "46 (34.3%)", "46 (34.3%)", "46 (34.3%)",
      "C: Combination", "(N=132)", "120 (90.9%)", "11 (8.3%)", "4 (3%)",
      "7 (5.3%)", "34 (25.8%)", "16 (12.1%)", "18 (13.6%)", "75 (56.8%)",
      "", "89 (67.4%)", "89 (67.4%)", "39 (29.5%)", "50 (37.9%)", "",
      "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "", "50 (37.9%)", "50 (37.9%)",
      "50 (37.9%)", "", "85 (64.4%)", "33 (25%)", "33 (25%)", "52 (39.4%)",
      "52 (39.4%)", "", "51 (38.6%)", "51 (38.6%)", "51 (38.6%)", "",
      "52 (39.4%)", "52 (39.4%)", "52 (39.4%)", "", "80 (60.6%)", "29 (22%)",
      "29 (22%)", "51 (38.6%)", "", "51 (38.6%)", "51 (38.6%)", "",
      "50 (37.9%)", "50 (37.9%)", "50 (37.9%)", "", "57 (43.2%)", "57 (43.2%)",
      "57 (43.2%)", "", "57 (43.2%)", "57 (43.2%)", "57 (43.2%)", "",
      "43 (32.6%)", "43 (32.6%)", "", "43 (32.6%)", "43 (32.6%)", "",
      "55 (41.7%)", "55 (41.7%)", "55 (41.7%)", "", "55 (41.7%)", "55 (41.7%)",
      "55 (41.7%)", "", "43 (32.6%)", "43 (32.6%)", "43 (32.6%)", "",
      "43 (32.6%)", "43 (32.6%)", "43 (32.6%)"
    ),
    .Dim = c(79L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04 variant 2 is produced correctly (Fill in of Treatment Groups)", {
  adae <- adae %>%
    preproc_adae() %>%
    dplyr::filter(ACTARM == "A: Drug X")

  gr_grp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4"),
    "Grade 5" = "5"
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4",
      "3", "4", "Grade 5", "cl B.2", "- Any Grade -", "Grade 1-2",
      "1", "Grade 3-4", "3", "dcd B.2.1.2.1", "- Any Grade -", "Grade 3-4",
      "3", "dcd B.2.2.3.1", "- Any Grade -", "Grade 1-2", "1", "cl D.1",
      "- Any Grade -", "Grade 3-4", "3", "Grade 5", "dcd D.1.1.1.1",
      "- Any Grade -", "Grade 5", "dcd D.1.1.4.2", "- Any Grade -",
      "Grade 3-4", "3", "cl A.1", "- Any Grade -", "Grade 1-2", "1",
      "2", "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1", "dcd A.1.1.1.2",
      "- Any Grade -", "Grade 1-2", "2", "cl B.1", "- Any Grade -",
      "Grade 5", "dcd B.1.1.1.1", "- Any Grade -", "Grade 5", "cl D.2",
      "- Any Grade -", "Grade 1-2", "1", "dcd D.2.1.5.3", "- Any Grade -",
      "Grade 1-2", "1", "cl C.1", "- Any Grade -", "Grade 3-4", "4",
      "dcd C.1.1.1.3", "- Any Grade -", "Grade 3-4", "4", "cl C.2",
      "- Any Grade -", "Grade 1-2", "2", "dcd C.2.1.2.1", "- Any Grade -",
      "Grade 1-2", "2", "A: Drug X", "(N=134)", "122 (91%)", "13 (9.7%)",
      "7 (5.2%)", "6 (4.5%)", "33 (24.6%)", "18 (13.4%)", "15 (11.2%)",
      "76 (56.7%)", "", "79 (59%)", "30 (22.4%)", "30 (22.4%)", "49 (36.6%)",
      "49 (36.6%)", "", "49 (36.6%)", "49 (36.6%)", "49 (36.6%)", "",
      "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "79 (59%)", "29 (21.6%)",
      "29 (21.6%)", "50 (37.3%)", "", "50 (37.3%)", "50 (37.3%)", "",
      "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "78 (58.2%)", "78 (58.2%)",
      "30 (22.4%)", "48 (35.8%)", "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)",
      "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "47 (35.1%)",
      "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)",
      "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)", "47 (35.1%)",
      "", "43 (32.1%)", "43 (32.1%)", "43 (32.1%)", "", "43 (32.1%)",
      "43 (32.1%)", "43 (32.1%)", "", "35 (26.1%)", "35 (26.1%)", "35 (26.1%)",
      "", "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "B: Placebo", "(N=134)",
      "0", "0", "0", "0", "0", "0", "0", "0", "", "0", "0", "0", "0",
      "0", "", "0", "0", "0", "", "0", "0", "0", "", "0", "0", "0",
      "0", "", "0", "0", "", "0", "0", "0", "", "0", "0", "0", "0",
      "", "0", "0", "0", "", "0", "0", "0", "", "0", "0", "", "0",
      "0", "", "0", "0", "0", "", "0", "0", "0", "", "0", "0", "0",
      "", "0", "0", "0", "", "0", "0", "0", "", "0", "0", "0", "C: Combination",
      "(N=132)", "0", "0", "0", "0", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "0", "", "0", "0", "0", "", "0", "0", "0", "", "0",
      "0", "0", "0", "", "0", "0", "", "0", "0", "0", "", "0", "0",
      "0", "0", "", "0", "0", "0", "", "0", "0", "0", "", "0", "0",
      "", "0", "0", "", "0", "0", "0", "", "0", "0", "0", "", "0",
      "0", "0", "", "0", "0", "0", "", "0", "0", "0", "", "0", "0",
      "0"
    ),
    .Dim = c(79L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04 variant 3 is produced correctly (Fill in of Grades)", {
  adae <- preproc_adae(adae)

  gr_grp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4"),
    "Grade 5" = "5"
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -1L) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD", child_labels = "visible", nested = TRUE, indent_mod = -1L) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4",
      "3", "4", "Grade 5", "cl A.1", "- Any Grade -", "Grade 1-2",
      "1", "2", "Grade 3-4", "3", "4", "Grade 5", "dcd A.1.1.1.1",
      "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4", "3", "4",
      "Grade 5", "dcd A.1.1.1.2", "- Any Grade -", "Grade 1-2", "1",
      "2", "Grade 3-4", "3", "4", "Grade 5", "cl B.2", "- Any Grade -",
      "Grade 1-2", "1", "2", "Grade 3-4", "3", "4", "Grade 5", "dcd B.2.2.3.1",
      "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4", "3", "4",
      "Grade 5", "dcd B.2.1.2.1", "- Any Grade -", "Grade 1-2", "1",
      "2", "Grade 3-4", "3", "4", "Grade 5", "cl D.1", "- Any Grade -",
      "Grade 1-2", "1", "2", "Grade 3-4", "3", "4", "Grade 5", "dcd D.1.1.1.1",
      "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4", "3", "4",
      "Grade 5", "dcd D.1.1.4.2", "- Any Grade -", "Grade 1-2", "1",
      "2", "Grade 3-4", "3", "4", "Grade 5", "cl D.2", "- Any Grade -",
      "Grade 1-2", "1", "2", "Grade 3-4", "3", "4", "Grade 5", "dcd D.2.1.5.3",
      "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4", "3", "4",
      "Grade 5", "cl B.1", "- Any Grade -", "Grade 1-2", "1", "2",
      "Grade 3-4", "3", "4", "Grade 5", "dcd B.1.1.1.1", "- Any Grade -",
      "Grade 1-2", "1", "2", "Grade 3-4", "3", "4", "Grade 5", "cl C.2",
      "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4", "3", "4",
      "Grade 5", "dcd C.2.1.2.1", "- Any Grade -", "Grade 1-2", "1",
      "2", "Grade 3-4", "3", "4", "Grade 5", "cl C.1", "- Any Grade -",
      "Grade 1-2", "1", "2", "Grade 3-4", "3", "4", "Grade 5", "dcd C.1.1.1.3",
      "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4", "3", "4",
      "Grade 5", "A: Drug X", "(N=134)", "122 (91%)", "13 (9.7%)",
      "7 (5.2%)", "6 (4.5%)", "33 (24.6%)", "18 (13.4%)", "15 (11.2%)",
      "76 (56.7%)", "", "78 (58.2%)", "78 (58.2%)", "30 (22.4%)", "48 (35.8%)",
      "0", "0", "0", "0", "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)",
      "0", "0", "0", "0", "0", "", "48 (35.8%)", "48 (35.8%)", "0",
      "48 (35.8%)", "0", "0", "0", "0", "", "79 (59%)", "30 (22.4%)",
      "30 (22.4%)", "0", "49 (36.6%)", "49 (36.6%)", "0", "0", "",
      "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "0", "0", "0", "0",
      "0", "", "49 (36.6%)", "0", "0", "0", "49 (36.6%)", "49 (36.6%)",
      "0", "0", "", "79 (59%)", "0", "0", "0", "29 (21.6%)", "29 (21.6%)",
      "0", "50 (37.3%)", "", "50 (37.3%)", "0", "0", "0", "0", "0",
      "0", "50 (37.3%)", "", "48 (35.8%)", "0", "0", "0", "48 (35.8%)",
      "48 (35.8%)", "0", "0", "", "47 (35.1%)", "47 (35.1%)", "47 (35.1%)",
      "0", "0", "0", "0", "0", "", "47 (35.1%)", "47 (35.1%)", "47 (35.1%)",
      "0", "0", "0", "0", "0", "", "47 (35.1%)", "0", "0", "0", "0",
      "0", "0", "47 (35.1%)", "", "47 (35.1%)", "0", "0", "0", "0",
      "0", "0", "47 (35.1%)", "", "35 (26.1%)", "35 (26.1%)", "0",
      "35 (26.1%)", "0", "0", "0", "0", "", "35 (26.1%)", "35 (26.1%)",
      "0", "35 (26.1%)", "0", "0", "0", "0", "", "43 (32.1%)", "0",
      "0", "0", "43 (32.1%)", "0", "43 (32.1%)", "0", "", "43 (32.1%)",
      "0", "0", "0", "43 (32.1%)", "0", "43 (32.1%)", "0", "B: Placebo",
      "(N=134)", "123 (91.8%)", "19 (14.2%)", "9 (6.7%)", "10 (7.5%)",
      "34 (25.4%)", "14 (10.4%)", "20 (14.9%)", "70 (52.2%)", "", "75 (56%)",
      "75 (56%)", "27 (20.1%)", "48 (35.8%)", "0", "0", "0", "0", "",
      "45 (33.6%)", "45 (33.6%)", "45 (33.6%)", "0", "0", "0", "0",
      "0", "", "48 (35.8%)", "48 (35.8%)", "0", "48 (35.8%)", "0",
      "0", "0", "0", "", "74 (55.2%)", "30 (22.4%)", "30 (22.4%)",
      "0", "44 (32.8%)", "44 (32.8%)", "0", "0", "", "54 (40.3%)",
      "54 (40.3%)", "54 (40.3%)", "0", "0", "0", "0", "0", "", "44 (32.8%)",
      "0", "0", "0", "44 (32.8%)", "44 (32.8%)", "0", "0", "", "67 (50%)",
      "0", "0", "0", "25 (18.7%)", "25 (18.7%)", "0", "42 (31.3%)",
      "", "42 (31.3%)", "0", "0", "0", "0", "0", "0", "42 (31.3%)",
      "", "42 (31.3%)", "0", "0", "0", "42 (31.3%)", "42 (31.3%)",
      "0", "0", "", "58 (43.3%)", "58 (43.3%)", "58 (43.3%)", "0",
      "0", "0", "0", "0", "", "58 (43.3%)", "58 (43.3%)", "58 (43.3%)",
      "0", "0", "0", "0", "0", "", "49 (36.6%)", "0", "0", "0", "0",
      "0", "0", "49 (36.6%)", "", "49 (36.6%)", "0", "0", "0", "0",
      "0", "0", "49 (36.6%)", "", "48 (35.8%)", "48 (35.8%)", "0",
      "48 (35.8%)", "0", "0", "0", "0", "", "48 (35.8%)", "48 (35.8%)",
      "0", "48 (35.8%)", "0", "0", "0", "0", "", "46 (34.3%)", "0",
      "0", "0", "46 (34.3%)", "0", "46 (34.3%)", "0", "", "46 (34.3%)",
      "0", "0", "0", "46 (34.3%)", "0", "46 (34.3%)", "0", "C: Combination",
      "(N=132)", "120 (90.9%)", "11 (8.3%)", "4 (3%)", "7 (5.3%)",
      "34 (25.8%)", "16 (12.1%)", "18 (13.6%)", "75 (56.8%)", "", "89 (67.4%)",
      "89 (67.4%)", "39 (29.5%)", "50 (37.9%)", "0", "0", "0", "0",
      "", "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "0", "0", "0",
      "0", "0", "", "50 (37.9%)", "50 (37.9%)", "0", "50 (37.9%)",
      "0", "0", "0", "0", "", "85 (64.4%)", "33 (25%)", "33 (25%)",
      "0", "52 (39.4%)", "52 (39.4%)", "0", "0", "", "51 (38.6%)",
      "51 (38.6%)", "51 (38.6%)", "0", "0", "0", "0", "0", "", "52 (39.4%)",
      "0", "0", "0", "52 (39.4%)", "52 (39.4%)", "0", "0", "", "80 (60.6%)",
      "0", "0", "0", "29 (22%)", "29 (22%)", "0", "51 (38.6%)", "",
      "51 (38.6%)", "0", "0", "0", "0", "0", "0", "51 (38.6%)", "",
      "50 (37.9%)", "0", "0", "0", "50 (37.9%)", "50 (37.9%)", "0",
      "0", "", "57 (43.2%)", "57 (43.2%)", "57 (43.2%)", "0", "0",
      "0", "0", "0", "", "57 (43.2%)", "57 (43.2%)", "57 (43.2%)",
      "0", "0", "0", "0", "0", "", "43 (32.6%)", "0", "0", "0", "0",
      "0", "0", "43 (32.6%)", "", "43 (32.6%)", "0", "0", "0", "0",
      "0", "0", "43 (32.6%)", "", "55 (41.7%)", "55 (41.7%)", "0",
      "55 (41.7%)", "0", "0", "0", "0", "", "55 (41.7%)", "55 (41.7%)",
      "0", "55 (41.7%)", "0", "0", "0", "0", "", "43 (32.6%)", "0",
      "0", "0", "43 (32.6%)", "0", "43 (32.6%)", "0", "", "43 (32.6%)",
      "0", "0", "0", "43 (32.6%)", "0", "43 (32.6%)", "0"
    ),
    .Dim = c(163L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET04 variant 4 is produced correctly (Collapsing of Grades: grades 1&2, grades 3&4&5)", {
  adae <- preproc_adae(adae)

  gr_grp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-5",
      "3", "4", "5", "cl A.1", "- Any Grade -", "Grade 1-2", "1", "2",
      "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1", "dcd A.1.1.1.2",
      "- Any Grade -", "Grade 1-2", "2", "cl B.2", "- Any Grade -",
      "Grade 1-2", "1", "Grade 3-5", "3", "dcd B.2.2.3.1", "- Any Grade -",
      "Grade 1-2", "1", "dcd B.2.1.2.1", "- Any Grade -", "Grade 3-5",
      "3", "cl D.1", "- Any Grade -", "Grade 3-5", "3", "5", "dcd D.1.1.1.1",
      "- Any Grade -", "Grade 3-5", "5", "dcd D.1.1.4.2", "- Any Grade -",
      "Grade 3-5", "3", "cl D.2", "- Any Grade -", "Grade 1-2", "1",
      "dcd D.2.1.5.3", "- Any Grade -", "Grade 1-2", "1", "cl B.1",
      "- Any Grade -", "Grade 3-5", "5", "dcd B.1.1.1.1", "- Any Grade -",
      "Grade 3-5", "5", "cl C.2", "- Any Grade -", "Grade 1-2", "2",
      "dcd C.2.1.2.1", "- Any Grade -", "Grade 1-2", "2", "cl C.1",
      "- Any Grade -", "Grade 3-5", "4", "dcd C.1.1.1.3", "- Any Grade -",
      "Grade 3-5", "4", "A: Drug X", "(N=134)", "122 (91%)", "13 (9.7%)",
      "7 (5.2%)", "6 (4.5%)", "109 (81.3%)", "18 (13.4%)", "15 (11.2%)",
      "76 (56.7%)", "", "78 (58.2%)", "78 (58.2%)", "30 (22.4%)", "48 (35.8%)",
      "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)", "", "48 (35.8%)",
      "48 (35.8%)", "48 (35.8%)", "", "79 (59%)", "30 (22.4%)", "30 (22.4%)",
      "49 (36.6%)", "49 (36.6%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)",
      "", "49 (36.6%)", "49 (36.6%)", "49 (36.6%)", "", "79 (59%)",
      "79 (59%)", "29 (21.6%)", "50 (37.3%)", "", "50 (37.3%)", "50 (37.3%)",
      "50 (37.3%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "",
      "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)",
      "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "",
      "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "", "35 (26.1%)", "35 (26.1%)",
      "35 (26.1%)", "", "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "",
      "43 (32.1%)", "43 (32.1%)", "43 (32.1%)", "", "43 (32.1%)", "43 (32.1%)",
      "43 (32.1%)", "B: Placebo", "(N=134)", "123 (91.8%)", "19 (14.2%)",
      "9 (6.7%)", "10 (7.5%)", "104 (77.6%)", "14 (10.4%)", "20 (14.9%)",
      "70 (52.2%)", "", "75 (56%)", "75 (56%)", "27 (20.1%)", "48 (35.8%)",
      "", "45 (33.6%)", "45 (33.6%)", "45 (33.6%)", "", "48 (35.8%)",
      "48 (35.8%)", "48 (35.8%)", "", "74 (55.2%)", "30 (22.4%)", "30 (22.4%)",
      "44 (32.8%)", "44 (32.8%)", "", "54 (40.3%)", "54 (40.3%)", "54 (40.3%)",
      "", "44 (32.8%)", "44 (32.8%)", "44 (32.8%)", "", "67 (50%)",
      "67 (50%)", "25 (18.7%)", "42 (31.3%)", "", "42 (31.3%)", "42 (31.3%)",
      "42 (31.3%)", "", "42 (31.3%)", "42 (31.3%)", "42 (31.3%)", "",
      "58 (43.3%)", "58 (43.3%)", "58 (43.3%)", "", "58 (43.3%)", "58 (43.3%)",
      "58 (43.3%)", "", "49 (36.6%)", "49 (36.6%)", "49 (36.6%)", "",
      "49 (36.6%)", "49 (36.6%)", "49 (36.6%)", "", "48 (35.8%)", "48 (35.8%)",
      "48 (35.8%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "",
      "46 (34.3%)", "46 (34.3%)", "46 (34.3%)", "", "46 (34.3%)", "46 (34.3%)",
      "46 (34.3%)", "C: Combination", "(N=132)", "120 (90.9%)", "11 (8.3%)",
      "4 (3%)", "7 (5.3%)", "109 (82.6%)", "16 (12.1%)", "18 (13.6%)",
      "75 (56.8%)", "", "89 (67.4%)", "89 (67.4%)", "39 (29.5%)", "50 (37.9%)",
      "", "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "", "50 (37.9%)",
      "50 (37.9%)", "50 (37.9%)", "", "85 (64.4%)", "33 (25%)", "33 (25%)",
      "52 (39.4%)", "52 (39.4%)", "", "51 (38.6%)", "51 (38.6%)", "51 (38.6%)",
      "", "52 (39.4%)", "52 (39.4%)", "52 (39.4%)", "", "80 (60.6%)",
      "80 (60.6%)", "29 (22%)", "51 (38.6%)", "", "51 (38.6%)", "51 (38.6%)",
      "51 (38.6%)", "", "50 (37.9%)", "50 (37.9%)", "50 (37.9%)", "",
      "57 (43.2%)", "57 (43.2%)", "57 (43.2%)", "", "57 (43.2%)", "57 (43.2%)",
      "57 (43.2%)", "", "43 (32.6%)", "43 (32.6%)", "43 (32.6%)", "",
      "43 (32.6%)", "43 (32.6%)", "43 (32.6%)", "", "55 (41.7%)", "55 (41.7%)",
      "55 (41.7%)", "", "55 (41.7%)", "55 (41.7%)", "55 (41.7%)", "",
      "43 (32.6%)", "43 (32.6%)", "43 (32.6%)", "", "43 (32.6%)", "43 (32.6%)",
      "43 (32.6%)"
    ),
    .Dim = c(82L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

# No test done for variant 5 (Using Worst Grade Flags from ADAE) since it's similar to
# variant 1 (just pre-processing the data).

testthat::test_that("AET04 variant 6 is produced correctly (with an
                    Incidence Rate of at Least 5%, totals restricted)", {
  adae <- preproc_adae(adae)
  adae <- get_adae_trimmed(adsl, adae, cutoff_rate = 0.4) %>%
    dplyr::mutate(AETOXGR = droplevels(AETOXGR))

  gr_grp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4"),
    "Grade 5" = "5"
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp[-1],
      .indent_mods = -1L
    )

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "cl D.2",
      "- Any Grade -", "Grade 1-2", "1", "dcd D.2.1.5.3", "- Any Grade -",
      "Grade 1-2", "1", "cl A.1", "- Any Grade -", "Grade 1-2", "1",
      "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1", "cl B.2",
      "- Any Grade -", "Grade 1-2", "1", "dcd B.2.2.3.1", "- Any Grade -",
      "Grade 1-2", "1", "cl C.2", "- Any Grade -", "Grade 1-2", "2",
      "dcd C.2.1.2.1", "- Any Grade -", "Grade 1-2", "2", "A: Drug X",
      "(N=134)", "92 (68.7%)", "92 (68.7%)", "57 (42.5%)", "35 (26.1%)",
      "", "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)",
      "47 (35.1%)", "47 (35.1%)", "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)",
      "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)", "", "48 (35.8%)",
      "48 (35.8%)", "48 (35.8%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)",
      "", "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "", "35 (26.1%)",
      "35 (26.1%)", "35 (26.1%)", "B: Placebo", "(N=134)", "104 (77.6%)",
      "104 (77.6%)", "56 (41.8%)", "48 (35.8%)", "", "58 (43.3%)",
      "58 (43.3%)", "58 (43.3%)", "", "58 (43.3%)", "58 (43.3%)", "58 (43.3%)",
      "", "45 (33.6%)", "45 (33.6%)", "45 (33.6%)", "", "45 (33.6%)",
      "45 (33.6%)", "45 (33.6%)", "", "54 (40.3%)", "54 (40.3%)", "54 (40.3%)",
      "", "54 (40.3%)", "54 (40.3%)", "54 (40.3%)", "", "48 (35.8%)",
      "48 (35.8%)", "48 (35.8%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)",
      "C: Combination", "(N=132)", "107 (81.1%)", "107 (81.1%)", "52 (39.4%)",
      "55 (41.7%)", "", "57 (43.2%)", "57 (43.2%)", "57 (43.2%)", "",
      "57 (43.2%)", "57 (43.2%)", "57 (43.2%)", "", "63 (47.7%)", "63 (47.7%)",
      "63 (47.7%)", "", "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "",
      "51 (38.6%)", "51 (38.6%)", "51 (38.6%)", "", "51 (38.6%)", "51 (38.6%)",
      "51 (38.6%)", "", "55 (41.7%)", "55 (41.7%)", "55 (41.7%)", "",
      "55 (41.7%)", "55 (41.7%)", "55 (41.7%)"
    ),
    .Dim = c(38L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# No test done for variant 7, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X Patients, totals unrestriced).
# With this variant, the SOC level is not trimmed (even if there are no terms left).

# NOTE: STREAM logic will only trim at term level
testthat::test_that("AET04 variant 8 is produced correctly (with an Incidence Rate of at Least X Patients)", {
  adae <- preproc_adae(adae)

  raw_result <- raw_table(adae, adsl)

  cutoff <- 58L
  row_condition <- has_count_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

  result <- prune_table(raw_result, keep_rows(row_condition))
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4",
      "3", "4", "Grade 5", "cl A.1", "- Any Grade -", "Grade 1-2",
      "1", "2", "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1",
      "cl D.2", "- Any Grade -", "Grade 1-2", "1", "dcd D.2.1.5.3",
      "- Any Grade -", "Grade 1-2", "1", "A: Drug X", "(N=134)", "122 (91%)",
      "13 (9.7%)", "7 (5.2%)", "6 (4.5%)", "33 (24.6%)", "18 (13.4%)",
      "15 (11.2%)", "76 (56.7%)", "", "78 (58.2%)", "78 (58.2%)", "30 (22.4%)",
      "48 (35.8%)", "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)", "",
      "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)",
      "47 (35.1%)", "B: Placebo", "(N=134)", "123 (91.8%)", "19 (14.2%)",
      "9 (6.7%)", "10 (7.5%)", "34 (25.4%)", "14 (10.4%)", "20 (14.9%)",
      "70 (52.2%)", "", "75 (56%)", "75 (56%)", "27 (20.1%)", "48 (35.8%)",
      "", "45 (33.6%)", "45 (33.6%)", "45 (33.6%)", "", "58 (43.3%)",
      "58 (43.3%)", "58 (43.3%)", "", "58 (43.3%)", "58 (43.3%)", "58 (43.3%)",
      "C: Combination", "(N=132)", "120 (90.9%)", "11 (8.3%)", "4 (3%)",
      "7 (5.3%)", "34 (25.8%)", "16 (12.1%)", "18 (13.6%)", "75 (56.8%)",
      "", "89 (67.4%)", "89 (67.4%)", "39 (29.5%)", "50 (37.9%)", "",
      "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "", "57 (43.2%)", "57 (43.2%)",
      "57 (43.2%)", "", "57 (43.2%)", "57 (43.2%)", "57 (43.2%)"
    ),
    .Dim = c(27L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# NOTE: STREAM logic will only tream at term level
testthat::test_that("AET04 variant 9 is produced correctlyb(with a Difference in Incidence Rate of at Least X%)", {
  adae <- preproc_adae(adae)

  raw_result <- raw_table(adae, adsl)

  cutoff <- 0.1
  row_condition <- has_fractions_difference(atleast = cutoff, col_names = names(raw_result))

  result <- prune_table(raw_result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4",
      "3", "4", "Grade 5", "cl A.1", "- Any Grade -", "Grade 1-2",
      "1", "2", "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1",
      "cl C.2", "- Any Grade -", "Grade 1-2", "2", "dcd C.2.1.2.1",
      "- Any Grade -", "Grade 1-2", "2", "A: Drug X", "(N=134)", "122 (91%)",
      "13 (9.7%)", "7 (5.2%)", "6 (4.5%)", "33 (24.6%)", "18 (13.4%)",
      "15 (11.2%)", "76 (56.7%)", "", "78 (58.2%)", "78 (58.2%)", "30 (22.4%)",
      "48 (35.8%)", "", "50 (37.3%)", "50 (37.3%)", "50 (37.3%)", "",
      "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "", "35 (26.1%)", "35 (26.1%)",
      "35 (26.1%)", "B: Placebo", "(N=134)", "123 (91.8%)", "19 (14.2%)",
      "9 (6.7%)", "10 (7.5%)", "34 (25.4%)", "14 (10.4%)", "20 (14.9%)",
      "70 (52.2%)", "", "75 (56%)", "75 (56%)", "27 (20.1%)", "48 (35.8%)",
      "", "45 (33.6%)", "45 (33.6%)", "45 (33.6%)", "", "48 (35.8%)",
      "48 (35.8%)", "48 (35.8%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)",
      "C: Combination", "(N=132)", "120 (90.9%)", "11 (8.3%)", "4 (3%)",
      "7 (5.3%)", "34 (25.8%)", "16 (12.1%)", "18 (13.6%)", "75 (56.8%)",
      "", "89 (67.4%)", "89 (67.4%)", "39 (29.5%)", "50 (37.9%)", "",
      "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "", "55 (41.7%)", "55 (41.7%)",
      "55 (41.7%)", "", "55 (41.7%)", "55 (41.7%)", "55 (41.7%)"
    ),
    .Dim = c(27L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

# No test done for variant 10, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X%, SOCs below X% removed).
# With this variant, SOC levels above the threshold are still in the table even if
# there are no terms left.

testthat::test_that(
  "AET04 variant 11 is produced correctly
  (with an Incidence Rate of at Least X%, all SOCs w/o preferred terms removed)",
  code = {
    adae <- preproc_adae(adae)

    raw_result <- raw_table(adae, adsl)

    cutoff <- 0.4
    row_condition <- has_fraction_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

    result <- prune_table(raw_result, keep_rows(row_condition))

    result_matrix <- to_string_matrix(result)

    expected_matrix <- structure(
      c(
        "", "", "- Any Grade -", "Grade 1-2", "1", "2", "Grade 3-4",
        "3", "4", "Grade 5", "cl A.1", "- Any Grade -", "Grade 1-2",
        "1", "2", "dcd A.1.1.1.1", "- Any Grade -", "Grade 1-2", "1",
        "cl B.2", "- Any Grade -", "Grade 1-2", "1", "Grade 3-4", "3",
        "dcd B.2.2.3.1", "- Any Grade -", "Grade 1-2", "1", "cl D.2",
        "- Any Grade -", "Grade 1-2", "1", "dcd D.2.1.5.3", "- Any Grade -",
        "Grade 1-2", "1", "cl C.2", "- Any Grade -", "Grade 1-2", "2",
        "dcd C.2.1.2.1", "- Any Grade -", "Grade 1-2", "2", "A: Drug X",
        "(N=134)", "122 (91%)", "13 (9.7%)", "7 (5.2%)", "6 (4.5%)",
        "33 (24.6%)", "18 (13.4%)", "15 (11.2%)", "76 (56.7%)", "", "78 (58.2%)",
        "78 (58.2%)", "30 (22.4%)", "48 (35.8%)", "", "50 (37.3%)", "50 (37.3%)",
        "50 (37.3%)", "", "79 (59%)", "30 (22.4%)", "30 (22.4%)", "49 (36.6%)",
        "49 (36.6%)", "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "",
        "47 (35.1%)", "47 (35.1%)", "47 (35.1%)", "", "47 (35.1%)", "47 (35.1%)",
        "47 (35.1%)", "", "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "",
        "35 (26.1%)", "35 (26.1%)", "35 (26.1%)", "B: Placebo", "(N=134)",
        "123 (91.8%)", "19 (14.2%)", "9 (6.7%)", "10 (7.5%)", "34 (25.4%)",
        "14 (10.4%)", "20 (14.9%)", "70 (52.2%)", "", "75 (56%)", "75 (56%)",
        "27 (20.1%)", "48 (35.8%)", "", "45 (33.6%)", "45 (33.6%)", "45 (33.6%)",
        "", "74 (55.2%)", "30 (22.4%)", "30 (22.4%)", "44 (32.8%)", "44 (32.8%)",
        "", "54 (40.3%)", "54 (40.3%)", "54 (40.3%)", "", "58 (43.3%)",
        "58 (43.3%)", "58 (43.3%)", "", "58 (43.3%)", "58 (43.3%)", "58 (43.3%)",
        "", "48 (35.8%)", "48 (35.8%)", "48 (35.8%)", "", "48 (35.8%)",
        "48 (35.8%)", "48 (35.8%)", "C: Combination", "(N=132)", "120 (90.9%)",
        "11 (8.3%)", "4 (3%)", "7 (5.3%)", "34 (25.8%)", "16 (12.1%)",
        "18 (13.6%)", "75 (56.8%)", "", "89 (67.4%)", "89 (67.4%)", "39 (29.5%)",
        "50 (37.9%)", "", "63 (47.7%)", "63 (47.7%)", "63 (47.7%)", "",
        "85 (64.4%)", "33 (25%)", "33 (25%)", "52 (39.4%)", "52 (39.4%)",
        "", "51 (38.6%)", "51 (38.6%)", "51 (38.6%)", "", "57 (43.2%)",
        "57 (43.2%)", "57 (43.2%)", "", "57 (43.2%)", "57 (43.2%)", "57 (43.2%)",
        "", "55 (41.7%)", "55 (41.7%)", "55 (41.7%)", "", "55 (41.7%)",
        "55 (41.7%)", "55 (41.7%)"
      ),
      .Dim = c(45L, 4L)
    )
    testthat::expect_identical(result_matrix, expected_matrix)
  }
)
