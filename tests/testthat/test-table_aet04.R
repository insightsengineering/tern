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
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
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

adsl <- adsl_raw
adae <- preproc_adae(adae_raw)

gr_grp <- list(
  "- Any Grade -" = c("1", "2", "3", "4", "5"),
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

testthat::test_that("AET04 variant 1 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
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

  result_matrix <- to_string_matrix(result, with_spaces = TRUE)

  expected_matrix <- c(
    "                       A: Drug X    B: Placebo    C: Combination",
    "                        (N=134)       (N=134)        (N=132)    ",
    "————————————————————————————————————————————————————————————————",
    "- Any Grade -         122 (91.0%)   123 (91.8%)    120 (90.9%)  ",
    "Grade 1-2              13 (9.7%)    19 (14.2%)      11 (8.3%)   ",
    "1                      7 (5.2%)      9 (6.7%)        4 (3.0%)   ",
    "2                      6 (4.5%)      10 (7.5%)       7 (5.3%)   ",
    "Grade 3-4             33 (24.6%)    34 (25.4%)      34 (25.8%)  ",
    "3                     18 (13.4%)    14 (10.4%)      16 (12.1%)  ",
    "4                     15 (11.2%)    20 (14.9%)      18 (13.6%)  ",
    "Grade 5               76 (56.7%)    70 (52.2%)      75 (56.8%)  ",
    "cl A.1                                                          ",
    "  - Any Grade -       78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  Grade 1-2           78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  1                   30 (22.4%)    27 (20.1%)      39 (29.5%)  ",
    "  2                   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "    dcd A.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      Grade 1-2       50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      1               50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "    dcd A.1.1.1.2                                               ",
    "      - Any Grade -   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "      Grade 1-2       48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "      2               48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "cl B.2                                                          ",
    "  - Any Grade -       79 (59.0%)    74 (55.2%)      85 (64.4%)  ",
    "  Grade 1-2           30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
    "  1                   30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
    "  Grade 3-4           49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "  3                   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "    dcd B.2.2.3.1                                               ",
    "      - Any Grade -   48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "      Grade 1-2       48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "      1               48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "    dcd B.2.1.2.1                                               ",
    "      - Any Grade -   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "      Grade 3-4       49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "      3               49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "cl D.1                                                          ",
    "  - Any Grade -       79 (59.0%)    67 (50.0%)      80 (60.6%)  ",
    "  Grade 3-4           29 (21.6%)    25 (18.7%)      29 (22.0%)  ",
    "  3                   29 (21.6%)    25 (18.7%)      29 (22.0%)  ",
    "  Grade 5             50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "    dcd D.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "      Grade 5         50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "    dcd D.1.1.4.2                                               ",
    "      - Any Grade -   48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "      Grade 3-4       48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "      3               48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "cl D.2                                                          ",
    "  - Any Grade -       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  Grade 1-2           47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  1                   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "    dcd D.2.1.5.3                                               ",
    "      - Any Grade -   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      Grade 1-2       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      1               47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "cl B.1                                                          ",
    "  - Any Grade -       47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "  Grade 5             47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "    dcd B.1.1.1.1                                               ",
    "      - Any Grade -   47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "      Grade 5         47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "cl C.2                                                          ",
    "  - Any Grade -       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  Grade 1-2           35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  2                   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "    dcd C.2.1.2.1                                               ",
    "      - Any Grade -   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      Grade 1-2       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      2               35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "cl C.1                                                          ",
    "  - Any Grade -       43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "  Grade 3-4           43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "  4                   43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "    dcd C.1.1.1.3                                               ",
    "      - Any Grade -   43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "      Grade 3-4       43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "      4               43 (32.1%)    46 (34.3%)      43 (32.6%)  "
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 1],
    c("- Any Grade -", "Grade 1-2")
  )
})

testthat::test_that("AET04 variant 2 is produced correctly (Fill in of Treatment Groups)", {
  adae <- adae %>%
    dplyr::filter(ACTARM == "A: Drug X")

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
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

  result_matrix <- to_string_matrix(result, with_spaces = TRUE)

  expected_matrix <- c(
    "                       A: Drug X    B: Placebo   C: Combination",
    "                        (N=134)      (N=134)        (N=132)    ",
    "———————————————————————————————————————————————————————————————",
    "- Any Grade -         122 (91.0%)       0              0       ",
    "Grade 1-2              13 (9.7%)        0              0       ",
    "1                      7 (5.2%)         0              0       ",
    "2                      6 (4.5%)         0              0       ",
    "Grade 3-4             33 (24.6%)        0              0       ",
    "3                     18 (13.4%)        0              0       ",
    "4                     15 (11.2%)        0              0       ",
    "Grade 5               76 (56.7%)        0              0       ",
    "cl B.2                                                         ",
    "  - Any Grade -       79 (59.0%)        0              0       ",
    "  Grade 1-2           30 (22.4%)        0              0       ",
    "  1                   30 (22.4%)        0              0       ",
    "  Grade 3-4           49 (36.6%)        0              0       ",
    "  3                   49 (36.6%)        0              0       ",
    "    dcd B.2.1.2.1                                              ",
    "      - Any Grade -   49 (36.6%)        0              0       ",
    "      Grade 3-4       49 (36.6%)        0              0       ",
    "      3               49 (36.6%)        0              0       ",
    "    dcd B.2.2.3.1                                              ",
    "      - Any Grade -   48 (35.8%)        0              0       ",
    "      Grade 1-2       48 (35.8%)        0              0       ",
    "      1               48 (35.8%)        0              0       ",
    "cl D.1                                                         ",
    "  - Any Grade -       79 (59.0%)        0              0       ",
    "  Grade 3-4           29 (21.6%)        0              0       ",
    "  3                   29 (21.6%)        0              0       ",
    "  Grade 5             50 (37.3%)        0              0       ",
    "    dcd D.1.1.1.1                                              ",
    "      - Any Grade -   50 (37.3%)        0              0       ",
    "      Grade 5         50 (37.3%)        0              0       ",
    "    dcd D.1.1.4.2                                              ",
    "      - Any Grade -   48 (35.8%)        0              0       ",
    "      Grade 3-4       48 (35.8%)        0              0       ",
    "      3               48 (35.8%)        0              0       ",
    "cl A.1                                                         ",
    "  - Any Grade -       78 (58.2%)        0              0       ",
    "  Grade 1-2           78 (58.2%)        0              0       ",
    "  1                   30 (22.4%)        0              0       ",
    "  2                   48 (35.8%)        0              0       ",
    "    dcd A.1.1.1.1                                              ",
    "      - Any Grade -   50 (37.3%)        0              0       ",
    "      Grade 1-2       50 (37.3%)        0              0       ",
    "      1               50 (37.3%)        0              0       ",
    "    dcd A.1.1.1.2                                              ",
    "      - Any Grade -   48 (35.8%)        0              0       ",
    "      Grade 1-2       48 (35.8%)        0              0       ",
    "      2               48 (35.8%)        0              0       ",
    "cl B.1                                                         ",
    "  - Any Grade -       47 (35.1%)        0              0       ",
    "  Grade 5             47 (35.1%)        0              0       ",
    "    dcd B.1.1.1.1                                              ",
    "      - Any Grade -   47 (35.1%)        0              0       ",
    "      Grade 5         47 (35.1%)        0              0       ",
    "cl D.2                                                         ",
    "  - Any Grade -       47 (35.1%)        0              0       ",
    "  Grade 1-2           47 (35.1%)        0              0       ",
    "  1                   47 (35.1%)        0              0       ",
    "    dcd D.2.1.5.3                                              ",
    "      - Any Grade -   47 (35.1%)        0              0       ",
    "      Grade 1-2       47 (35.1%)        0              0       ",
    "      1               47 (35.1%)        0              0       ",
    "cl C.1                                                         ",
    "  - Any Grade -       43 (32.1%)        0              0       ",
    "  Grade 3-4           43 (32.1%)        0              0       ",
    "  4                   43 (32.1%)        0              0       ",
    "    dcd C.1.1.1.3                                              ",
    "      - Any Grade -   43 (32.1%)        0              0       ",
    "      Grade 3-4       43 (32.1%)        0              0       ",
    "      4               43 (32.1%)        0              0       ",
    "cl C.2                                                         ",
    "  - Any Grade -       35 (26.1%)        0              0       ",
    "  Grade 1-2           35 (26.1%)        0              0       ",
    "  2                   35 (26.1%)        0              0       ",
    "    dcd C.2.1.2.1                                              ",
    "      - Any Grade -   35 (26.1%)        0              0       ",
    "      Grade 1-2       35 (26.1%)        0              0       ",
    "      2               35 (26.1%)        0              0       "
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 2],
    c("122 (91.0%)", "13 (9.7%)")
  )
})

testthat::test_that("AET04 variant 3 is produced correctly (Fill in of Grades)", {
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD", child_labels = "visible", nested = TRUE) %>%
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
    ) %>%
    trim_rows()

  result_matrix <- to_string_matrix(result, with_spaces = TRUE)

  expected_matrix <- c(
    "                       A: Drug X    B: Placebo    C: Combination",
    "                        (N=134)       (N=134)        (N=132)    ",
    "————————————————————————————————————————————————————————————————",
    "- Any Grade -         122 (91.0%)   123 (91.8%)    120 (90.9%)  ",
    "Grade 1-2              13 (9.7%)    19 (14.2%)      11 (8.3%)   ",
    "1                      7 (5.2%)      9 (6.7%)        4 (3.0%)   ",
    "2                      6 (4.5%)      10 (7.5%)       7 (5.3%)   ",
    "Grade 3-4             33 (24.6%)    34 (25.4%)      34 (25.8%)  ",
    "3                     18 (13.4%)    14 (10.4%)      16 (12.1%)  ",
    "4                     15 (11.2%)    20 (14.9%)      18 (13.6%)  ",
    "Grade 5               76 (56.7%)    70 (52.2%)      75 (56.8%)  ",
    "cl A.1                                                          ",
    "  - Any Grade -       78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  Grade 1-2           78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  1                   30 (22.4%)    27 (20.1%)      39 (29.5%)  ",
    "  2                   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "    dcd A.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      Grade 1-2       50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      1               50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "    dcd A.1.1.1.2                                               ",
    "      - Any Grade -   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "      Grade 1-2       48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "      2               48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "cl B.2                                                          ",
    "  - Any Grade -       79 (59.0%)    74 (55.2%)      85 (64.4%)  ",
    "  Grade 1-2           30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
    "  1                   30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
    "  Grade 3-4           49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "  3                   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "    dcd B.2.2.3.1                                               ",
    "      - Any Grade -   48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "      Grade 1-2       48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "      1               48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "    dcd B.2.1.2.1                                               ",
    "      - Any Grade -   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "      Grade 3-4       49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "      3               49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "cl D.1                                                          ",
    "  - Any Grade -       79 (59.0%)    67 (50.0%)      80 (60.6%)  ",
    "  Grade 3-4           29 (21.6%)    25 (18.7%)      29 (22.0%)  ",
    "  3                   29 (21.6%)    25 (18.7%)      29 (22.0%)  ",
    "  Grade 5             50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "    dcd D.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "      Grade 5         50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "    dcd D.1.1.4.2                                               ",
    "      - Any Grade -   48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "      Grade 3-4       48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "      3               48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "cl D.2                                                          ",
    "  - Any Grade -       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  Grade 1-2           47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  1                   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "    dcd D.2.1.5.3                                               ",
    "      - Any Grade -   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      Grade 1-2       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      1               47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "cl B.1                                                          ",
    "  - Any Grade -       47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "  Grade 5             47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "    dcd B.1.1.1.1                                               ",
    "      - Any Grade -   47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "      Grade 5         47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "cl C.2                                                          ",
    "  - Any Grade -       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  Grade 1-2           35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  2                   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "    dcd C.2.1.2.1                                               ",
    "      - Any Grade -   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      Grade 1-2       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      2               35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "cl C.1                                                          ",
    "  - Any Grade -       43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "  Grade 3-4           43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "  4                   43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "    dcd C.1.1.1.3                                               ",
    "      - Any Grade -   43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "      Grade 3-4       43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "      4               43 (32.1%)    46 (34.3%)      43 (32.6%)  "
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination also works (and sorting)
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 2],
    c("122 (91.0%)", "13 (9.7%)")
  )
})

testthat::test_that("AET04 variant 4 is produced correctly (Collapsing of Grades: grades 1&2, grades 3&4&5)", {
  gr_grp_tmp <- list(
    "- Any Grade -" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp_tmp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp_tmp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "- Any Grade -"
    ) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp_tmp[-1],
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

  result_matrix <- to_string_matrix(result, with_spaces = TRUE)

  expected_matrix <- c(
    "                       A: Drug X    B: Placebo    C: Combination",
    "                        (N=134)       (N=134)        (N=132)    ",
    "————————————————————————————————————————————————————————————————",
    "- Any Grade -         122 (91.0%)   123 (91.8%)    120 (90.9%)  ",
    "Grade 1-2              13 (9.7%)    19 (14.2%)      11 (8.3%)   ",
    "1                      7 (5.2%)      9 (6.7%)        4 (3.0%)   ",
    "2                      6 (4.5%)      10 (7.5%)       7 (5.3%)   ",
    "Grade 3-5             109 (81.3%)   104 (77.6%)    109 (82.6%)  ",
    "3                     18 (13.4%)    14 (10.4%)      16 (12.1%)  ",
    "4                     15 (11.2%)    20 (14.9%)      18 (13.6%)  ",
    "5                     76 (56.7%)    70 (52.2%)      75 (56.8%)  ",
    "cl A.1                                                          ",
    "  - Any Grade -       78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  Grade 1-2           78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  1                   30 (22.4%)    27 (20.1%)      39 (29.5%)  ",
    "  2                   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "    dcd A.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      Grade 1-2       50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      1               50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "    dcd A.1.1.1.2                                               ",
    "      - Any Grade -   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "      Grade 1-2       48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "      2               48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "cl B.2                                                          ",
    "  - Any Grade -       79 (59.0%)    74 (55.2%)      85 (64.4%)  ",
    "  Grade 1-2           30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
    "  1                   30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
    "  Grade 3-5           49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "  3                   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "    dcd B.2.2.3.1                                               ",
    "      - Any Grade -   48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "      Grade 1-2       48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "      1               48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
    "    dcd B.2.1.2.1                                               ",
    "      - Any Grade -   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "      Grade 3-5       49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "      3               49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
    "cl D.1                                                          ",
    "  - Any Grade -       79 (59.0%)    67 (50.0%)      80 (60.6%)  ",
    "  Grade 3-5           79 (59.0%)    67 (50.0%)      80 (60.6%)  ",
    "  3                   29 (21.6%)    25 (18.7%)      29 (22.0%)  ",
    "  5                   50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "    dcd D.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "      Grade 3-5       50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "      5               50 (37.3%)    42 (31.3%)      51 (38.6%)  ",
    "    dcd D.1.1.4.2                                               ",
    "      - Any Grade -   48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "      Grade 3-5       48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "      3               48 (35.8%)    42 (31.3%)      50 (37.9%)  ",
    "cl D.2                                                          ",
    "  - Any Grade -       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  Grade 1-2           47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  1                   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "    dcd D.2.1.5.3                                               ",
    "      - Any Grade -   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      Grade 1-2       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      1               47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "cl B.1                                                          ",
    "  - Any Grade -       47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "  Grade 3-5           47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "  5                   47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "    dcd B.1.1.1.1                                               ",
    "      - Any Grade -   47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "      Grade 3-5       47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "      5               47 (35.1%)    49 (36.6%)      43 (32.6%)  ",
    "cl C.2                                                          ",
    "  - Any Grade -       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  Grade 1-2           35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  2                   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "    dcd C.2.1.2.1                                               ",
    "      - Any Grade -   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      Grade 1-2       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      2               35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "cl C.1                                                          ",
    "  - Any Grade -       43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "  Grade 3-5           43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "  4                   43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "    dcd C.1.1.1.3                                               ",
    "      - Any Grade -   43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "      Grade 3-5       43 (32.1%)    46 (34.3%)      43 (32.6%)  ",
    "      4               43 (32.1%)    46 (34.3%)      43 (32.6%)  "
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 5 (Using Worst Grade Flags from ADAE) since it's similar to
# variant 1 (just pre-processing the data).

testthat::test_that("AET04 variant 6 is produced correctly (with an
                    Incidence Rate of at Least 5%, totals restricted)", {
  adae <- get_adae_trimmed(adsl, adae, cutoff_rate = 0.4) %>%
    dplyr::mutate(AETOXGR = droplevels(AETOXGR))

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("AETOXGR"),
      child_labels = "visible", nested = TRUE
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

  result_matrix <- to_string_matrix(result, TRUE)

  expected_matrix <- c(
    "                      A: Drug X    B: Placebo    C: Combination",
    "                       (N=134)       (N=134)        (N=132)    ",
    "———————————————————————————————————————————————————————————————",
    "- Any Grade -         92 (68.7%)   104 (77.6%)    107 (81.1%)  ",
    "Grade 1-2             92 (68.7%)   104 (77.6%)    107 (81.1%)  ",
    "1                     57 (42.5%)   56 (41.8%)      52 (39.4%)  ",
    "2                     35 (26.1%)   48 (35.8%)      55 (41.7%)  ",
    "cl D.2                                                         ",
    "  - Any Grade -       47 (35.1%)   58 (43.3%)      57 (43.2%)  ",
    "  Grade 1-2           47 (35.1%)   58 (43.3%)      57 (43.2%)  ",
    "  1                   47 (35.1%)   58 (43.3%)      57 (43.2%)  ",
    "    dcd D.2.1.5.3                                              ",
    "      - Any Grade -   47 (35.1%)   58 (43.3%)      57 (43.2%)  ",
    "      Grade 1-2       47 (35.1%)   58 (43.3%)      57 (43.2%)  ",
    "      1               47 (35.1%)   58 (43.3%)      57 (43.2%)  ",
    "cl A.1                                                         ",
    "  - Any Grade -       50 (37.3%)   45 (33.6%)      63 (47.7%)  ",
    "  Grade 1-2           50 (37.3%)   45 (33.6%)      63 (47.7%)  ",
    "  1                   50 (37.3%)   45 (33.6%)      63 (47.7%)  ",
    "    dcd A.1.1.1.1                                              ",
    "      - Any Grade -   50 (37.3%)   45 (33.6%)      63 (47.7%)  ",
    "      Grade 1-2       50 (37.3%)   45 (33.6%)      63 (47.7%)  ",
    "      1               50 (37.3%)   45 (33.6%)      63 (47.7%)  ",
    "cl B.2                                                         ",
    "  - Any Grade -       48 (35.8%)   54 (40.3%)      51 (38.6%)  ",
    "  Grade 1-2           48 (35.8%)   54 (40.3%)      51 (38.6%)  ",
    "  1                   48 (35.8%)   54 (40.3%)      51 (38.6%)  ",
    "    dcd B.2.2.3.1                                              ",
    "      - Any Grade -   48 (35.8%)   54 (40.3%)      51 (38.6%)  ",
    "      Grade 1-2       48 (35.8%)   54 (40.3%)      51 (38.6%)  ",
    "      1               48 (35.8%)   54 (40.3%)      51 (38.6%)  ",
    "cl C.2                                                         ",
    "  - Any Grade -       35 (26.1%)   48 (35.8%)      55 (41.7%)  ",
    "  Grade 1-2           35 (26.1%)   48 (35.8%)      55 (41.7%)  ",
    "  2                   35 (26.1%)   48 (35.8%)      55 (41.7%)  ",
    "    dcd C.2.1.2.1                                              ",
    "      - Any Grade -   35 (26.1%)   48 (35.8%)      55 (41.7%)  ",
    "      Grade 1-2       35 (26.1%)   48 (35.8%)      55 (41.7%)  ",
    "      2               35 (26.1%)   48 (35.8%)      55 (41.7%)  "
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 7, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X Patients, totals unrestriced).
# With this variant, the SOC level is not trimmed (even if there are no terms left).

# This function is needed to check only the inner loops as the first piece should
# not be checked and filtered out by prune_table.
my_row_condition <- function(table_row) {
  if (indent_mod(table_row) == 0) {
    return(TRUE)
  } else {
    row_condition(table_row)
  }
}

# NOTE: STREAM logic will only trim at term level
testthat::test_that("AET04 variant 8 is produced correctly (with an Incidence Rate of at Least X Patients)", {
  raw_result <- raw_table(adae, adsl)

  cutoff <- 58L
  row_condition <- has_count_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

  result <- prune_table(raw_result, keep_rows(my_row_condition))

  result_matrix <- to_string_matrix(result, with_spaces = TRUE)

  expected_matrix <- c(
    "                       A: Drug X    B: Placebo    C: Combination",
    "                        (N=134)       (N=134)        (N=132)    ",
    "————————————————————————————————————————————————————————————————",
    "- Any Grade -         122 (91.0%)   123 (91.8%)    120 (90.9%)  ",
    "Grade 1-2              13 (9.7%)    19 (14.2%)      11 (8.3%)   ",
    "1                      7 (5.2%)      9 (6.7%)        4 (3.0%)   ",
    "2                      6 (4.5%)      10 (7.5%)       7 (5.3%)   ",
    "Grade 3-4             33 (24.6%)    34 (25.4%)      34 (25.8%)  ",
    "3                     18 (13.4%)    14 (10.4%)      16 (12.1%)  ",
    "4                     15 (11.2%)    20 (14.9%)      18 (13.6%)  ",
    "Grade 5               76 (56.7%)    70 (52.2%)      75 (56.8%)  ",
    "cl A.1                                                          ",
    "  - Any Grade -       78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  Grade 1-2           78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  1                   30 (22.4%)    27 (20.1%)      39 (29.5%)  ",
    "  2                   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "    dcd A.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      Grade 1-2       50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      1               50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "cl D.2                                                          ",
    "  - Any Grade -       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  Grade 1-2           47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "  1                   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "    dcd D.2.1.5.3                                               ",
    "      - Any Grade -   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      Grade 1-2       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
    "      1               47 (35.1%)    58 (43.3%)      57 (43.2%)  "
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# NOTE: STREAM logic will only stream at term level
testthat::test_that("AET04 variant 9 is produced correctlyb(with a Difference in Incidence Rate of at Least X%)", {
  raw_result <- raw_table(adae, adsl)

  cutoff <- 0.1
  row_condition <- has_fractions_difference(atleast = cutoff, col_names = names(raw_result))

  result <- prune_table(raw_result, keep_rows(my_row_condition))

  result_matrix <- to_string_matrix(result, with_spaces = TRUE)

  expected_matrix <- c(
    "                       A: Drug X    B: Placebo    C: Combination",
    "                        (N=134)       (N=134)        (N=132)    ",
    "————————————————————————————————————————————————————————————————",
    "- Any Grade -         122 (91.0%)   123 (91.8%)    120 (90.9%)  ",
    "Grade 1-2              13 (9.7%)    19 (14.2%)      11 (8.3%)   ",
    "1                      7 (5.2%)      9 (6.7%)        4 (3.0%)   ",
    "2                      6 (4.5%)      10 (7.5%)       7 (5.3%)   ",
    "Grade 3-4             33 (24.6%)    34 (25.4%)      34 (25.8%)  ",
    "3                     18 (13.4%)    14 (10.4%)      16 (12.1%)  ",
    "4                     15 (11.2%)    20 (14.9%)      18 (13.6%)  ",
    "Grade 5               76 (56.7%)    70 (52.2%)      75 (56.8%)  ",
    "cl A.1                                                          ",
    "  - Any Grade -       78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  Grade 1-2           78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
    "  1                   30 (22.4%)    27 (20.1%)      39 (29.5%)  ",
    "  2                   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
    "    dcd A.1.1.1.1                                               ",
    "      - Any Grade -   50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      Grade 1-2       50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "      1               50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
    "cl C.2                                                          ",
    "  - Any Grade -       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  Grade 1-2           35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "  2                   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "    dcd C.2.1.2.1                                               ",
    "      - Any Grade -   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      Grade 1-2       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
    "      2               35 (26.1%)    48 (35.8%)      55 (41.7%)  "
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  # Pagination works
  testthat::expect_silent(
    pag_result <- paginate_table(result, lpp = 15)
  )
})

# No test done for variant 10, Adverse Events by Highest NCI CTCAE Grade
# (with an Incidence Rate of at Least X%, SOCs below X% removed).
# With this variant, SOC levels above the threshold are still in the table even if
# there are no terms left.

testthat::test_that(
  "AET04 variant 11 is produced correctly
  (with an Incidence Rate of at Least X%, all SOCs w/o preferred terms removed)",
  {
    raw_result <- raw_table(adae, adsl)

    cutoff <- 0.4
    row_condition <- has_fraction_in_any_col(atleast = cutoff, col_names = levels(adsl$ACTARM))

    result <- prune_table(raw_result, keep_rows(my_row_condition))

    result_matrix <- to_string_matrix(result, with_spaces = TRUE, T)

    expected_matrix <- c(
      "                       A: Drug X    B: Placebo    C: Combination",
      "                        (N=134)       (N=134)        (N=132)    ",
      "————————————————————————————————————————————————————————————————",
      "- Any Grade -         122 (91.0%)   123 (91.8%)    120 (90.9%)  ",
      "Grade 1-2              13 (9.7%)    19 (14.2%)      11 (8.3%)   ",
      "1                      7 (5.2%)      9 (6.7%)        4 (3.0%)   ",
      "2                      6 (4.5%)      10 (7.5%)       7 (5.3%)   ",
      "Grade 3-4             33 (24.6%)    34 (25.4%)      34 (25.8%)  ",
      "3                     18 (13.4%)    14 (10.4%)      16 (12.1%)  ",
      "4                     15 (11.2%)    20 (14.9%)      18 (13.6%)  ",
      "Grade 5               76 (56.7%)    70 (52.2%)      75 (56.8%)  ",
      "cl A.1                                                          ",
      "  - Any Grade -       78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
      "  Grade 1-2           78 (58.2%)    75 (56.0%)      89 (67.4%)  ",
      "  1                   30 (22.4%)    27 (20.1%)      39 (29.5%)  ",
      "  2                   48 (35.8%)    48 (35.8%)      50 (37.9%)  ",
      "    dcd A.1.1.1.1                                               ",
      "      - Any Grade -   50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
      "      Grade 1-2       50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
      "      1               50 (37.3%)    45 (33.6%)      63 (47.7%)  ",
      "cl B.2                                                          ",
      "  - Any Grade -       79 (59.0%)    74 (55.2%)      85 (64.4%)  ",
      "  Grade 1-2           30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
      "  1                   30 (22.4%)    30 (22.4%)      33 (25.0%)  ",
      "  Grade 3-4           49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
      "  3                   49 (36.6%)    44 (32.8%)      52 (39.4%)  ",
      "    dcd B.2.2.3.1                                               ",
      "      - Any Grade -   48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
      "      Grade 1-2       48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
      "      1               48 (35.8%)    54 (40.3%)      51 (38.6%)  ",
      "cl D.2                                                          ",
      "  - Any Grade -       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
      "  Grade 1-2           47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
      "  1                   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
      "    dcd D.2.1.5.3                                               ",
      "      - Any Grade -   47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
      "      Grade 1-2       47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
      "      1               47 (35.1%)    58 (43.3%)      57 (43.2%)  ",
      "cl C.2                                                          ",
      "  - Any Grade -       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
      "  Grade 1-2           35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
      "  2                   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
      "    dcd C.2.1.2.1                                               ",
      "      - Any Grade -   35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
      "      Grade 1-2       35 (26.1%)    48 (35.8%)      55 (41.7%)  ",
      "      2               35 (26.1%)    48 (35.8%)      55 (41.7%)  "
    )
    testthat::expect_identical(result_matrix, expected_matrix)

    # Pagination works
    testthat::expect_silent(
      pag_result <- paginate_table(result, lpp = 15)
    )
  }
)
