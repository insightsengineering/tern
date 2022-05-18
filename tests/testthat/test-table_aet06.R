# Test variants 1 and 5 for AET06. Variants 2-4 are essentially identical
# to variant 1, needing only the data frame to be pre-processed
# and the baseline variable to be changed from SEX.

library(scda)
library(magrittr)
adsl <- synthetic_cdisc_data("latest")$adsl
adae <- synthetic_cdisc_data("latest")$adae


testthat::test_that("AET06 variant 1 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
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
    count_occurrences(vars = "AEDECOD")
  result <- build_table(lyt, adae, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "", "Total number of patients with at least one adverse event",
      "Overall total number of events", "cl A.1",
      "Total number of patients with at least one adverse event",
      "Total number of events", "dcd A.1.1.1.1", "dcd A.1.1.1.2", "cl B.1",
      "Total number of patients with at least one adverse event", "Total number of events",
      "dcd B.1.1.1.1", "cl B.2", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd B.2.1.2.1", "dcd B.2.2.3.1", "cl C.1",
      "Total number of patients with at least one adverse event", "Total number of events",
      "dcd C.1.1.1.3", "cl C.2", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd C.2.1.2.1", "cl D.1",
      "Total number of patients with at least one adverse event",
      "Total number of events", "dcd D.1.1.1.1", "dcd D.1.1.4.2", "cl D.2",
      "Total number of patients with at least one adverse event", "Total number of events",
      "dcd D.2.1.5.3", "A: Drug X", "F", "(N=79)", "72 (91.1%)", "377",
      "", "53 (67.1%)", "85", "34 (43%)", "32 (40.5%)", "", "28 (35.4%)",
      "33", "28 (35.4%)", "", "46 (58.2%)", "81", "29 (36.7%)", "30 (38%)",
      "", "30 (38%)", "39", "30 (38%)", "", "23 (29.1%)", "32", "23 (29.1%)",
      "", "45 (57%)", "72", "25 (31.6%)", "30 (38%)", "", "26 (32.9%)",
      "35", "26 (32.9%)", "A: Drug X", "M", "(N=55)", "50 (90.9%)",
      "232", "", "25 (45.5%)", "47", "16 (29.1%)", "16 (29.1%)", "",
      "19 (34.5%)", "23", "19 (34.5%)", "", "33 (60%)", "48", "20 (36.4%)",
      "18 (32.7%)", "", "13 (23.6%)", "16", "13 (23.6%)", "", "12 (21.8%)",
      "16", "12 (21.8%)", "", "34 (61.8%)", "55", "25 (45.5%)", "18 (32.7%)",
      "", "21 (38.2%)", "27", "21 (38.2%)", "B: Placebo", "F", "(N=82)",
      "77 (93.9%)", "419", "", "51 (62.2%)", "93", "31 (37.8%)", "33 (40.2%)",
      "", "33 (40.2%)", "36", "33 (40.2%)", "", "45 (54.9%)", "86",
      "30 (36.6%)", "32 (39%)", "", "36 (43.9%)", "52", "36 (43.9%)",
      "", "36 (43.9%)", "39", "36 (43.9%)", "", "40 (48.8%)", "64",
      "29 (35.4%)", "22 (26.8%)", "", "40 (48.8%)", "49", "40 (48.8%)",
      "B: Placebo", "M", "(N=52)", "46 (88.5%)", "203", "", "24 (46.2%)",
      "37", "14 (26.9%)", "15 (28.8%)", "", "16 (30.8%)", "24", "16 (30.8%)",
      "", "29 (55.8%)", "52", "14 (26.9%)", "22 (42.3%)", "", "10 (19.2%)",
      "11", "10 (19.2%)", "", "12 (23.1%)", "14", "12 (23.1%)", "",
      "27 (51.9%)", "42", "13 (25%)", "20 (38.5%)", "", "18 (34.6%)",
      "23", "18 (34.6%)", "C: Combination", "F", "(N=70)", "65 (92.9%)",
      "378", "", "43 (61.4%)", "86", "33 (47.1%)", "24 (34.3%)", "",
      "24 (34.3%)", "35", "24 (34.3%)", "", "44 (62.9%)", "64", "22 (31.4%)",
      "26 (37.1%)", "", "27 (38.6%)", "44", "27 (38.6%)", "", "30 (42.9%)",
      "33", "30 (42.9%)", "", "41 (58.6%)", "73", "27 (38.6%)", "27 (38.6%)",
      "", "34 (48.6%)", "43", "34 (48.6%)", "C: Combination", "M",
      "(N=62)", "55 (88.7%)", "325", "", "46 (74.2%)", "74", "30 (48.4%)",
      "26 (41.9%)", "", "19 (30.6%)", "27", "19 (30.6%)", "", "41 (66.1%)",
      "79", "30 (48.4%)", "25 (40.3%)", "", "16 (25.8%)", "20", "16 (25.8%)",
      "", "25 (40.3%)", "32", "25 (40.3%)", "", "39 (62.9%)", "62",
      "24 (38.7%)", "23 (37.1%)", "", "23 (37.1%)", "31", "23 (37.1%)"
    ),
    .Dim = c(36L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET06 variant 3 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
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
      indent_mod = -1L,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
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
      indent_mod = -1L,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(adae$AEHLT)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences("AEDECOD", .indent_mods = -1L) %>%
    append_varlabels(adae, "AEDECOD", indent = 2L)

  result <- build_table(
    lyt = lyt,
    df = adae,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    # Sorted by decreasing frequency across all groups by System Organ Class, High-Level Term and Preferred Term.
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEHLT"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"),
      scorefun = score_occurrences
    )
  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Body System or Organ Class", "A: Drug X", "A: Drug X", "B: Placebo", "B: Placebo", "C: Combination", "C: Combination", # nolint
    "  High Level Term", "F", "M", "F", "M", "F", "M",
    "    Dictionary-Derived Term", "(N=79)", "(N=55)", "(N=82)", "(N=52)", "(N=70)", "(N=62)",
    "Total number of patients with at least one adverse event", "72 (91.1%)", "50 (90.9%)", "77 (93.9%)", "46 (88.5%)", "65 (92.9%)", "55 (88.7%)", # nolint
    "Overall total number of events", "377", "232", "419", "203", "378", "325",
    "cl A.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "53 (67.1%)", "25 (45.5%)", "51 (62.2%)", "24 (46.2%)", "43 (61.4%)", "46 (74.2%)", # nolint
    "Total number of events", "85", "47", "93", "37", "86", "74",
    "hlt A.1.1.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "53 (67.1%)", "25 (45.5%)", "51 (62.2%)", "24 (46.2%)", "43 (61.4%)", "46 (74.2%)", # nolint
    "Total number of events", "85", "47", "93", "37", "86", "74",
    "dcd A.1.1.1.1", "34 (43%)", "16 (29.1%)", "31 (37.8%)", "14 (26.9%)", "33 (47.1%)", "30 (48.4%)",
    "dcd A.1.1.1.2", "32 (40.5%)", "16 (29.1%)", "33 (40.2%)", "15 (28.8%)", "24 (34.3%)", "26 (41.9%)",
    "cl B.2", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "46 (58.2%)", "33 (60%)", "45 (54.9%)", "29 (55.8%)", "44 (62.9%)", "41 (66.1%)", # nolint
    "Total number of events", "81", "48", "86", "52", "64", "79",
    "hlt B.2.2.3", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "30 (38%)", "18 (32.7%)", "32 (39%)", "22 (42.3%)", "26 (37.1%)", "25 (40.3%)", # nolint
    "Total number of events", "40", "24", "43", "33", "37", "40",
    "dcd B.2.2.3.1", "30 (38%)", "18 (32.7%)", "32 (39%)", "22 (42.3%)", "26 (37.1%)", "25 (40.3%)",
    "hlt B.2.1.2", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "29 (36.7%)", "20 (36.4%)", "30 (36.6%)", "14 (26.9%)", "22 (31.4%)", "30 (48.4%)", # nolint
    "Total number of events", "41", "24", "43", "19", "27", "39",
    "dcd B.2.1.2.1", "29 (36.7%)", "20 (36.4%)", "30 (36.6%)", "14 (26.9%)", "22 (31.4%)", "30 (48.4%)",
    "cl D.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "45 (57%)", "34 (61.8%)", "40 (48.8%)", "27 (51.9%)", "41 (58.6%)", "39 (62.9%)", # nolint
    "Total number of events", "72", "55", "64", "42", "73", "62",
    "hlt D.1.1.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "25 (31.6%)", "25 (45.5%)", "29 (35.4%)", "13 (25%)", "27 (38.6%)", "24 (38.7%)", # nolint
    "Total number of events", "32", "29", "36", "15", "39", "32",
    "dcd D.1.1.1.1", "25 (31.6%)", "25 (45.5%)", "29 (35.4%)", "13 (25%)", "27 (38.6%)", "24 (38.7%)",
    "hlt D.1.1.4", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "30 (38%)", "18 (32.7%)", "22 (26.8%)", "20 (38.5%)", "27 (38.6%)", "23 (37.1%)", # nolint
    "Total number of events", "40", "26", "28", "27", "34", "30",
    "dcd D.1.1.4.2", "30 (38%)", "18 (32.7%)", "22 (26.8%)", "20 (38.5%)", "27 (38.6%)", "23 (37.1%)",
    "cl D.2", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "26 (32.9%)", "21 (38.2%)", "40 (48.8%)", "18 (34.6%)", "34 (48.6%)", "23 (37.1%)", # nolint
    "Total number of events", "35", "27", "49", "23", "43", "31",
    "hlt D.2.1.5", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "26 (32.9%)", "21 (38.2%)", "40 (48.8%)", "18 (34.6%)", "34 (48.6%)", "23 (37.1%)", # nolint
    "Total number of events", "35", "27", "49", "23", "43", "31",
    "dcd D.2.1.5.3", "26 (32.9%)", "21 (38.2%)", "40 (48.8%)", "18 (34.6%)", "34 (48.6%)", "23 (37.1%)",
    "cl B.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "28 (35.4%)", "19 (34.5%)", "33 (40.2%)", "16 (30.8%)", "24 (34.3%)", "19 (30.6%)", # nolint
    "Total number of events", "33", "23", "36", "24", "35", "27",
    "hlt B.1.1.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "28 (35.4%)", "19 (34.5%)", "33 (40.2%)", "16 (30.8%)", "24 (34.3%)", "19 (30.6%)", # nolint
    "Total number of events", "33", "23", "36", "24", "35", "27",
    "dcd B.1.1.1.1", "28 (35.4%)", "19 (34.5%)", "33 (40.2%)", "16 (30.8%)", "24 (34.3%)", "19 (30.6%)",
    "cl C.2", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "23 (29.1%)", "12 (21.8%)", "36 (43.9%)", "12 (23.1%)", "30 (42.9%)", "25 (40.3%)", # nolint
    "Total number of events", "32", "16", "39", "14", "33", "32",
    "hlt C.2.1.2", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "23 (29.1%)", "12 (21.8%)", "36 (43.9%)", "12 (23.1%)", "30 (42.9%)", "25 (40.3%)", # nolint
    "Total number of events", "32", "16", "39", "14", "33", "32",
    "dcd C.2.1.2.1", "23 (29.1%)", "12 (21.8%)", "36 (43.9%)", "12 (23.1%)", "30 (42.9%)", "25 (40.3%)",
    "cl C.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "30 (38%)", "13 (23.6%)", "36 (43.9%)", "10 (19.2%)", "27 (38.6%)", "16 (25.8%)", # nolint
    "Total number of events", "39", "16", "52", "11", "44", "20",
    "hlt C.1.1.1", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "30 (38%)", "13 (23.6%)", "36 (43.9%)", "10 (19.2%)", "27 (38.6%)", "16 (25.8%)", # nolint
    "Total number of events", "39", "16", "52", "11", "44", "20",
    "dcd C.1.1.1.3", "30 (38%)", "13 (23.6%)", "36 (43.9%)", "10 (19.2%)", "27 (38.6%)", "16 (25.8%)"
  )

  expected_matrix <- matrix(expected_matrix, nrow = 63, ncol = 7, byrow = TRUE)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET06 variant 5 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%
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
    count_occurrences("AEDECOD")
  result <- build_table(lyt, adae, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "", "Total number of patients with at least one adverse event",
      "Overall total number of events", "cl A.1", "Total number of patients with at least one adverse event",
      "Total number of events", "hlt A.1.1.1", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd A.1.1.1.1", "dcd A.1.1.1.2", "cl B.1",
      "Total number of patients with at least one adverse event", "Total number of events",
      "hlt B.1.1.1", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd B.1.1.1.1", "cl B.2", "Total number of patients with at least one adverse event",
      "Total number of events", "hlt B.2.1.2", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd B.2.1.2.1", "hlt B.2.2.3",
      "Total number of patients with at least one adverse event",
      "Total number of events", "dcd B.2.2.3.1", "cl C.1", "Total number of patients with at least one adverse event",
      "Total number of events", "hlt C.1.1.1", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd C.1.1.1.3", "cl C.2", "Total number of patients with at least one adverse event",
      "Total number of events", "hlt C.2.1.2", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd C.2.1.2.1", "cl D.1", "Total number of patients with at least one adverse event",
      "Total number of events", "hlt D.1.1.1", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd D.1.1.1.1", "hlt D.1.1.4",
      "Total number of patients with at least one adverse event",
      "Total number of events", "dcd D.1.1.4.2", "cl D.2", "Total number of patients with at least one adverse event",
      "Total number of events", "hlt D.2.1.5", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd D.2.1.5.3", "A: Drug X", "F",
      "(N=79)", "72 (91.1%)", "377", "", "53 (67.1%)", "85", "", "53 (67.1%)",
      "85", "34 (43%)", "32 (40.5%)", "", "28 (35.4%)", "33", "", "28 (35.4%)",
      "33", "28 (35.4%)", "", "46 (58.2%)", "81", "", "29 (36.7%)",
      "41", "29 (36.7%)", "", "30 (38%)", "40", "30 (38%)", "", "30 (38%)",
      "39", "", "30 (38%)", "39", "30 (38%)", "", "23 (29.1%)", "32",
      "", "23 (29.1%)", "32", "23 (29.1%)", "", "45 (57%)", "72", "",
      "25 (31.6%)", "32", "25 (31.6%)", "", "30 (38%)", "40", "30 (38%)",
      "", "26 (32.9%)", "35", "", "26 (32.9%)", "35", "26 (32.9%)",
      "A: Drug X", "M", "(N=55)", "50 (90.9%)", "232", "", "25 (45.5%)",
      "47", "", "25 (45.5%)", "47", "16 (29.1%)", "16 (29.1%)", "",
      "19 (34.5%)", "23", "", "19 (34.5%)", "23", "19 (34.5%)", "",
      "33 (60%)", "48", "", "20 (36.4%)", "24", "20 (36.4%)", "", "18 (32.7%)",
      "24", "18 (32.7%)", "", "13 (23.6%)", "16", "", "13 (23.6%)",
      "16", "13 (23.6%)", "", "12 (21.8%)", "16", "", "12 (21.8%)",
      "16", "12 (21.8%)", "", "34 (61.8%)", "55", "", "25 (45.5%)",
      "29", "25 (45.5%)", "", "18 (32.7%)", "26", "18 (32.7%)", "",
      "21 (38.2%)", "27", "", "21 (38.2%)", "27", "21 (38.2%)", "B: Placebo",
      "F", "(N=82)", "77 (93.9%)", "419", "", "51 (62.2%)", "93", "",
      "51 (62.2%)", "93", "31 (37.8%)", "33 (40.2%)", "", "33 (40.2%)",
      "36", "", "33 (40.2%)", "36", "33 (40.2%)", "", "45 (54.9%)",
      "86", "", "30 (36.6%)", "43", "30 (36.6%)", "", "32 (39%)", "43",
      "32 (39%)", "", "36 (43.9%)", "52", "", "36 (43.9%)", "52", "36 (43.9%)",
      "", "36 (43.9%)", "39", "", "36 (43.9%)", "39", "36 (43.9%)",
      "", "40 (48.8%)", "64", "", "29 (35.4%)", "36", "29 (35.4%)",
      "", "22 (26.8%)", "28", "22 (26.8%)", "", "40 (48.8%)", "49",
      "", "40 (48.8%)", "49", "40 (48.8%)", "B: Placebo", "M", "(N=52)",
      "46 (88.5%)", "203", "", "24 (46.2%)", "37", "", "24 (46.2%)",
      "37", "14 (26.9%)", "15 (28.8%)", "", "16 (30.8%)", "24", "",
      "16 (30.8%)", "24", "16 (30.8%)", "", "29 (55.8%)", "52", "",
      "14 (26.9%)", "19", "14 (26.9%)", "", "22 (42.3%)", "33", "22 (42.3%)",
      "", "10 (19.2%)", "11", "", "10 (19.2%)", "11", "10 (19.2%)",
      "", "12 (23.1%)", "14", "", "12 (23.1%)", "14", "12 (23.1%)",
      "", "27 (51.9%)", "42", "", "13 (25%)", "15", "13 (25%)", "",
      "20 (38.5%)", "27", "20 (38.5%)", "", "18 (34.6%)", "23", "",
      "18 (34.6%)", "23", "18 (34.6%)", "C: Combination", "F", "(N=70)",
      "65 (92.9%)", "378", "", "43 (61.4%)", "86", "", "43 (61.4%)",
      "86", "33 (47.1%)", "24 (34.3%)", "", "24 (34.3%)", "35", "",
      "24 (34.3%)", "35", "24 (34.3%)", "", "44 (62.9%)", "64", "",
      "22 (31.4%)", "27", "22 (31.4%)", "", "26 (37.1%)", "37", "26 (37.1%)",
      "", "27 (38.6%)", "44", "", "27 (38.6%)", "44", "27 (38.6%)",
      "", "30 (42.9%)", "33", "", "30 (42.9%)", "33", "30 (42.9%)",
      "", "41 (58.6%)", "73", "", "27 (38.6%)", "39", "27 (38.6%)",
      "", "27 (38.6%)", "34", "27 (38.6%)", "", "34 (48.6%)", "43",
      "", "34 (48.6%)", "43", "34 (48.6%)", "C: Combination", "M",
      "(N=62)", "55 (88.7%)", "325", "", "46 (74.2%)", "74", "", "46 (74.2%)",
      "74", "30 (48.4%)", "26 (41.9%)", "", "19 (30.6%)", "27", "",
      "19 (30.6%)", "27", "19 (30.6%)", "", "41 (66.1%)", "79", "",
      "30 (48.4%)", "39", "30 (48.4%)", "", "25 (40.3%)", "40", "25 (40.3%)",
      "", "16 (25.8%)", "20", "", "16 (25.8%)", "20", "16 (25.8%)",
      "", "25 (40.3%)", "32", "", "25 (40.3%)", "32", "25 (40.3%)",
      "", "39 (62.9%)", "62", "", "24 (38.7%)", "32", "24 (38.7%)",
      "", "23 (37.1%)", "30", "23 (37.1%)", "", "23 (37.1%)", "31",
      "", "23 (37.1%)", "31", "23 (37.1%)"
    ),
    .Dim = c(63L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
