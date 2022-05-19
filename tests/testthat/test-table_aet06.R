# Test variants 1 and 5 for AET06. Variants 2-4 are essentially identical
# to variant 1, needing only the data frame to be pre-processed
# and the baseline variable to be changed from SEX.

library(scda)
library(magrittr)
adsl <- synthetic_cdisc_data("latest")$adsl
adae <- synthetic_cdisc_data("latest")$adae
adsub <- synthetic_cdisc_data("latest")$adsub
adsub_bmi <- adsub %>%
  filter(PARAMCD == "BBMISI") %>%
  select(STUDYID, USUBJID, AVALCAT1) %>%
  mutate(
    AVALCAT1 = factor(AVALCAT1, levels = c("<18.5", "18.5 - 24.9", "25 - 29.9", ">30"))
  )
adsl <- adsl %>%
  mutate(
    RACE1 = case_when(
      RACE == "WHITE" ~ "WHITE",
      TRUE ~ "NON-WHITE"
    ),
    RACE1 = factor(
      RACE1,
      levels = c("WHITE", "NON-WHITE")
    )
  ) %>%
  left_join(
    y = adsub_bmi,
    by = c("STUDYID", "USUBJID")
  )

adae_labels <- var_labels(adae)

adae <- adae %>%
  mutate(
    RACE1 = case_when(
      RACE == "WHITE" ~ "WHITE",
      TRUE ~ "NON-WHITE"
    ),
    RACE1 = factor(
      RACE1,
      levels = c("WHITE", "NON-WHITE")
    )
  ) %>%
  left_join(
    y = adsub_bmi,
    by = c("STUDYID", "USUBJID")
  )


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
    split_cols_by("AVALCAT1") %>%
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
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)


  result <- build_table(
    lyt = lyt,
    df = adae,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    # Sorted by decreasing frequency across all groups by System Organ Class and Preferred Term.
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )
  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Body System or Organ Class", "A: Drug X", "A: Drug X", "A: Drug X", "A: Drug X", "B: Placebo", "B: Placebo", "B: Placebo", # nolint
    "B: Placebo", "C: Combination", "C: Combination", "C: Combination", "C: Combination",
    "  Dictionary-Derived Term", "<18.5", "18.5 - 24.9", "25 - 29.9", ">30", "<18.5", "18.5 - 24.9", "25 - 29.9", ">30",
    "<18.5", "18.5 - 24.9", "25 - 29.9", ">30",
    "", "(N=44)", "(N=17)", "(N=11)", "(N=62)", "(N=37)", "(N=18)", "(N=10)", "(N=69)", "(N=28)", "(N=20)", "(N=18)", "(N=66)",# nolint
    "Total number of patients with at least one adverse event", "41 (93.2%)", "14 (82.4%)", "11 (100%)", "56 (90.3%)",
    "35 (94.6%)", "16 (88.9%)", "9 (90%)", "63 (91.3%)", "25 (89.3%)", "19 (95%)", "15 (83.3%)", "61 (92.4%)",
    "Overall total number of events", "186", "80", "66", "277", "174", "89", "47", "312", "137", "129", "100", "337",
    "cl A.1", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "23 (52.3%)", "12 (70.6%)", "7 (63.6%)", "36 (58.1%)",
    "19 (51.4%)", "10 (55.6%)", "6 (60%)", "40 (58%)", "16 (57.1%)", "13 (65%)", "14 (77.8%)", "46 (69.7%)",
    "Total number of events", "38", "22", "13", "59", "35", "22", "6", "67", "30", "23", "30", "77",
    "dcd A.1.1.1.1", "14 (31.8%)", "7 (41.2%)", "3 (27.3%)", "26 (41.9%)", "12 (32.4%)", "6 (33.3%)", "4 (40%)", "23 (33.3%)", # nolint
    "12 (42.9%)", "7 (35%)", "13 (72.2%)", "31 (47%)",
    "dcd A.1.1.1.2", "15 (34.1%)", "8 (47.1%)", "5 (45.5%)", "20 (32.3%)", "12 (32.4%)", "8 (44.4%)", "2 (20%)", "26 (37.7%)", # nolint
    "9 (32.1%)", "7 (35%)", "7 (38.9%)", "27 (40.9%)",
    "cl B.2", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "23 (52.3%)", "10 (58.8%)", "8 (72.7%)", "38 (61.3%)",
    "21 (56.8%)", "8 (44.4%)", "7 (70%)", "38 (55.1%)", "16 (57.1%)", "16 (80%)", "12 (66.7%)", "41 (62.1%)",
    "Total number of events", "37", "19", "12", "61", "41", "18", "12", "67", "28", "28", "18", "69",
    "dcd B.2.2.3.1", "13 (29.5%)", "7 (41.2%)", "6 (54.5%)", "22 (35.5%)", "14 (37.8%)", "7 (38.9%)", "6 (60%)", "27 (39.1%)", # nolint
    "7 (25%)", "13 (65%)", "5 (27.8%)", "26 (39.4%)",
    "dcd B.2.1.2.1", "14 (31.8%)", "8 (47.1%)", "4 (36.4%)", "23 (37.1%)", "14 (37.8%)", "6 (33.3%)", "4 (40%)", "20 (29%)", # nolint
    "13 (46.4%)", "8 (40%)", "8 (44.4%)", "23 (34.8%)",
    "cl D.1", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "24 (54.5%)", "9 (52.9%)", "8 (72.7%)", "38 (61.3%)",
    "18 (48.6%)", "5 (27.8%)", "7 (70%)", "37 (53.6%)", "15 (53.6%)", "12 (60%)", "11 (61.1%)", "42 (63.6%)",
    "Total number of events", "38", "11", "17", "61", "30", "11", "14", "51", "30", "23", "16", "66",
    "dcd D.1.1.1.1", "13 (29.5%)", "7 (41.2%)", "7 (63.6%)", "23 (37.1%)", "13 (35.1%)", "3 (16.7%)", "4 (40%)", "22 (31.9%)", # nolint
    "12 (42.9%)", "6 (30%)", "8 (44.4%)", "25 (37.9%)",
    "dcd D.1.1.4.2", "16 (36.4%)", "4 (23.5%)", "6 (54.5%)", "22 (35.5%)", "10 (27%)", "4 (22.2%)", "5 (50%)", "23 (33.3%)", # nolint
    "10 (35.7%)", "9 (45%)", "6 (33.3%)", "25 (37.9%)",
    "cl D.2", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "14 (31.8%)", "7 (41.2%)", "5 (45.5%)", "21 (33.9%)",
    "14 (37.8%)", "10 (55.6%)", "4 (40%)", "30 (43.5%)", "13 (46.4%)", "10 (50%)", "7 (38.9%)", "27 (40.9%)",
    "Total number of events", "20", "7", "10", "25", "17", "14", "5", "36", "18", "15", "10", "31",
    "dcd D.2.1.5.3", "14 (31.8%)", "7 (41.2%)", "5 (45.5%)", "21 (33.9%)", "14 (37.8%)", "10 (55.6%)", "4 (40%)", "30 (43.5%)", # nolint
    "13 (46.4%)", "10 (50%)", "7 (38.9%)", "27 (40.9%)",
    "cl B.1", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "17 (38.6%)", "5 (29.4%)", "7 (63.6%)", "18 (29%)",
    "14 (37.8%)", "8 (44.4%)", "4 (40%)", "23 (33.3%)", "7 (25%)", "10 (50%)", "7 (38.9%)", "19 (28.8%)",
    "Total number of events", "22", "5", "7", "22", "16", "11", "4", "29", "11", "12", "8", "31",
    "dcd B.1.1.1.1", "17 (38.6%)", "5 (29.4%)", "7 (63.6%)", "18 (29%)", "14 (37.8%)", "8 (44.4%)", "4 (40%)", "23 (33.3%)", # nolint
    "7 (25%)", "10 (50%)", "7 (38.9%)", "19 (28.8%)",
    "cl C.2", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "14 (31.8%)", "4 (23.5%)", "2 (18.2%)", "15 (24.2%)",
    "15 (40.5%)", "5 (27.8%)", "4 (40%)", "24 (34.8%)", "10 (35.7%)", "13 (65%)", "8 (44.4%)", "24 (36.4%)",
    "Total number of events", "17", "9", "3", "19", "15", "6", "5", "27", "12", "16", "9", "28",
    "dcd C.2.1.2.1", "14 (31.8%)", "4 (23.5%)", "2 (18.2%)", "15 (24.2%)", "15 (40.5%)", "5 (27.8%)", "4 (40%)", "24 (34.8%)", # nolint
    "10 (35.7%)", "13 (65%)", "8 (44.4%)", "24 (36.4%)",
    "cl C.1", "", "", "", "", "", "", "", "", "", "", "", "",
    "Total number of patients with at least one adverse event", "11 (25%)", "4 (23.5%)", "4 (36.4%)", "24 (38.7%)",
    "13 (35.1%)", "6 (33.3%)", "1 (10%)", "26 (37.7%)", "6 (21.4%)", "7 (35%)", "6 (33.3%)", "24 (36.4%)",
    "Total number of events", "14", "7", "4", "30", "20", "7", "1", "35", "8", "12", "9", "35",
    "dcd C.1.1.1.3", "11 (25%)", "4 (23.5%)", "4 (36.4%)", "24 (38.7%)", "13 (35.1%)", "6 (33.3%)", "1 (10%)", "26 (37.7%)", # nolint
    "6 (21.4%)", "7 (35%)", "6 (33.3%)", "24 (36.4%)"
  )

  expected_matrix <- matrix(expected_matrix, nrow = 36, ncol = 13, byrow = TRUE)

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
