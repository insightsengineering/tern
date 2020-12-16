# Test variants 1 and 5 for AET06. Variants 2-4 are essentially identical
# to variant 1, needing only the data frame to be pre-processed
# and the baseline variable to be changed from SEX.

library(random.cdisc.data)
library(magrittr)

test_that("AET06 variant 1 is produced correctly", {

  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

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
      )) %>%
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
      )) %>%
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

  expect_identical(result_matrix, expected_matrix)

})

test_that("AET06 variant 5 is produced correctly", {

  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

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
      )) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L)  %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )) %>%
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
      )) %>%
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

  expect_identical(result_matrix, expected_matrix)

})
