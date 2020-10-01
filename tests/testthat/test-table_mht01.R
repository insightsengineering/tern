# Tests the single variant for MHT01.

library(dplyr)
library(random.cdisc.data)
library(rtables)
library(tern)

test_that("MHT01 variant 1 is produced accurately", {

  adsl <- radsl(cached = TRUE)
  n_per_arm <- table(adsl$ARM)

  admh <- radmh(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    split_rows_by("MHBODSYS", split_fun = drop_split_levels, child_labels = "visible", nested = FALSE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    count_occurrences(var = "MHDECOD")

  result <- build_table(lyt, admh, col_counts = n_per_arm) %>%
    prune_table()
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(c(
    "", "", "Total number of patients with at least one event",
    "Total number of events", "cl A", "Total number of patients with at least one event",
    "Total number of events", "trm A_1/2", "trm A_2/2", "cl B", "Total number of patients with at least one event",
    "Total number of events", "trm B_1/3", "trm B_2/3", "trm B_3/3",
    "cl C", "Total number of patients with at least one event", "Total number of events",
    "trm C_1/2", "trm C_2/2", "cl D", "Total number of patients with at least one event",
    "Total number of events", "trm D_1/3", "trm D_2/3", "trm D_3/3",
    "A: Drug X", "(N=134)", "122 (91%)", "609", "", "78 (58.2%)",
    "132", "50 (37.3%)", "48 (35.8%)", "", "96 (71.6%)", "185", "47 (35.1%)",
    "49 (36.6%)", "48 (35.8%)", "", "67 (50%)", "103", "43 (32.1%)",
    "35 (26.1%)", "", "96 (71.6%)", "189", "50 (37.3%)", "48 (35.8%)",
    "47 (35.1%)", "B: Placebo", "(N=134)", "123 (91.8%)", "622",
    "", "75 (56%)", "130", "45 (33.6%)", "48 (35.8%)", "", "89 (66.4%)",
    "198", "49 (36.6%)", "44 (32.8%)", "54 (40.3%)", "", "75 (56%)",
    "116", "46 (34.3%)", "48 (35.8%)", "", "90 (67.2%)", "178", "42 (31.3%)",
    "42 (31.3%)", "58 (43.3%)", "C: Combination", "(N=132)", "120 (90.9%)",
    "703", "", "89 (67.4%)", "160", "63 (47.7%)", "50 (37.9%)", "",
    "97 (73.5%)", "205", "43 (32.6%)", "52 (39.4%)", "51 (38.6%)",
    "", "79 (59.8%)", "129", "43 (32.6%)", "55 (41.7%)", "", "98 (74.2%)",
    "209", "51 (38.6%)", "50 (37.9%)", "57 (43.2%)"),
    .Dim = c(26L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination")))

  expect_identical(result_matrix, expected_matrix)

})

test_that("MHT01 variant 2 is produced accurately", {

  adsl <- radsl(cached = TRUE)
  n_per_arm <- table(adsl$ARM)

  admh <- radmh(cached = TRUE)

  admh_prior <- admh %>%
    filter(ASTDY <= 0)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    split_rows_by("MHBODSYS", split_fun = drop_split_levels, child_labels = "visible", nested = FALSE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    count_occurrences(var = "MHDECOD")

  result <- build_table(lyt, admh_prior, col_counts = n_per_arm) %>%
    prune_table()
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(c(
    "", "", "Total number of patients with at least one event",
    "Total number of events", "cl B", "Total number of patients with at least one event",
    "Total number of events", "trm B_1/3", "trm B_3/3", "cl D", "Total number of patients with at least one event",
    "Total number of events", "trm D_1/3", "trm D_2/3", "A: Drug X",
    "(N=134)", "2 (1.5%)", "3", "", "2 (1.5%)", "2", "1 (0.7%)",
    "1 (0.7%)", "", "1 (0.7%)", "1", "0 (0%)", "1 (0.7%)", "B: Placebo",
    "(N=134)", "0 (0%)", "0", "", "0 (0%)", "0", "0 (0%)", "0 (0%)",
    "", "0 (0%)", "0", "0 (0%)", "0 (0%)", "C: Combination", "(N=132)",
    "2 (1.5%)", "2", "", "0 (0%)", "0", "0 (0%)", "0 (0%)", "", "2 (1.5%)",
    "2", "1 (0.8%)", "1 (0.8%)"),
    .Dim = c(14L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination")))

  expect_identical(result_matrix, expected_matrix)

})

test_that("MHT01 variant 3 is produced accurately", {

  adsl <- radsl(cached = TRUE)
  n_per_arm <- table(adsl$ARM)

  admh <- radmh(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event")
    ) %>%
    split_rows_by("MHBODSYS", split_fun = drop_split_levels, child_labels = "visible", nested = FALSE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event")
    ) %>%
    count_occurrences(var = "MHDECOD")

  result <- build_table(lyt, admh, col_counts = n_per_arm) %>%
    prune_table()
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(c(
    "", "", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "cl A", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm A_1/2",
    "trm A_2/2", "cl B", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm B_1/3",
    "trm B_2/3", "trm B_3/3", "cl C", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm C_1/2",
    "trm C_2/2", "cl D", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm D_1/3",
    "trm D_2/3", "trm D_3/3", "A: Drug X", "(N=134)", "122 (91%)",
    "609", "", "78 (58.2%)", "132", "50 (37.3%)", "48 (35.8%)", "",
    "96 (71.6%)", "185", "47 (35.1%)", "49 (36.6%)", "48 (35.8%)",
    "", "67 (50%)", "103", "43 (32.1%)", "35 (26.1%)", "", "96 (71.6%)",
    "189", "50 (37.3%)", "48 (35.8%)", "47 (35.1%)", "B: Placebo",
    "(N=134)", "123 (91.8%)", "622", "", "75 (56%)", "130", "45 (33.6%)",
    "48 (35.8%)", "", "89 (66.4%)", "198", "49 (36.6%)", "44 (32.8%)",
    "54 (40.3%)", "", "75 (56%)", "116", "46 (34.3%)", "48 (35.8%)",
    "", "90 (67.2%)", "178", "42 (31.3%)", "42 (31.3%)", "58 (43.3%)",
    "C: Combination", "(N=132)", "120 (90.9%)", "703", "", "89 (67.4%)",
    "160", "63 (47.7%)", "50 (37.9%)", "", "97 (73.5%)", "205", "43 (32.6%)",
    "52 (39.4%)", "51 (38.6%)", "", "79 (59.8%)", "129", "43 (32.6%)",
    "55 (41.7%)", "", "98 (74.2%)", "209", "51 (38.6%)", "50 (37.9%)",
    "57 (43.2%)"),
    .Dim = c(26L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination")))

  expect_identical(result_matrix, expected_matrix)

})

# MHT01 variant 4 can not be produced by current rtables
# Medical History with total number of conditions per body system after the summary of patients
# Not a blocker given it's just a cosmetic variant

test_that("MHT01 variant 5 is produced accurately", {

  adsl <- radsl(cached = TRUE)
  n_per_arm <- table(adsl$ARM)
  n_per_arm_tot <- c(n_per_arm, "All Patients" = dim(adsl)[1])

  admh <- radmh(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event")
    ) %>%
    split_rows_by("MHBODSYS", split_fun = drop_split_levels, child_labels = "visible", nested = FALSE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c("Total number of patients with at least one event")
    ) %>%
    count_occurrences(var = "MHDECOD")

  result <- build_table(lyt, admh, col_counts = n_per_arm_tot) %>%
    prune_table()
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(c(
    "", "", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "cl A", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm A_1/2",
    "trm A_2/2", "cl B", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm B_1/3",
    "trm B_2/3", "trm B_3/3", "cl C", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm C_1/2",
    "trm C_2/2", "cl D", "Total number of patients with at least one event",
    "Total number of patients with at least one event", "trm D_1/3",
    "trm D_2/3", "trm D_3/3", "A: Drug X", "(N=134)", "122 (91%)",
    "609", "", "78 (58.2%)", "132", "50 (37.3%)", "48 (35.8%)", "",
    "96 (71.6%)", "185", "47 (35.1%)", "49 (36.6%)", "48 (35.8%)",
    "", "67 (50%)", "103", "43 (32.1%)", "35 (26.1%)", "", "96 (71.6%)",
    "189", "50 (37.3%)", "48 (35.8%)", "47 (35.1%)", "B: Placebo",
    "(N=134)", "123 (91.8%)", "622", "", "75 (56%)", "130", "45 (33.6%)",
    "48 (35.8%)", "", "89 (66.4%)", "198", "49 (36.6%)", "44 (32.8%)",
    "54 (40.3%)", "", "75 (56%)", "116", "46 (34.3%)", "48 (35.8%)",
    "", "90 (67.2%)", "178", "42 (31.3%)", "42 (31.3%)", "58 (43.3%)",
    "C: Combination", "(N=132)", "120 (90.9%)", "703", "", "89 (67.4%)",
    "160", "63 (47.7%)", "50 (37.9%)", "", "97 (73.5%)", "205", "43 (32.6%)",
    "52 (39.4%)", "51 (38.6%)", "", "79 (59.8%)", "129", "43 (32.6%)",
    "55 (41.7%)", "", "98 (74.2%)", "209", "51 (38.6%)", "50 (37.9%)",
    "57 (43.2%)", "All Patients", "(N=400)", "365 (91.2%)", "1934",
    "", "242 (60.5%)", "422", "158 (39.5%)", "146 (36.5%)", "", "282 (70.5%)",
    "588", "139 (34.8%)", "145 (36.2%)", "153 (38.2%)", "", "221 (55.2%)",
    "348", "132 (33%)", "138 (34.5%)", "", "284 (71%)", "576", "143 (35.8%)",
    "140 (35%)", "162 (40.5%)"),
    .Dim = c(26L, 5L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination", "All Patients")))

  expect_identical(result_matrix, expected_matrix)

})
