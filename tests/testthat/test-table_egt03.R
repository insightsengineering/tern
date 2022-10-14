# Tests variant 1 for EGT03.
adsl <- adsl_raw
adeg <- adeg_raw

testthat::test_that("EGT03 variant 1 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")
  adeg_labels <- formatters::var_labels(adeg)

  # Filtering
  # ---------
  adeg_f <- subset(
    adeg,
    PARAMCD == "HR" & # Heart Rate
      SAFFL == "Y" & # "Safety Population Flag"
      ONTRTFL == "Y" & # "On Treatment Record Flag"
      AVISIT == "POST-BASELINE MINIMUM" # "Analysis Visit"
  )

  # Preprocessing

  # For the EGT03 template, data imputation shoud be avoided, and missing data
  # explicit and accounted for, so the contingency table sum adds up to the group N.
  # For illustration purpose, missing data are added to the example.
  adeg_f$BNRIND <- factor( # nolint
    adeg_f$BNRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )
  adeg_f$ANRIND <- factor( # nolint
    adeg_f$ANRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$BNRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"
  adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"


  formatters::var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts() %>%
    summarize_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "A: Drug X (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "C: Combination (N=132)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "LOW", "", "40", "1 (0.7%)", "36 (26.9%)", "2 (1.5%)",
      "1 (0.7%)", "", "43", "1 (0.7%)", "40 (29.9%)", "1 (0.7%)", "1 (0.7%)",
      "", "37", "4 (3%)", "31 (23.5%)", "1 (0.8%)", "1 (0.8%)", "NORMAL",
      "", "92", "5 (3.7%)", "83 (61.9%)", "4 (3%)", "0", "", "89",
      "9 (6.7%)", "75 (56%)", "4 (3%)", "1 (0.7%)", "", "94", "11 (8.3%)",
      "75 (56.8%)", "8 (6.1%)", "0", "HIGH", "", "0", "0", "0", "0",
      "0", "", "0", "0", "0", "0", "0", "", "0", "0", "0", "0", "0",
      "Missing", "", "2", "0", "1 (0.7%)", "0", "1 (0.7%)",
      "", "2", "0", "2 (1.5%)", "0", "0", "", "1", "0", "0", "1 (0.8%)", "0"
    ),
    .Dim = c(19L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("EGT03 variant 2 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")
  adeg_labels <- formatters::var_labels(adeg)

  # Filtering
  # ---------
  adeg_f <- subset(
    adeg,
    PARAMCD == "HR" & # Heart Rate
      SAFFL == "Y" & # "Safety Population Flag"
      ONTRTFL == "Y" & # "On Treatment Record Flag"
      AVISIT == "POST-BASELINE MINIMUM" # "Analysis Visit"
  )

  # Preprocessing

  # For the EGT03 template, data imputation shoud be avoided, and missing data
  # explicit and accounted for, so the contingency table sum adds up to the group N.
  # For illustration purpose, missing data are added to the example.
  adeg_f$BNRIND <- factor( # nolint
    adeg_f$BNRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$BNRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

  formatters::var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts() %>%
    summarize_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "A: Drug X (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "C: Combination (N=132)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "LOW", "", "40", "1 (0.7%)", "36 (26.9%)", "2 (1.5%)",
      "1 (0.7%)", "", "43", "1 (0.7%)", "40 (29.9%)", "1 (0.7%)", "1 (0.7%)",
      "", "37", "4 (3%)", "31 (23.5%)", "1 (0.8%)", "1 (0.8%)", "NORMAL",
      "", "94", "5 (3.7%)", "84 (62.7%)", "4 (3%)", "1 (0.7%)", "",
      "91", "9 (6.7%)", "77 (57.5%)", "4 (3%)", "1 (0.7%)", "", "95",
      "11 (8.3%)", "75 (56.8%)", "9 (6.8%)", "0", "HIGH", "", "0",
      "0", "0", "0", "0", "", "0", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "0"
    ),
    .Dim = c(19L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("EGT03 variant 3 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")
  adeg_labels <- formatters::var_labels(adeg)

  # Filtering
  # ---------
  adeg_f <- subset(
    adeg,
    PARAMCD == "HR" & # Heart Rate
      SAFFL == "Y" & # "Safety Population Flag"
      ONTRTFL == "Y" & # "On Treatment Record Flag"
      AVISIT == "POST-BASELINE MINIMUM" # "Analysis Visit"
  )

  # Preprocessing

  # For the EGT03 template, data imputation shoud be avoided, and missing data
  # explicit and accounted for, so the contingency table sum adds up to the group N.
  # For illustration purpose, missing data are added to the example.
  adeg_f$ANRIND <- factor( # nolint
    adeg_f$ANRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

  formatters::var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts() %>%
    summarize_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "A: Drug X (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH", "C: Combination (N=132)",
      "n", "LOW", "NORMAL", "HIGH", "LOW", "", "39", "1 (0.7%)", "36 (26.9%)",
      "2 (1.5%)", "", "42", "1 (0.7%)", "40 (29.9%)", "1 (0.7%)", "",
      "36", "4 (3%)", "31 (23.5%)", "1 (0.8%)", "NORMAL", "", "93",
      "5 (3.7%)", "84 (62.7%)", "4 (3%)", "", "90", "9 (6.7%)", "77 (57.5%)",
      "4 (3%)", "", "95", "11 (8.3%)", "75 (56.8%)", "9 (6.8%)", "HIGH",
      "", "0", "0", "0", "0", "", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "Missing", "", "2", "0", "1 (0.7%)", "1 (0.7%)", "", "2",
      "0", "2 (1.5%)", "0", "", "1", "0", "1 (0.8%)", "0"
    ),
    .Dim = c(16L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("EGT03 variant 4 is produced correctly", {
  set.seed(123, kind = "Mersenne-Twister")
  adeg_labels <- formatters::var_labels(adeg)

  # Filtering
  # ---------
  adeg_f <- subset(
    adeg,
    PARAMCD == "HR" & # Heart Rate
      SAFFL == "Y" & # "Safety Population Flag"
      ONTRTFL == "Y" & # "On Treatment Record Flag"
      AVISIT == "POST-BASELINE MAXIMUM" # "Analysis Visit"
  )

  # Preprocessing

  # For the EGT03 template, data imputation shoud be avoided, and missing data
  # explicit and accounted for, so the contingency table sum adds up to the group N.
  # For illustration purpose, missing data are added to the example.
  adeg_f$BNRIND <- factor( # nolint
    adeg_f$BNRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )
  adeg_f$ANRIND <- factor( # nolint
    adeg_f$ANRIND,
    levels = c("LOW", "NORMAL", "HIGH", "Missing"),
    labels = c("LOW", "NORMAL", "HIGH", "Missing")
  )

  adeg_f$BNRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"
  adeg_f$ANRIND[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

  formatters::var_labels(adeg_f) <- adeg_labels

  lyt <- basic_table() %>%
    split_cols_by("ANRIND") %>%
    split_rows_by("ARM") %>%
    add_rowcounts() %>%
    summarize_vars("BNRIND", denom = "N_row")

  result <- build_table(lyt = lyt, df = adeg_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "A: Drug X (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "C: Combination (N=132)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "LOW", "", "0", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "0", "", "0", "0", "0", "0", "0", "NORMAL", "", "95",
      "2 (1.5%)", "88 (65.7%)", "5 (3.7%)", "0", "", "88", "8 (6%)",
      "76 (56.7%)", "3 (2.2%)", "1 (0.7%)", "", "96", "12 (9.1%)",
      "79 (59.8%)", "5 (3.8%)", "0", "HIGH", "", "37", "4 (3%)",
      "31 (23.1%)", "1 (0.7%)", "1 (0.7%)", "", "44", "2 (1.5%)", "39 (29.1%)",
      "2 (1.5%)", "1 (0.7%)", "", "35", "3 (2.3%)", "27 (20.5%)", "4 (3%)",
      "1 (0.8%)", "Missing", "", "2", "0", "1 (0.7%)", "0", "1 (0.7%)", "",
      "2", "0", "2 (1.5%)", "0", "0", "", "1", "0", "0", "1 (0.8%)",
      "0"
    ),
    .Dim = c(19L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
