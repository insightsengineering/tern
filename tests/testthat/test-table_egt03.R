# Tests variant 1 for EGT03.

library(rtables)
library(tern)
library(scda)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adeg <- synthetic_cdisc_data("rcd_2022_02_28")$adeg

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
      "", "A: Drug X (N=133)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "C: Combination (N=132)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "LOW", "", "39", "1 (0.8%)", "35 (26.3%)", "3 (2.3%)",
      "0", "", "43", "1 (0.7%)", "40 (29.9%)", "1 (0.7%)", "1 (0.7%)",
      "", "37", "4 (3%)", "30 (22.7%)", "1 (0.8%)", "2 (1.5%)", "NORMAL",
      "", "90", "3 (2.3%)", "83 (62.4%)", "4 (3%)", "0", "", "91",
      "9 (6.7%)", "77 (57.5%)", "4 (3%)", "1 (0.7%)", "", "94", "11 (8.3%)",
      "75 (56.8%)", "8 (6.1%)", "0", "HIGH", "", "0", "0", "0", "0",
      "0", "", "0", "0", "0", "0", "0", "", "0", "0", "0", "0", "0",
      "Missing", "", "4", "2 (1.5%)", "1 (0.8%)", "0", "1 (0.8%)",
      "", "0", "0", "0", "0", "0", "", "1", "0", "0", "1 (0.8%)", "0"
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
      "", "A: Drug X (N=133)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "C: Combination (N=132)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "LOW", "", "39", "1 (0.8%)", "35 (26.3%)", "3 (2.3%)",
      "0", "", "43", "1 (0.7%)", "40 (29.9%)", "1 (0.7%)", "1 (0.7%)",
      "", "37", "4 (3%)", "30 (22.7%)", "1 (0.8%)", "2 (1.5%)", "NORMAL",
      "", "94", "5 (3.8%)", "84 (63.2%)", "4 (3%)", "1 (0.8%)", "",
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
      "", "A: Drug X (N=133)", "n", "LOW", "NORMAL", "HIGH",
      "B: Placebo (N=134)", "n", "LOW", "NORMAL", "HIGH", "C: Combination (N=132)",
      "n", "LOW", "NORMAL", "HIGH", "LOW", "", "39", "1 (0.8%)", "35 (26.3%)",
      "3 (2.3%)", "", "42", "1 (0.7%)", "40 (29.9%)", "1 (0.7%)", "",
      "35", "4 (3%)", "30 (22.7%)", "1 (0.8%)", "NORMAL", "", "93",
      "5 (3.8%)", "84 (63.2%)", "4 (3%)", "", "90", "9 (6.7%)", "77 (57.5%)",
      "4 (3%)", "", "95", "11 (8.3%)", "75 (56.8%)", "9 (6.8%)", "HIGH",
      "", "0", "0", "0", "0", "", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "Missing", "", "1", "0", "1 (0.8%)", "0", "", "2",
      "0", "2 (1.5%)", "0", "", "2", "0", "2 (1.5%)", "0"
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
      "", "A: Drug X (N=133)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "B: Placebo (N=133)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "C: Combination (N=132)", "n", "LOW", "NORMAL", "HIGH",
      "Missing", "LOW", "", "0", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "0", "", "0", "0", "0", "0", "0", "NORMAL", "", "94",
      "2 (1.5%)", "87 (65.4%)", "5 (3.8%)", "0", "", "88", "8 (6%)",
      "76 (57.1%)", "3 (2.3%)", "1 (0.8%)", "", "95", "11 (8.3%)",
      "78 (59.1%)", "5 (3.8%)", "1 (0.8%)", "HIGH", "", "37", "4 (3%)",
      "30 (22.6%)", "2 (1.5%)", "1 (0.8%)", "", "43", "2 (1.5%)", "38 (28.6%)",
      "2 (1.5%)", "1 (0.8%)", "", "36", "3 (2.3%)", "28 (21.2%)", "5 (3.8%)",
      "0", "Missing", "", "2", "0", "1 (0.8%)", "0", "1 (0.8%)", "",
      "2", "0", "2 (1.5%)", "0", "0", "", "1", "1 (0.8%)", "0", "0",
      "0"
    ),
    .Dim = c(19L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
