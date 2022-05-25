# Tests the single variant for EGT04.

library(scda)
library(rtables)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adeg <- synthetic_cdisc_data("rcd_2022_02_28")$adeg

testthat::test_that("EGT04 default variant is produced correctly", {
  adeg_labels <- formatters::var_labels(adeg)
  adeg_f <- subset(
    adeg,
    PARAMCD == "ECGINTP" & # Analysis in terms of "NORMAL"/"ABNORMAL" (AVALC)
      SAFFL == "Y" & # "Safety Population Flag"
      ONTRTFL == "Y" & # "On Treatment Record Flag"
      WORS02FL == "Y" # "Worst Post-Baseline Observation"
  )

  # Preprocessing
  # For the EGT04 template, data imputation shoud be avoided, and missing data
  # explicit and accounted for, so the contingency table sum adds up to the group N.
  # For illustration purpose, missing data are added to the example.
  set.seed(123, kind = "Mersenne-Twister")

  adeg_f$AVALC[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"
  adeg_f$BASEC[sample(seq_len(nrow(adeg_f)), size = 5)] <- "Missing"

  adeg_f$AVALC <- factor( # nolint
    adeg_f$AVALC,
    levels = c("NORMAL", "ABNORMAL", "Missing"),
    labels = c("Normal", "Abnormal", "Missing")
  )
  adeg_f$BASEC <- factor( # nolint
    adeg_f$BASEC,
    levels = c("NORMAL", "ABNORMAL", "Missing"),
    labels = c("Normal", "Abnormal", "Missing")
  )
  formatters::var_labels(adeg_f) <- adeg_labels

  l <- basic_table() %>%
    split_cols_by("AVALC") %>%
    split_rows_by("ARM") %>%
    add_rowcounts() %>%
    summarize_vars(
      "BASEC",
      denom = "N_row",
      .stats = "count_fraction",
      na.rm = FALSE
    )
  result <- build_table(l, adeg_f)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X (N=133)", "Normal", "Abnormal", "Missing",
      "B: Placebo (N=134)", "Normal", "Abnormal", "Missing", "C: Combination (N=130)",
      "Normal", "Abnormal", "Missing", "Normal", "", "24 (18%)", "6 (4.5%)",
      "0", "", "14 (10.4%)", "4 (3%)", "1 (0.7%)", "", "23 (17.7%)",
      "14 (10.8%)", "1 (0.8%)", "Abnormal", "", "76 (57.1%)", "24 (18%)",
      "1 (0.8%)", "", "88 (65.7%)", "24 (17.9%)", "1 (0.7%)", "", "66 (50.8%)",
      "25 (19.2%)", "0", "Missing", "", "1 (0.8%)", "0",
      "1 (0.8%)", "", "2 (1.5%)", "0", "0", "", "1 (0.8%)",
      "0", "0"
    ),
    .Dim = c(13L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
