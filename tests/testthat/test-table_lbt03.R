# Tests the single variant for LBT03.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

testthat::test_that("LBT03 default variant is produced correctly", {
  adlb_f <- adlb %>%
    dplyr::filter(AVISIT != "SCREENING", PARAMCD == "ALT") %>%
    dplyr::mutate(
      AVISIT = droplevels(AVISIT),
      ABLFLL = ABLFL == "Y"
    )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_change(
      "CHG",
      variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
      na.rm = TRUE
    ) %>%
    build_table(
      df = adlb_f,
      alt_counts_df = adsl
    )

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 1 DAY 8",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 3 DAY 22", "n", "Mean (SD)", "Median",
      "Min - Max", "WEEK 4 DAY 29", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 5 DAY 36", "n", "Mean (SD)", "Median", "Min - Max", "A: Drug X",
      "(N=134)", "", "134", "19.79 (4.16)", "19.79", "6.99 - 30.43", "", "134",
      "-0.48 (5.85)", "-0.51", "-12.52 - 19.70", "", "134", "-0.08 (6.00)",
      "0.04", "-13.15 - 17.17", "", "134", "0.34 (5.76)", "0.45", "-16.04 - 16.26",
      "", "134", "0.57 (6.51)", "0.35", "-17.00 - 18.79", "", "134", "0.63 (6.05)",
      "0.93", "-16.03 - 16.09", "B: Placebo", "(N=134)", "", "134", "20.15 (4.17)",
      "20.08", "8.11 - 34.56", "", "134", "0.07 (6.04)", "-0.72", "-12.29 - 17.41",
      "", "134", "-0.03 (6.26)", "-0.37", "-18.37 - 14.39", "", "134",
      "-0.32 (5.60)", "-0.18", "-18.52 - 15.00", "", "134", "-0.52 (6.29)",
      "-1.19", "-16.58 - 17.23", "", "134", "-0.29 (6.31)", "0.73", "-19.04 - 12.64",
      "C: Combination", "(N=132)", "", "132", "20.45 (3.91)", "20.39",
      "8.82 - 28.72", "", "132", "0.10 (5.46)", "0.03", "-13.69 - 15.54", "",
      "132", "-1.20 (5.49)", "-0.48", "-14.28 - 15.65", "", "132", "-1.02 (5.55)",
      "-1.07", "-12.33 - 11.10", "", "132", "-0.63 (5.31)", "-0.65",
      "-12.13 - 15.30", "", "132", "-0.46 (5.57)", "-0.99", "-15.62 - 16.61"
    ),
    .Dim = c(32L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
