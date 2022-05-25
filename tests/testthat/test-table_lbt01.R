# Test the single variant for LBT01.

library(scda)
library(rtables)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

testthat::test_that("LBT01 default variant is produced correctly", {
  adlb_f <- adlb %>%
    dplyr::filter(
      PARAM == "Alanine Aminotransferase Measurement" &
        !(ARM == "B: Placebo" & AVISIT == "WEEK 1 DAY 8") &
        AVISIT != "SCREENING"
    )

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Analysis Value", "Absolute Change from Baseline")
    ) %>%
    summarize_colvars()

  result <- build_table(l, adlb_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 1 DAY 8",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 3 DAY 22", "n", "Mean (SD)", "Median",
      "Min - Max", "WEEK 4 DAY 29", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 5 DAY 36", "n", "Mean (SD)", "Median", "Min - Max", "A: Drug X",
      "Analysis Value", "", "134", "19.8 (4.2)", "19.8", "7.0 - 30.4", "", "134",
      "19.3 (4.0)", "19.2", "8.9 - 27.3", "", "134", "19.7 (4.2)", "19.2",
      "7.2 - 30.5", "", "134", "20.1 (3.8)", "20.0", "11.5 - 29.5", "", "134",
      "20.4 (4.6)", "19.8", "9.9 - 34.5", "", "134", "20.4 (3.9)", "20.7",
      "10.9 - 30.2", "A: Drug X", "Absolute Change from Baseline", "", "134",
      "0.0 (0.0)", "0.0", "0.0 - 0.0", "", "134", "-0.5 (5.9)", "-0.5",
      "-12.5 - 19.7", "", "134", "-0.1 (6.0)", "0.0", "-13.2 - 17.2", "", "134",
      "0.3 (5.8)", "0.5", "-16.0 - 16.3", "", "134", "0.6 (6.5)", "0.4",
      "-17.0 - 18.8", "", "134", "0.6 (6.1)", "0.9", "-16.0 - 16.1", "B: Placebo",
      "Analysis Value", "", "134", "20.2 (4.2)", "20.1", "8.1 - 34.6", "", "0",
      "NA (NA)", "NA", "NA - NA", "", "134", "20.1 (4.3)", "20.0", "7.2 - 30.5",
      "", "134", "19.8 (3.9)", "19.9", "11.9 - 28.2", "", "134", "19.6 (4.3)",
      "19.2", "11.5 - 32.0", "", "134", "19.9 (4.2)", "20.1", "10.3 - 29.1",
      "B: Placebo", "Absolute Change from Baseline", "", "134", "0.0 (0.0)",
      "0.0", "0.0 - 0.0", "", "0", "NA (NA)", "NA", "NA - NA", "", "134",
      "-0.0 (6.3)", "-0.4", "-18.4 - 14.4", "", "134", "-0.3 (5.6)", "-0.2",
      "-18.5 - 15.0", "", "134", "-0.5 (6.3)", "-1.2", "-16.6 - 17.2", "", "134",
      "-0.3 (6.3)", "0.7", "-19.0 - 12.6", "C: Combination", "Analysis Value",
      "", "132", "20.5 (3.9)", "20.4", "8.8 - 28.7", "", "132", "20.6 (3.9)",
      "20.4", "9.9 - 30.7", "", "132", "19.2 (3.6)", "19.6", "8.1 - 26.7", "",
      "132", "19.4 (3.9)", "18.8", "10.2 - 28.5", "", "132", "19.8 (4.0)", "19.9",
      "7.8 - 29.3", "", "132", "20.0 (4.2)", "20.5", "7.4 - 27.8", "C: Combination",
      "Absolute Change from Baseline", "", "132", "0.0 (0.0)", "0.0", "0.0 - 0.0",
      "", "132", "0.1 (5.5)", "0.0", "-13.7 - 15.5", "", "132", "-1.2 (5.5)",
      "-0.5", "-14.3 - 15.6", "", "132", "-1.0 (5.6)", "-1.1", "-12.3 - 11.1",
      "", "132", "-0.6 (5.3)", "-0.6", "-12.1 - 15.3", "", "132", "-0.5 (5.6)",
      "-1.0", "-15.6 - 16.6"
    ),
    .Dim = c(32L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
