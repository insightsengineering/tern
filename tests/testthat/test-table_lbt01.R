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
      "", "", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 1 DAY 8", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 3 DAY 22", "n",
      "Mean (SD)", "Median", "Min - Max", "WEEK 4 DAY 29", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 5 DAY 36", "n", "Mean (SD)", "Median",
      "Min - Max", "A: Drug X", "Analysis Value", "", "134", "49.6 (8.3)",
      "49.6", "24.0 - 70.9", "", "134", "48.6 (8.0)", "48.4", "27.7 - 64.6",
      "", "134", "49.4 (8.5)", "48.3", "24.3 - 71.1", "", "134", "50.3 (7.5)",
      "50.1", "33.0 - 69.0", "", "134", "50.7 (9.2)", "49.5", "29.8 - 79.0",
      "", "134", "50.8 (7.8)", "51.4", "31.9 - 70.3", "A: Drug X",
      "Absolute Change from Baseline", "", "134", "0.0 (0.0)", "0.0", "0.0 - 0.0",
      "", "134", "-1.0 (11.7)", "-1.0", "-25.0 - 39.4", "", "134", "-0.2 (12.0)",
      "0.1", "-26.3 - 34.3", "", "134", "0.7 (11.5)", "0.9", "-32.1 - 32.5",
      "", "134", "1.1 (13.0)", "0.7", "-34.0 - 37.6", "", "134", "1.3 (12.1)",
      "1.9", "-32.1 - 32.2", "B: Placebo", "Analysis Value", "", "134",
      "50.3 (8.3)", "50.2", "26.2 - 79.1", "", "0", "NA (NA)", "NA",
      "NA - NA", "", "134", "50.2 (8.5)", "50.0", "24.4 - 71.1", "",
      "134", "49.7 (7.7)", "49.7", "33.7 - 66.5", "", "134", "49.3 (8.7)",
      "48.3", "33.0 - 74.0", "", "134", "49.7 (8.4)", "50.2", "30.6 - 68.1",
      "B: Placebo", "Absolute Change from Baseline", "", "134", "0.0 (0.0)",
      "0.0", "0.0 - 0.0", "", "0", "NA (NA)", "NA", "NA - NA", "", "134",
      "-0.1 (12.5)", "-0.7", "-36.7 - 28.8", "", "134", "-0.6 (11.2)",
      "-0.4", "-37.0 - 30.0", "", "134", "-1.0 (12.6)", "-2.4", "-33.2 - 34.5",
      "", "134", "-0.6 (12.6)", "1.5", "-38.1 - 25.3", "C: Combination",
      "Analysis Value", "", "132", "50.9 (7.8)", "50.8", "27.6 - 67.4",
      "", "132", "51.1 (7.8)", "50.8", "29.7 - 71.4", "", "132", "48.5 (7.2)",
      "49.2", "26.2 - 63.4", "", "132", "48.9 (7.9)", "47.7", "30.4 - 67.0",
      "", "132", "49.6 (8.0)", "49.8", "25.5 - 68.6", "", "132", "50.0 (8.3)",
      "51.0", "24.8 - 65.6", "C: Combination", "Absolute Change from Baseline",
      "", "132", "0.0 (0.0)", "0.0", "0.0 - 0.0", "", "132", "0.2 (10.9)", "0.1",
      "-27.4 - 31.1", "", "132", "-2.4 (11.0)", "-1.0", "-28.6 - 31.3",
      "", "132", "-2.0 (11.1)", "-2.1", "-24.7 - 22.2", "", "132", "-1.3 (10.6)",
      "-1.3", "-24.3 - 30.6", "", "132", "-0.9 (11.1)", "-2.0", "-31.2 - 33.2"
    ),
    .Dim = c(32L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
