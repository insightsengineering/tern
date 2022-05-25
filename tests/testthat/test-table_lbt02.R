# Tests the single variant for LBT02.

library(scda)
library(magrittr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

testthat::test_that("LBT02 default variant is produced correctly", {
  adlb <- subset(adlb, AVISIT != "SCREENING" & PARAMCD == "ALT")
  adlb$AVISIT <- droplevels(adlb$AVISIT) # nolint snake_case

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    add_colcounts() %>%
    summarize_vars(vars = "AVAL")

  result <- build_table(l, df = adlb, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 1 DAY 8",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 3 DAY 22", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 4 DAY 29", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 5 DAY 36",
      "n", "Mean (SD)", "Median", "Min - Max", "A: Drug X", "(N=134)", "", "134",
      "19.8 (4.2)", "19.8", "7.0 - 30.4", "", "134", "19.3 (4.0)", "19.2",
      "8.9 - 27.3", "", "134", "19.7 (4.2)", "19.2", "7.2 - 30.5", "", "134",
      "20.1 (3.8)", "20.0", "11.5 - 29.5", "", "134", "20.4 (4.6)", "19.8",
      "9.9 - 34.5", "", "134", "20.4 (3.9)", "20.7", "10.9 - 30.2", "B: Placebo",
      "(N=134)", "", "134", "20.2 (4.2)", "20.1", "8.1 - 34.6", "", "134",
      "20.2 (4.0)", "20.1", "5.8 - 28.8", "", "134", "20.1 (4.3)", "20.0",
      "7.2 - 30.5", "", "134", "19.8 (3.9)", "19.9", "11.9 - 28.2", "", "134",
      "19.6 (4.3)", "19.2", "11.5 - 32.0", "", "134", "19.9 (4.2)", "20.1",
      "10.3 - 29.1", "C: Combination", "(N=132)", "", "132", "20.5 (3.9)", "20.4",
      "8.8 - 28.7", "", "132", "20.6 (3.9)", "20.4", "9.9 - 30.7", "", "132",
      "19.2 (3.6)", "19.6", "8.1 - 26.7", "", "132", "19.4 (3.9)", "18.8",
      "10.2 - 28.5", "", "132", "19.8 (4.0)", "19.9", "7.8 - 29.3", "", "132",
      "20.0 (4.2)", "20.5", "7.4 - 27.8"
    ),
    .Dim = c(32L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
