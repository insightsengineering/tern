# Tests the single variant for LBT03.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
adlb <- synthetic_cdisc_data("rcd_2021_05_05")$adlb

test_that("LBT03 default variant is produced correctly", {
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
    c("", "", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 1 DAY 8", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 3 DAY 22", "n",
      "Mean (SD)", "Median", "Min - Max", "WEEK 4 DAY 29", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 5 DAY 36", "n", "Mean (SD)", "Median",
      "Min - Max", "A: Drug X", "(N=134)", "", "134", "49.57 (8.32)",
      "49.57", "23.97 - 70.85", "", "134", "-0.97 (11.7)", "-1.02",
      "-25.05 - 39.39", "", "134", "-0.16 (12.01)", "0.08", "-26.31 - 34.35",
      "", "134", "0.69 (11.52)", "0.9", "-32.07 - 32.51", "", "134",
      "1.13 (13.01)", "0.71", "-34 - 37.59", "", "134", "1.26 (12.11)",
      "1.85", "-32.06 - 32.18", "B: Placebo", "(N=134)", "", "134",
      "50.31 (8.34)", "50.15", "26.22 - 79.12", "", "134", "0.14 (12.07)",
      "-1.45", "-24.58 - 34.81", "", "134", "-0.06 (12.53)", "-0.74",
      "-36.75 - 28.78", "", "134", "-0.63 (11.21)", "-0.36", "-37.04 - 30",
      "", "134", "-1.04 (12.59)", "-2.37", "-33.16 - 34.47", "", "134",
      "-0.59 (12.63)", "1.46", "-38.08 - 25.27", "C: Combination",
      "(N=132)", "", "132", "50.9 (7.82)", "50.77", "27.63 - 67.45",
      "", "132", "0.2 (10.92)", "0.05", "-27.38 - 31.08", "", "132",
      "-2.41 (10.97)", "-0.96", "-28.56 - 31.29", "", "132", "-2.05 (11.1)",
      "-2.13", "-24.65 - 22.2", "", "132", "-1.25 (10.62)", "-1.29",
      "-24.26 - 30.6", "", "132", "-0.92 (11.14)", "-1.98", "-31.24 - 33.22"
    ),
    .Dim = c(32L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
