# Tests the single variant for LBT02.

library(random.cdisc.data)
library(magrittr)

test_that("LBT02 default variant is produced correctly", {

  adsl <- radsl(cached = TRUE)
  adlb <- radlb(cached = TRUE)
  adlb <- subset(adlb, AVISIT != "SCREENING" & PARAMCD == "ALT")
  adlb$AVISIT <- droplevels(adlb$AVISIT) # nolint snake_case

  l <- split_cols_by(lyt = NULL, var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    add_colcounts() %>%
    summarize_vars(vars = "AVAL")

  result <- build_table(l, df = adlb, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 1 DAY 8", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 3 DAY 22", "n",
      "Mean (SD)", "Median", "Min - Max", "WEEK 4 DAY 29", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 5 DAY 36", "n", "Mean (SD)", "Median",
      "Min - Max", "A: Drug X", "(N=134)", "", "134", "49.6 (8.3)",
      "49.6", "24 - 70.9", "", "134", "48.6 (8)", "48.4", "27.7 - 64.6",
      "", "134", "49.4 (8.5)", "48.3", "24.3 - 71.1", "", "134", "50.3 (7.5)",
      "50.1", "33 - 69", "", "134", "50.7 (9.2)", "49.5", "29.8 - 79",
      "", "134", "50.8 (7.8)", "51.4", "31.9 - 70.3", "B: Placebo",
      "(N=134)", "", "134", "50.3 (8.3)", "50.2", "26.2 - 79.1", "",
      "134", "50.4 (7.9)", "50.2", "21.7 - 67.5", "", "134", "50.2 (8.5)",
      "50", "24.4 - 71.1", "", "134", "49.7 (7.7)", "49.7", "33.7 - 66.5",
      "", "134", "49.3 (8.7)", "48.3", "33 - 74", "", "134", "49.7 (8.4)",
      "50.2", "30.6 - 68.1", "C: Combination", "(N=132)", "", "132",
      "50.9 (7.8)", "50.8", "27.6 - 67.4", "", "132", "51.1 (7.8)",
      "50.8", "29.7 - 71.4", "", "132", "48.5 (7.2)", "49.2", "26.2 - 63.4",
      "", "132", "48.9 (7.9)", "47.7", "30.4 - 67", "", "132", "49.6 (8)",
      "49.8", "25.5 - 68.6", "", "132", "50 (8.3)", "51", "24.8 - 65.6"
    ),
    .Dim = c(32L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)

})
