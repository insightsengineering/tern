# Preparation of the test case.
library(scda)
library(dplyr)
adpp <- synthetic_cdisc_data("rcd_2022_02_28")$adpp
adpp <- adpp %>% dplyr::filter(AVAL != 0)

testthat::test_that("PKPT03 is produced correctly", {
  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpp)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "AUC Infinity Obs", "Max Conc", "Renal CL", "Renal CL Dose Norm",
      "Time of Maximum Response", "Time to Onset", "Total CL Obs", "C: Combination",
      "AUC Infinity Obs", "Max Conc", "Renal CL", "Renal CL Dose Norm",
      "Time of Maximum Response", "Time to Onset", "Total CL Obs", "n", "", "402",
      "402", "402", "402", "402", "402", "402", "", "792", "792", "792", "792",
      "792", "792", "792", "Mean", "", "199.2", "30.2", "0.1", "0.0", "9.9", "3.0",
      "5.0", "", "200.2", "30.1", "0.0", "0.0", "10.0", "3.0", "5.0", "SD", "", "39.1",
      "6.1", "0.0", "0.0", "1.9", "0.6", "1.0", "", "40.3", "6.0", "0.0", "0.0", "2.0",
      "0.6", "1.0", "SE", "", "1.9", "0.3", "0.0", "0.0", "0.1", "0.0", "0.1", "",
      "1.4", "0.2", "0.0", "0.0", "0.1", "0.0", "0.0", "CV (%)", "", "19.6", "20.3",
      "19.8", "19.8", "19.7", "19.3", "21.1", "", "20.1", "20.0", "19.5", "19.3",
      "20.3", "19.7", "20.2", "CV % Geometric Mean", "", "21.5", "21.7", "20.7",
      "20.4", "20.9", "20.3", "22.8", "", "21.6", "21.6", "21.0", "20.5", "21.9", "20.8", "21.6"
    ),
    .Dim = c(17L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
