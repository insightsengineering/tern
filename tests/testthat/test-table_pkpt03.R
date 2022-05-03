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
      "", "A: Drug X", "AUC Infinity Obs", "Max Conc", "Total CL Obs", "C: Combination",
      "AUC Infinity Obs", "Max Conc", "Total CL Obs", "n", "", "402", "402", "402",
      "", "792", "792", "792", "Mean", "", "198.0", "29.5", "5.0", "", "199.6",
      "30.0", "5.0", "SD", "", "37.9", "6.1", "1.1", "", "40.9", "6.0", "1.0",
      "SE", "", "1.9", "0.3", "0.1", "", "1.5", "0.2", "0.0", "CV (%)", "",
      "19.1", "20.5", "21.6", "", "20.5", "19.9", "20.6", "CV % Geometric Mean",
      "", "20.4", "22.6", "23.4", "", "21.9", "21.8", "22.3"
    ),
    .Dim = c(9L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
