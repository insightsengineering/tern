# Preparation of the test case.
library(scda)
library(dplyr)
adpc <- synthetic_cdisc_data("rcd_2021_10_13")$adpc
adpc <- adpc %>% filter(AVAL != 0)

testthat::test_that("PKPT03 is produced correctly", {

  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_pk_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- t(structure(
    c("", "n", "Mean", "SD", "CV % Mean", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum",
      "A: Drug X", "", "", "", "", "", "", "", "", "",
      "Plasma Drug X", "1072", "8.94", "6.26", "70", "3.06", "1687.8", "10.33", "0", "19.86",
      "Plasma Drug Y", "0", "NA", "NA", "NA", "NaN", "NA", "NA", "NA", "NA",
      "C: Combination", "", "", "", "", "", "", "", "", "",
      "Plasma Drug X", "1056", "8.98", "6.37", "70.9", "2.95", "1971.1", "10.44", "0", "19.73",
      "Plasma Drug Y", "1056", "17.97", "12.74", "70.9", "5.9", "1972.3", "20.88", "0", "39.47"),
    .Dim = c(10L, 7L)
  ))
  testthat::expect_identical(result_matrix, expected_matrix)
})
