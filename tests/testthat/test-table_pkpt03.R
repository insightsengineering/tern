# Preparation of the test case.
library(scda)
library(dplyr)
adpc <- synthetic_cdisc_data("rcd_2021_10_13")$adpc
adpc <- adpc %>% dplyr::filter(AVAL != 0)

testthat::test_that("PKPT03 is produced correctly", {

  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_numeric_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "A: Drug X", "Plasma Drug X", "Plasma Drug Y",
      "C: Combination", "Plasma Drug X", "Plasma Drug Y", "n", "",
      "1072", "0", "", "1056", "1056", "Mean", "", "8.9", "NA", "",
      "9", "18", "SD", "", "6.3", "NA", "", "6.4", "12.7", "SE", "",
      "0.2", "NA", "", "0.2", "0.4", "CV (%)", "", "70", "NA", "",
      "70.9", "70.9", "CV % Geometric Mean", "", "1687.8", "NA", "",
      "1971.1", "1972.3"),
    .Dim = c(7L, 7L)
    )
  testthat::expect_identical(result_matrix, expected_matrix)
})
