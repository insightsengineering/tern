# Preparation of the test case.
library(scda)
library(dplyr)
adpc <- synthetic_cdisc_data("rcd_2022_02_28")$adpc

testthat::test_that("PKCT01 is produced correctly", {
  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", "Plasma Drug Y", "C: Combination", "Plasma Drug X",
      "Plasma Drug Y", "n", "", "1474", "0", "", "1452", "1452", "Mean", "", "6.5", "NA", "",
      "6.5", "13.1", "SD", "", "6.7", "NA", "", "6.7", "13.5", "SE", "", "0.2", "NA", "",
      "0.2", "0.4", "CV (%)", "", "102.4", "NA", "", "103.3", "103.3", "CV % Geometric Mean",
      "", "NA", "NA", "", "NA", "NA"
    ),
    .Dim = c(7L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)


  l2 <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(var = "AVALC", var_type = "character", .stats = c("n_blq"), .labels = c(n_blq = "n_blq"), col_split = TRUE)

  adpc <- adpc %>% mutate(AVALC = as.factor(AVALC))
  result <- build_table(l2, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", "Plasma Drug Y",
      "C: Combination", "Plasma Drug X", "Plasma Drug Y",
      "n_blq", "", "402", "0", "", "396", "396"
    ),
    .Dim = c(7L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
