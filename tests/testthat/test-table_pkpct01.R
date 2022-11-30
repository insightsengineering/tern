# Preparation of the test case.
adpc <- adpc_raw

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
      "", "A: Drug X", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "C: Combination", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "n", "", "1474", "0", "804", "804", "", "1452", "1452", "792", "792",
      "Mean", "", "6.5", "NA", "0.9", "0.9", "", "6.5", "13.1", "0.8", "0.8",
      "SD", "", "6.7", "NA", "1.8", "1.8", "", "6.7", "13.5", "1.8", "1.8",
      "SE", "", "0.2", "NA", "0.1", "0.1", "", "0.2", "0.4", "0.1", "0.1",
      "CV (%)", "", "102.4", "NA", "210.7", "210.7", "", "103.3", "103.3", "212.4", "212.4",
      "CV % Geometric Mean", "", "NA", "NA", "NA", "NA", "", "NA", "NA", "NA", "NA"
    ),
    .Dim = c(11L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  l2 <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(
      var = "AVALC", var_type = "character", .stats = c("n_blq"),
      .labels = c(n_blq = "n_blq"), col_split = TRUE
    )

  adpc <- adpc %>% mutate(AVALC = as.factor(AVALC))
  result <- build_table(l2, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "C: Combination", "Plasma Drug X", "Plasma Drug Y", "Urine Drug X", "Urine Drug Y",
      "n_blq", "", "402", "0", "402", "402", "", "396", "396", "396", "396"
    ),
    .Dim = c(11L, 2L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
