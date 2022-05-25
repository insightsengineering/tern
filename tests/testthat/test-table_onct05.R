library(scda)
library(dplyr)

preprocess_adrs <- function(adrs, n_records = 20) {
  adrs_labels <- formatters::var_labels(adrs)
  adrs <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
    dplyr::slice(seq_len(n_records)) %>%
    droplevels() %>%
    dplyr::mutate(
      # Reorder levels of factor to make the placebo group the reference arm.
      ARM = forcats::fct_relevel(ARM, "B: Placebo"),
      rsp = AVALC == "CR"
    )
  formatters::var_labels(adrs) <- c(adrs_labels, "Response")

  adrs
}

adrs <- synthetic_cdisc_data("rcd_2022_02_28")$adrs

testthat::test_that("ONCT05 variant 1 (Objective Response Rate by Subgroup) is produced correctly", {
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200)

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Stratification Factor 2",
      "S1", "S2", " ", "Total n", "200", "", "120", "80", "", "105",
      "95", "B: Placebo", "n", "100", "", "62", "38", "", "48", "52",
      "B: Placebo", "Response (%)", "71.0%", "", "64.5%", "81.6%", "",
      "70.8%", "71.2%", "A: Drug X", "n", "100", "", "58", "42", "",
      "57", "43", "A: Drug X", "Response (%)", "90.0%", "", "91.4%",
      "88.1%", "", "89.5%", "90.7%", " ", "Odds Ratio", "3.68", "",
      "5.83", "1.67", "", "3.50", "3.95", " ", "95% CI", "(1.68, 8.04)",
      "", "(2.03, 16.73)", "(0.48, 5.79)", "", "(1.22, 10.00)", "(1.20, 13.01)"
    ),
    .Dim = 9:8
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("ONCT05 variant 2 (Specifying class variables) is produced correctly", {
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200)

  adrs <- adrs %>%
    dplyr::mutate(
      # Reorder levels of SEX.
      SEX = forcats::fct_relevel(SEX, "M", "F"),
      # Reorder levels of STRATA1 by frequency.
      STRATA1 = forcats::fct_infreq(STRATA1)
    )

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA1")),
    data = adrs
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex", "M", "F", "Stratification Factor 1",
      "C", "B", "A", " ", "Total n", "200", "", "80", "120", "", "72",
      "71", "57", "B: Placebo", "n", "100", "", "38", "62", "", "36",
      "35", "29", "B: Placebo", "Response (%)", "71.0%", "", "81.6%",
      "64.5%", "", "72.2%", "74.3%", "65.5%", "A: Drug X", "n", "100",
      "", "42", "58", "", "36", "36", "28", "A: Drug X", "Response (%)",
      "90.0%", "", "88.1%", "91.4%", "", "94.4%", "77.8%", "100.0%", " ",
      "Odds Ratio", "3.68", "", "1.67", "5.83", "", "6.54", "1.21",
      ">999.99", " ", "95% CI", "(1.68, 8.04)", "", "(0.48, 5.79)",
      "(2.03, 16.73)", "", "(1.32, 32.44)", "(0.41, 3.61)", "(0.00, >999.99)"
    ),
    .Dim = c(10L, 8L)
  )

  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("ONCT05 variant 3 (selecting columns and changing the alpha level) is produced correctly", {
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200)

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs,
    conf_level = 0.9,
    method = "chisq"
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "or", "ci", "pval"))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Stratification Factor 2",
      "S1", "S2", " ", "Total n", "200", "", "120", "80", "", "105",
      "95", " ", "Odds Ratio", "3.68", "", "5.83", "1.67", "", "3.50",
      "3.95", " ", "90% CI", "(1.91, 7.09)", "", "(2.41, 14.12)", "(0.59, 4.74)",
      "", "(1.45, 8.45)", "(1.45, 10.74)", " ", "p-value (Chi-Squared Test)",
      "0.0007", "", "0.0004", "0.4150", "", "0.0154", "0.0178"
    ),
    .Dim = c(9L, 5L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("ONCT05 variant 4 (setting values indicating response) is produced correctly", {

  # Define new criteria for responder.
  adrs <- adrs %>%
    preprocess_adrs(n_records = 200) %>%
    dplyr::mutate(
      new_rsp = AVALC %in% c("CR", "PR")
    )

  df <- extract_rsp_subgroups(
    variables = list(rsp = "new_rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  # Response table.
  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Stratification Factor 2",
      "S1", "S2", " ", "Total n", "200", "", "120", "80", "", "105",
      "95", "B: Placebo", "n", "100", "", "62", "38", "", "48", "52",
      "B: Placebo", "Response (%)", "95.0%", "", "96.8%", "92.1%", "",
      "97.9%", "92.3%", "A: Drug X", "n", "100", "", "58", "42", "",
      "57", "43", "A: Drug X", "Response (%)", "99.0%", "", "98.3%",
      "100.0%", "", "100.0%", "97.7%", " ", "Odds Ratio", "5.21", "", "1.90",
      ">999.99", "", ">999.99", "3.50", " ", "95% CI", "(0.60, 45.43)",
      "", "(0.17, 21.53)", "(0.00, >999.99)", "", "(0.00, >999.99)",
      "(0.38, 32.55)"
    ),
    .Dim = 9:8
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})
