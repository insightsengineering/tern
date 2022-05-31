# Tests all variants of AOVT01

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adqs <- synthetic_cdisc_data("rcd_2022_02_28")$adqs

testthat::test_that("AOVT01 variant with single endpoint is produced correctly", {
  adqs_single <- adqs %>%
    dplyr::filter(
      AVISIT == "WEEK 1 DAY 8", # single time point
      PARAMCD == "FKSI-FWB" # single end point
    ) %>%
    dplyr::mutate(CHG = ifelse(BMEASIFL == "Y", CHG, NA)) # only analyze evaluable population

  result <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = NULL),
      conf_level = 0.95, var_labels = "Unadjusted comparison",
      .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
      table_names = "unadjusted"
    ) %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
      conf_level = 0.95, var_labels = "Adjusted comparison (covariates BASE and STRATA1)",
      table_names = "adjusted"
    ) %>%
    build_table(adqs_single, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Unadjusted comparison", "n", "Mean", "Difference in Means", "95% CI", "p-value",
      "Adjusted comparison (covariates BASE and STRATA1)", "n", "Adjusted Mean",
      "Difference in Adjusted Means", "95% CI", "p-value",
      "ARM A", "(N=134)", "", "68", "3.68", "", "", "", "", "68", "4.06", "", "", "",
      "ARM B", "(N=134)", "", "73", "5.07", "1.38", "(-2.76, 5.53)", "0.5113", "",
      "73", "3.57", "-0.49", "(-3.28, 2.29)", "0.7277",
      "ARM C", "(N=132)", "", "62", "3.09", "-0.59", "(-4.91, 3.73)", "0.7873", "",
      "62", "3.34", "-0.72", "(-3.57, 2.12)", "0.6165"
    ),
    .Dim = c(14L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AOVT01 variant with multiple endpoints is produced correctly", {
  adqs_multi <- dplyr::filter(adqs, AVISIT == "WEEK 1 DAY 8")
  n_per_arm <- table(adsl$ARM)

  result <- basic_table() %>%
    split_cols_by("ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    summarize_ancova(
      vars = "CHG",
      variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
      conf_level = 0.95, var_labels = "Adjusted mean"
    ) %>%
    build_table(adqs_multi, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "BFIALL", "Adjusted mean", "n", "Adjusted Mean",
      "Difference in Adjusted Means", "95% CI", "p-value",
      "FATIGI", "Adjusted mean", "n", "Adjusted Mean", "Difference in Adjusted Means", "95% CI", "p-value",
      "FKSI-FWB", "Adjusted mean", "n", "Adjusted Mean", "Difference in Adjusted Means", "95% CI", "p-value",
      "FKSI-TSE", "Adjusted mean", "n", "Adjusted Mean", "Difference in Adjusted Means", "95% CI", "p-value",
      "FKSIALL", "Adjusted mean", "n", "Adjusted Mean", "Difference in Adjusted Means", "95% CI", "p-value",
      "ARM A", "(N=134)", "", "", "134", "4.47", "", "", "", "", "", "134", "5.42", "", "", "", "", "", "134", "4.29",
      "", "", "", "", "", "134", "4.70", "", "", "", "", "", "134", "5.03", "", "", "",
      "ARM B", "(N=134)", "", "", "134", "6.33", "1.85", "(-0.14, 3.85)", "0.0679", "", "",
      "134", "4.83", "-0.59", "(-2.58, 1.41)", "0.5644", "", "",
      "134", "3.51", "-0.79", "(-2.71, 1.14)", "0.4221", "", "",
      "134", "3.84", "-0.86", "(-2.80, 1.09)", "0.3858", "", "",
      "134", "5.82", "0.79", "(-1.17, 2.76)", "0.4288",
      "ARM C", "(N=132)", "", "", "132", "4.02", "-0.46", "(-2.45, 1.54)", "0.6539", "", "",
      "132", "4.56", "-0.86", "(-2.87, 1.15)", "0.4026", "", "",
      "132", "3.06", "-1.24", "(-3.17, 0.69)", "0.2088", "", "",
      "132", "4.45", "-0.25", "(-2.20, 1.70)", "0.8007", "", "",
      "132", "6.44", "1.42", "(-0.56, 3.39)", "0.1591"
    ),
    .Dim = c(37L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
