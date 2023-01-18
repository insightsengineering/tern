# Tests all variants of AOVT01

adsl <- adsl_raw
adqs <- adqs_raw

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
