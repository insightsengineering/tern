# Test all tables for MMRMT01.

library(dplyr)
library(tern)
library(scda)
library(broom)

adqs <- synthetic_cdisc_data("rcd_2021_05_05")$adqs
adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl

#nolint start
adqs_f <- adqs %>%
  dplyr::filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
  droplevels() %>%
  dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
  dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#nolint end

too_old_lme4 <- compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0


mmrm_results <- if (too_old_lme4) {
  NULL
} else {
  fit_mmrm(
    vars = list(
      response = "AVAL",
      covariates = c("STRATA1", "BMRKR2"),
      id = "USUBJID",
      arm = "ARM",
      visit = "AVISIT"
    ),
    data = adqs_f,
    cor_struct = "unstructured",
    weights_emmeans = "proportional",
    optimizer = "nloptwrap_neldermead"
  )
}


testthat::test_that("LS means table is produced correctly", {

  skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  df <- broom::tidy(mmrm_results)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = mmrm_results$ref_level) %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(show_relative = "increase") %>%
    build_table(df, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "", "SCREENING", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 1 DAY 8", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 2 DAY 15", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 3 DAY 22", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 4 DAY 29", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 5 DAY 36", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "B: Placebo", "(N=134)", "", "134", "45.190 (0.738)",
      "(43.739, 46.64)", "", "", "", "", "", "134", "54.306 (0.689)",
      "(52.952, 55.661)", "", "", "", "", "", "134", "59.954 (0.766)",
      "(58.448, 61.459)", "", "", "", "", "", "134", "64.366 (0.870)",
      "(62.655, 66.076)", "", "", "", "", "", "134", "68.921 (0.994)",
      "(66.967, 70.875)", "", "", "", "", "", "134", "74.322 (1.057)",
      "(72.243, 76.4)", "", "", "", "", "A: Drug X", "(N=134)", "",
      "134", "44.578 (0.737)", "(43.129, 46.028)", "-0.611 (1.044)",
      "(-2.664, 1.441)", "-1.4%", "0.5583", "", "134", "55.032 (0.688)",
      "(53.679, 56.385)", "0.725 (0.975)", "(-1.191, 2.642)", "1.3%",
      "0.4572", "", "134", "59.804 (0.765)", "(58.299, 61.308)", "-0.150 (1.083)",
      "(-2.28, 1.98)", "-0.2%", "0.8901", "", "134", "66.799 (0.870)",
      "(65.09, 68.509)", "2.434 (1.231)", "(0.014, 4.854)", "3.8%",
      "0.0487", "", "134", "70.264 (0.994)", "(68.311, 72.218)", "1.343 (1.406)",
      "(-1.421, 4.108)", "1.9%", "0.3399", "", "134", "75.716 (1.057)",
      "(73.638, 77.794)", "1.394 (1.495)", "(-1.546, 4.334)", "1.9%",
      "0.3517", "C: Combination", "(N=132)", "", "132", "44.733 (0.743)",
      "(43.273, 46.193)", "-0.457 (1.048)", "(-2.516, 1.603)", "-1%",
      "0.6630", "", "132", "53.974 (0.693)", "(52.611, 55.337)", "-0.333 (0.978)",
      "(-2.256, 1.59)", "-0.6%", "0.7339", "", "132", "59.320 (0.771)",
      "(57.805, 60.836)", "-0.633 (1.087)", "(-2.771, 1.504)", "-1.1%",
      "0.5605", "", "132", "66.600 (0.876)", "(64.878, 68.323)", "2.235 (1.235)",
      "(-0.194, 4.663)", "3.5%", "0.0712", "", "132", "70.322 (1.001)",
      "(68.354, 72.29)", "1.401 (1.411)", "(-1.373, 4.175)", "2%",
      "0.3214", "", "132", "74.515 (1.065)", "(72.422, 76.608)", "0.193 (1.501)",
      "(-2.758, 3.144)", "0.3%", "0.8977"),
    .Dim = c(50L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Fixed effects table is produced correctly", {

  skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "fixed")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "(Intercept)", "STRATA1B", "STRATA1C", "BMRKR2MEDIUM",
      "BMRKR2HIGH", "ARMA: Drug X", "ARMC: Combination", "AVISITWEEK 1 DAY 8",
      "AVISITWEEK 2 DAY 15", "AVISITWEEK 3 DAY 22", "AVISITWEEK 4 DAY 29",
      "AVISITWEEK 5 DAY 36", "ARMA: Drug X:AVISITWEEK 1 DAY 8", "ARMC: Combination:AVISITWEEK 1 DAY 8",
      "ARMA: Drug X:AVISITWEEK 2 DAY 15", "ARMC: Combination:AVISITWEEK 2 DAY 15",
      "ARMA: Drug X:AVISITWEEK 3 DAY 22", "ARMC: Combination:AVISITWEEK 3 DAY 22",
      "ARMA: Drug X:AVISITWEEK 4 DAY 29", "ARMC: Combination:AVISITWEEK 4 DAY 29",
      "ARMA: Drug X:AVISITWEEK 5 DAY 36", "ARMC: Combination:AVISITWEEK 5 DAY 36",
      "Estimate", "45.778", "-0.1907", "-0.3187", "-0.3822", "-0.8639",
      "-0.6115", "-0.4568", "9.1165", "14.7637", "19.1756", "23.731",
      "29.1319", "1.3369", "0.1241", "0.4617", "-0.1766", "3.0454",
      "2.6914", "1.955", "1.8577", "2.0059", "0.6498", "Std. Error",
      "0.833", "0.4804", "0.4736", "0.4701", "0.4735", "1.0439", "1.0475",
      "0.992", "1.0685", "1.1449", "1.2304", "1.2525", "1.4029", "1.4082",
      "1.511", "1.5168", "1.6191", "1.6253", "1.74", "1.7466", "1.7713",
      "1.778", "t value", "54.953", "-0.397", "-0.6728", "-0.8131",
      "-1.8244", "-0.5858", "-0.436", "9.1898", "13.8177", "16.7488",
      "19.2876", "23.2596", "0.9529", "0.0881", "0.3055", "-0.1164",
      "1.8809", "1.656", "1.1235", "1.0636", "1.1324", "0.3655", "df",
      "543", "393", "393", "393", "393", "398", "398", "397", "397",
      "397", "397", "397", "397", "397", "397", "397", "397", "397",
      "397", "397", "397", "397", "Pr(>|t|)", "<0.0001", "0.6916",
      "0.5014", "0.4167", "0.0688", "0.5583", "0.6630", "<0.0001",
      "<0.0001", "<0.0001", "<0.0001", "<0.0001", "0.3412", "0.9298",
      "0.7601", "0.9074", "0.0607", "0.0985", "0.2619", "0.2881", "0.2581",
      "0.7150"),
    .Dim = c(23L, 6L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Covariance matrix table is produced correctly", {

  skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "cov")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "SCREENING", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
      "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36", "SCREENING",
      "72.7364", "2.1266", "-0.9311", "-0.8374", "1.0434", "6.0506",
      "WEEK 1 DAY 8", "2.1266", "63.3894", "-0.6225", "2.7514", "-1.5364",
      "0.3846", "WEEK 2 DAY 15", "-0.9311", "-0.6225", "78.3785", "1.528",
      "-12.1084", "-0.6278", "WEEK 3 DAY 22", "-0.8374", "2.7514",
      "1.528", "101.2345", "-5.9713", "7.0232", "WEEK 4 DAY 29", "1.0434",
      "-1.5364", "-12.1084", "-5.9713", "132.2039", "-5.532", "WEEK 5 DAY 36",
      "6.0506", "0.3846", "-0.6278", "7.0232", "-5.532", "149.5674"
    ),
    .Dim = c(7L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Model diagnostics table is produced correctly", {

  skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "diagnostic")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "REML criterion", "AIC", "AICc", "BIC", "Diagnostic statistic value",
      "17672.9257", "17714.9257", "17715.3179", "17798.7464"),
    .Dim = c(5L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
