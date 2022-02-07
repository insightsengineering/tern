# Test all tables for MMRMT01.

library(dplyr)
library(tern)
library(scda)
library(broom)

adqs <- synthetic_cdisc_data("rcd_2021_05_05")$adqs
adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl

# nolint start
adqs_f <- adqs %>%
  dplyr::filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
  droplevels() %>%
  dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
  dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
# nolint end

too_old_lme4 <- compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0


mmrm_results <- if (too_old_lme4) {
  NULL
} else {
  fit_mmrm(
    vars = list(
      response = "AVAL",
      covariates = c("STRATA2"),
      id = "USUBJID",
      arm = "ARM",
      visit = "AVISIT"
    ),
    data = adqs_f,
    cor_struct = "unstructured",
    weights_emmeans = "proportional",
    optimizer = "automatic"
  )
}


testthat::test_that("LS means table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  df <- broom::tidy(mmrm_results)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = mmrm_results$ref_level) %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(show_relative = "increase") %>%
    build_table(df, alt_counts_df = adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "SCREENING", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "WEEK 1 DAY 8", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "WEEK 2 DAY 15", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "WEEK 3 DAY 22", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "WEEK 4 DAY 29", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "WEEK 5 DAY 36", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)", "p-value (MMRM)",
      "B: Placebo", "(N=134)",
      "", "134", "45.233 (0.737)", "(43.784, 46.682)", "", "", "", "",
      "", "134", "54.350 (0.691)", "(52.992, 55.707)", "", "", "", "",
      "", "134", "59.997 (0.765)", "(58.494, 61.5)", "", "", "", "",
      "", "134", "64.409 (0.866)", "(62.706, 66.112)", "", "", "", "",
      "", "134", "68.964 (0.993)", "(67.012, 70.917)", "", "", "", "",
      "", "134", "74.365 (1.057)", "(72.288, 76.442)", "", "", "", "",
      "A: Drug X", "(N=134)",
      "", "134", "44.567 (0.737)", "(43.118, 46.017)", "-0.666 (1.043)", "(-2.715, 1.384)", "-1.5%", "0.5235",
      "", "134", "55.021 (0.691)", "(53.663, 56.379)", "0.671 (0.977)", "(-1.249, 2.591)", "1.2%", "0.4924",
      "", "134", "59.793 (0.765)", "(58.289, 61.297)", "-0.204 (1.082)", "(-2.33, 1.922)", "-0.3%", "0.8504",
      "", "134", "66.788 (0.866)", "(65.085, 68.492)", "2.380 (1.225)", "(-0.029, 4.788)", "3.7%", "0.0528",
      "", "134", "70.253 (0.993)", "(68.3, 72.206)", "1.289 (1.405)", "(-1.472, 4.051)", "1.9%", "0.3593",
      "", "134", "75.705 (1.057)", "(73.628, 77.783)", "1.340 (1.494)", "(-1.598, 4.278)", "1.8%", "0.3704",
      "C: Combination", "(N=132)",
      "", "132", "44.701 (0.743)", "(43.24, 46.162)", "-0.533 (1.047)", "(-2.591, 1.525)", "-1.2%", "0.6112",
      "", "132", "53.941 (0.696)", "(52.572, 55.31)", "-0.409 (0.981)", "(-2.337, 1.52)", "-0.8%", "0.6772",
      "", "132", "59.288 (0.771)", "(57.772, 60.803)", "-0.709 (1.086)", "(-2.844, 1.426)", "-1.2%", "0.5140",
      "", "132", "66.568 (0.873)", "(64.851, 68.284)", "2.159 (1.230)", "(-0.259, 4.577)", "3.4%", "0.0800",
      "", "132", "70.289 (1.001)", "(68.321, 72.257)", "1.325 (1.410)", "(-1.447, 4.098)", "1.9%", "0.3480",
      "", "132", "74.482 (1.065)", "(72.389, 76.576)", "0.117 (1.500)", "(-2.832, 3.066)", "0.2%", "0.9378"
    ),
    .Dim = c(50L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Fixed effects table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "fixed")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "(Intercept)", "STRATA2S2", "ARMA: Drug X", "ARMC: Combination",
      "AVISITWEEK 1 DAY 8", "AVISITWEEK 2 DAY 15", "AVISITWEEK 3 DAY 22", "AVISITWEEK 4 DAY 29", "AVISITWEEK 5 DAY 36",
      "ARMA: Drug X:AVISITWEEK 1 DAY 8", "ARMC: Combination:AVISITWEEK 1 DAY 8",
      "ARMA: Drug X:AVISITWEEK 2 DAY 15", "ARMC: Combination:AVISITWEEK 2 DAY 15",
      "ARMA: Drug X:AVISITWEEK 3 DAY 22", "ARMC: Combination:AVISITWEEK 3 DAY 22",
      "ARMA: Drug X:AVISITWEEK 4 DAY 29", "ARMC: Combination:AVISITWEEK 4 DAY 29",
      "ARMA: Drug X:AVISITWEEK 5 DAY 36", "ARMC: Combination:AVISITWEEK 5 DAY 36",
      "Estimate", "45.2996", "-0.1302", "-0.6657", "-0.5326", "9.1165", "14.7637", "19.1756",
      "23.731", "29.1319", "1.3369", "0.1241", "0.4617", "-0.1766", "3.0454", "2.6914", "1.955",
      "1.8577", "2.0059", "0.6498", "Std. Error", "0.762", "0.3862", "1.0426", "1.0468", "0.992",
      "1.0685", "1.1449", "1.2304", "1.2525", "1.4029", "1.4082", "1.511", "1.5168", "1.6191",
      "1.6252", "1.74", "1.7466", "1.7713", "1.778", "t value", "59.4468", "-0.3372", "-0.6385",
      "-0.5088", "9.1898", "13.8177", "16.7488", "19.2875", "23.2596", "0.9529", "0.0881", "0.3055",
      "-0.1164", "1.8809", "1.656", "1.1235", "1.0636", "1.1324", "0.3655",
      "df", "439", "396", "397", "397", "397", "397", "397", "397", "397", "397", "397", "397", "397",
      "397", "397", "397", "397", "397", "397", "Pr(>|t|)", "<0.0001", "0.7362", "0.5235", "0.6112",
      "<0.0001", "<0.0001", "<0.0001", "<0.0001", "<0.0001", "0.3412", "0.9298", "0.7601", "0.9074",
      "0.0607", "0.0985", "0.2619", "0.2881", "0.2581", "0.7150"
    ),
    .Dim = c(20L, 6L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Covariance matrix table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "cov")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "SCREENING", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36",
      "SCREENING", "72.813", "2.4211", "-0.9076", "-1.1484", "1.0733", "6.0976", "WEEK 1 DAY 8", "2.4211",
      "63.9014", "-0.3814", "2.6581", "-1.2883", "0.6494", "WEEK 2 DAY 15", "-0.9076", "-0.3814", "78.3489",
      "1.1637", "-12.1309", "-0.6337", "WEEK 3 DAY 22", "-1.1484", "2.6581", "1.1637", "100.5352", "-6.3289",
      "6.6827", "WEEK 4 DAY 29", "1.0733", "-1.2883", "-12.1309", "-6.3289", "132.1878", "-5.5313",
      "WEEK 5 DAY 36", "6.0976", "0.6494", "-0.6337", "6.6827", "-5.5313", "149.5853"
    ),
    .Dim = c(7L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Model diagnostics table is produced correctly", {
  testthat::skip_if(too_old_lme4, "lme4 version is <= 1.1.21, a newer version is needed for the test.")

  result <- as.rtable(mmrm_results, type = "diagnostic")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "REML criterion", "AIC", "AICc", "BIC", "Diagnostic statistic value", "17677.3763", "17719.3763",
      "17719.768", "17803.1971"
    ),
    .Dim = c(5L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
