library(scda)
library(dplyr)

preprocess_adrs <- function(adrs) {

  # Save variable labels before data processing steps.
  adrs_labels <- formatable::var_labels(adrs)

  adrs_mod <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(rsp = AVALC == "CR")

  reapply_varlabels(
    adrs_mod,
    adrs_labels,
    rsp = "Response"
  )
}

adrs <- synthetic_cdisc_data("rcd_2021_05_05")$adrs

# h_rsp_to_logistic_variables ----

testthat::test_that("h_rsp_to_logistic_variables works as expected", {
  result <- testthat::expect_silent(h_rsp_to_logistic_variables(
    variables = list(
      rsp = "RSP",
      covariates = c("A", "B"),
      strat = "D"
    ),
    biomarker = "AGE"
  ))
  expected <- list(
    response = "RSP",
    arm = "AGE",
    covariates = c("A", "B"),
    strata = "D"
  )
  testthat::expect_identical(result, expected)
})

# h_logistic_mult_cont_df ----

testthat::test_that("h_logistic_mult_cont_df works as expected", {
  adrs_f <- adrs %>%
    preprocess_adrs()

  result <- testthat::expect_silent(h_logistic_mult_cont_df(
    variables = list(
      rsp = "rsp",
      biomarkers = c("BMRKR1", "AGE"),
      covariates = "SEX",
      strata = "STRATA2"
    ),
    data = adrs_f
  ))
  expected <- data.frame(
    biomarker = c("BMRKR1", "AGE"),
    biomarker_label = c("Continous Level Biomarker 1", "Age"),
    n_tot = c(400L, 400L),
    n_rsp = c(336L, 336L),
    prop = c(0.84, 0.84),
    or = c(1.05741036237079, 0.998839149282868),
    lcl = c(0.97168048155206, 0.963368082787044),
    ucl = c(1.15070405928415, 1.03561625505987),
    conf_level = c(0.95, 0.95),
    pval = c(0.195658638331358, 0.949797749266217),
    pval_label = c("p-value (Wald)", "p-value (Wald)")
  )
  testthat::expect_equal(result, expected, tol = 1e-5)
})

testthat::test_that("h_logistic_mult_cont_df returns missing values if data is empty (0 rows)", {
  adrs_f <- adrs %>%
    preprocess_adrs()

  result <- testthat::expect_silent(h_logistic_mult_cont_df(
    variables = list(
      rsp = "rsp",
      biomarkers = c("BMRKR1", "AGE"),
      covariates = "SEX",
      strata = "STRATA2"
    ),
    data = adrs_f[NULL, ]
  ))
  expected <- data.frame(
    biomarker = c("BMRKR1", "AGE"),
    biomarker_label = c("Continous Level Biomarker 1", "Age"),
    n_tot = c(0L, 0L),
    n_rsp = c(0L, 0L),
    prop = c(NA, NA),
    or = c(NA, NA),
    lcl = c(NA, NA),
    ucl = c(NA, NA),
    conf_level = c(0.95, 0.95),
    pval = c(NA, NA),
    pval_label = c("p-value (Wald)", "p-value (Wald)")
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_logistic_mult_cont_df also works with response not being called rsp", {
  adrs_f <- adrs %>%
    preprocess_adrs() %>%
    dplyr::rename(RESP = rsp)

  result <- testthat::expect_silent(h_logistic_mult_cont_df(
    variables = list(
      rsp = "RESP",
      biomarkers = c("BMRKR1", "AGE"),
      covariates = "SEX",
      strata = "STRATA2"
    ),
    data = adrs_f
  ))
  expected <- data.frame(
    biomarker = c("BMRKR1", "AGE"),
    biomarker_label = c("Continous Level Biomarker 1", "Age"),
    n_tot = c(400L, 400L),
    n_rsp = c(336L, 336L),
    prop = c(0.84, 0.84),
    or = c(1.05741036237079, 0.998839149282868),
    lcl = c(0.97168048155206, 0.963368082787044),
    ucl = c(1.15070405928415, 1.03561625505987),
    conf_level = c(0.95, 0.95),
    pval = c(0.195658638331358, 0.949797749266217),
    pval_label = c("p-value (Wald)", "p-value (Wald)")
  )
  testthat::expect_equal(result, expected, tol = 1e-5)
})

# h_tab_rsp_one_biomarker ----

testthat::test_that("h_tab_rsp_one_biomarker works as expected", {
  df <- data.frame(
    n_tot = c(48L, 48L),
    n_rsp = c(24L, 24L),
    prop = c(0.5, 0.5),
    or = c(0.992727618706316, 1.00485769099575),
    lcl = c(0.859391304891713, 0.950491104268725),
    ucl = c(1.14675133356916, 1.06233396043214),
    conf_level = c(0.95, 0.95),
    pval = c(0.920991170690111, 0.864415775291559),
    pval_label = c("p-value (Wald)", "p-value (Wald)"),
    subgroup = c("All patients", "All patients"),
    row_type = c("content", "content"),
    var = c("ALL", "ALL"),
    var_label = c("All patients", "All patients")
  )
  result <- testthat::expect_silent(h_tab_rsp_one_biomarker(
    df = df,
    vars = c("n_tot", "or", "ci")
  ))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- matrix(
    nrow = 3, ncol = 4,
    data = c(
      "", "All patients", "All patients", "Total n", "48",
      "48", "Odds Ratio", "0.99", "1.00", "95% CI", "(0.86, 1.15)",
      "(0.95, 1.06)"
    )
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
