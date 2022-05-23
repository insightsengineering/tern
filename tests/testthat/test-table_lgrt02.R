# Tests variants of LGRT02.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adrs <- synthetic_cdisc_data("rcd_2022_02_28")$adrs

adsl_cached <- adsl %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(adsl))
adrs_cached <- adrs %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(adrs))


get_adrs <- function() {
  adrs_f <- adrs_cached %>%
    dplyr::filter(
      PARAMCD == "BESRSPI"
    ) %>%
    dplyr::mutate(
      Response = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
      SEX = factor(SEX, c("M", "F")),
      RACE = as.character(RACE),
      RACE = factor(
        RACE,
        levels = c(
          "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN", "BLACK OR AFRICAN AMERICAN",
          "WHITE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"
        )
      )
    )
  formatters::var_labels(adrs_f) <- c(formatters::var_labels(adrs_cached), Response = "Response") # nolint
  adrs_f
}

testthat::test_that("LGRT02 without interaction term is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(response = "Response", arm = "ARMCD", covariates = c("SEX", "RACE", "AGE"))
  )
  conf_level <- 0.95
  df <- broom::tidy(model, conf_level = conf_level)
  result <- basic_table() %>%
    summarize_logistic(conf_level = conf_level) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Planned Arm Code", "Reference ARM A, n = 134",
      "ARM B, n = 134", "ARM C, n = 132", "Sex", "Reference M, n = 169",
      "F, n = 231", "Race", "Reference AMERICAN INDIAN OR ALASKA NATIVE, n = 25",
      "ASIAN, n = 208", "BLACK OR AFRICAN AMERICAN, n = 91", "WHITE, n = 74",
      "MULTIPLE, n = 1", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, n = 1",
      "Age", "Age", "Degrees of Freedom", "2", "", "1", "1", "", "",
      "1", "5", "", "1", "1", "1", "1", "1", "", "1", "Parameter Estimate",
      "", "", "-2.162", "-0.090", "", "", "0.364", "", "", "-16.246",
      "-15.205", "-15.955", "-0.363", "1.036", "", "0.071", "Standard Error",
      "", "", "1.084", "1.426", "", "", "0.701", "", "", "2017.122",
      "2017.122", "2017.122", "10941.553", "10941.553", "", "0.053",
      "Odds Ratio", "", "", "0.12", "0.91", "", "", "1.44", "", "",
      "0.00", "0.00", "0.00", "0.70", "2.82", "", "1.07", "Wald 95% CI", "",
      "", "(0.01, 0.96)", "(0.06, 14.97)", "", "", "(0.36, 5.69)",
      "", "", "(0.00, >999.99)", "(0.00, >999.99)", "(0.00, >999.99)",
      "(0.00, >999.99)", "(0.00, >999.99)", "", "(0.97, 1.19)", "p-value",
      "0.0346", "", "0.0461", "0.9499", "", "", "0.6032", "0.9685",
      "", "0.9936", "0.9940", "0.9937", "1.0000", "0.9999", "", "0.1866"
    ),
    .Dim = c(17L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LGRT02 with categorical interaction is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("SEX", "AGE"),
      interaction = "SEX"
    )
  )
  conf_level <- 0.95
  df <- broom::tidy(model, conf_level = conf_level)
  result <- basic_table() %>%
    summarize_logistic(conf_level = conf_level) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Age", "Age", "Planned Arm Code", "Reference ARM A, n = 134",
      "ARM B, n = 134", "Sex", "F", "M", "ARM C, n = 132", "Sex", "F",
      "M", "Sex", "Reference M, n = 169", "F, n = 231", "Planned Arm Code",
      "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Sex",
      "Reference ARM A or M, n = 248", "ARM B * F, n = 82", "ARM C * F, n = 70",
      "Degrees of Freedom", "", "1", "2", "", "1", "", "", "", "1",
      "", "", "", "", "", "1", "", "", "", "", "2", "", "1", "1", "Parameter Estimate",
      "", "0.067", "", "", "-17.850", "", "", "", "-16.442", "", "",
      "", "", "", "-16.044", "", "", "", "", "", "", "16.373", "32.492",
      "Standard Error", "", "0.054", "", "", "2362.767", "", "", "",
      "2362.767", "", "", "", "", "", "2362.767", "", "", "", "", "",
      "", "2362.767", "3156.732", "Odds Ratio", "", "1.07", "", "",
      "", "", "0.23", "0.00", "", "", ">999.99", "0.00", "", "", "", "",
      "0.00", "1.39", ">999.99", "", "", "", "", "Wald 95% CI", "", "(0.96, 1.19)",
      "", "", "", "", "(0.02, 2.11)", "(0.00, >999.99)", "", "", "(0.00, >999.99)",
      "(0.00, >999.99)", "", "", "", "", "(0.00, >999.99)", "(0.29, 6.59)",
      "(0.00, >999.99)", "", "", "", "", "p-value", "", "0.2084", "0.4882",
      "", "0.9940", "", "", "", "0.9944", "", "", "", "", "", "0.9946",
      "", "", "", "", "0.9999", "", "0.9945", "0.9918"
    ),
    .Dim = c(24L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LGRT02 with continuous interaction is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("SEX", "AGE"),
      interaction = "AGE"
    )
  )
  conf_level <- 0.95
  df <- broom::tidy(model, conf_level = conf_level, at = c(18, 65))
  result <- basic_table() %>%
    summarize_logistic(conf_level = conf_level) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Sex", "Reference M, n = 169", "F, n = 231",
      "Planned Arm Code", "Reference ARM A, n = 134", "ARM B, n = 134",
      "Age", "18", "65", "ARM C, n = 132", "Age", "18", "65", "Age",
      "Age", "Planned Arm Code", "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Age",
      "Reference ARM A, n = 134", "ARM B, n = 134", "ARM C, n = 132",
      "Degrees of Freedom", "", "", "1", "2", "", "1", "", "", "",
      "1", "", "", "", "", "1", "", "", "", "", "2", "", "1", "1",
      "Parameter Estimate", "", "", "0.381", "", "", "20.020", "", "",
      "", "15.622", "", "", "", "", "0.877", "", "", "", "", "", "",
      "-0.849", "-0.636", "Standard Error", "", "", "0.710", "", "",
      "13.714", "", "", "", "14.810", "", "", "", "", "0.581", "", "",
      "", "", "", "", "0.583", "0.618", "Odds Ratio", "", "", "1.46",
      "", "", "", "", "113.59", "0.00", "", "", "64.74", "0.00", "", "",
      "", "2.40", "1.03", "1.27", "", "", "", "", "Wald 95% CI", "",
      "", "(0.36, 5.88)", "", "", "", "", "(0.14, >999.99)", "(<0.01, >999.99)",
      "", "", "(0.03, >999.99)", "(<0.01, >999.99)", "", "", "", "(0.77, 7.50)",
      "(0.93, 1.14)", "(0.84, 1.93)", "", "", "", "", "p-value", "",
      "", "0.5915", "0.2768", "", "0.1443", "", "", "", "0.2915", "",
      "", "", "", "0.1309", "", "", "", "", "0.2213", "", "0.1449",
      "0.3034"
    ),
    .Dim = c(24L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LGRT02 with setting values indicating an event and custom alpha level is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("SEX", "AGE", "RACE")
    ),
    response_definition = "1 - response"
  )
  conf_level <- 0.9
  df <- broom::tidy(model, conf_level = conf_level)
  result <- basic_table() %>%
    summarize_logistic(conf_level = conf_level) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Planned Arm Code", "Reference ARM A, n = 134",
      "ARM B, n = 134", "ARM C, n = 132", "Sex", "Reference M, n = 169",
      "F, n = 231", "Age", "Age", "Race", "Reference AMERICAN INDIAN OR ALASKA NATIVE, n = 25",
      "ASIAN, n = 208", "BLACK OR AFRICAN AMERICAN, n = 91", "WHITE, n = 74",
      "MULTIPLE, n = 1", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, n = 1",
      "Degrees of Freedom", "2", "", "1", "1", "", "", "1", "", "1",
      "5", "", "1", "1", "1", "1", "1", "Parameter Estimate", "", "",
      "2.162", "0.090", "", "", "-0.364", "", "-0.071", "", "", "16.246",
      "15.205", "15.955", "0.363", "-1.036", "Standard Error", "",
      "", "1.084", "1.426", "", "", "0.701", "", "0.053", "", "", "2017.122",
      "2017.122", "2017.122", "10941.553", "10941.553", "Odds Ratio",
      "", "", "8.69", "1.09", "", "", "0.69", "", "0.93", "", "", ">999.99",
      ">999.99", ">999.99", "1.44", "0.35", "Wald 90% CI", "", "",
      "(1.46, 51.66)", "(0.10, 11.43)", "", "", "(0.22, 2.20)", "",
      "(0.85, 1.02)", "", "", "(0.00, >999.99)", "(0.00, >999.99)",
      "(0.00, >999.99)", "(0.00, >999.99)", "(0.00, >999.99)", "p-value",
      "0.0346", "", "0.0461", "0.9499", "", "", "0.6032", "", "0.1866",
      "0.9685", "", "0.9936", "0.9940", "0.9937", "1.0000", "0.9999"
    ),
    .Dim = c(17L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
