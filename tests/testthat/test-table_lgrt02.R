# Tests variants of LGRT02.

library(random.cdisc.data)
library(dplyr)

adsl_cached <- radsl(cached = TRUE) %>% dplyr::filter(SEX %in% c("F", "M"))
adrs_cached <- radrs(adsl_cached, seed = 2)
get_adrs <- function() {
  adrs_f <- adrs_cached %>%
    dplyr::filter(
      PARAMCD == "BESRSPI"
    ) %>%
    dplyr::mutate(
      Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
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
  var_labels(adrs_f) <- c(var_labels(adrs_cached), Response = "Response")  #nolint
  adrs_f
}

test_that("LGRT02 without interaction term is produced correctly", {
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
    c("", "Planned Arm Code", "Reference ARM A, n = 134",
      "ARM B, n = 134", "ARM C, n = 132", "Sex", "Reference M, n = 169",
      "F, n = 231", "Race", "Reference AMERICAN INDIAN OR ALASKA NATIVE, n = 25",
      "ASIAN, n = 208", "BLACK OR AFRICAN AMERICAN, n = 91", "WHITE, n = 74",
      "MULTIPLE, n = 1", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, n = 1",
      "Age", "Age", "Degrees of Freedom", "2", "", "1", "1", "", "",
      "1", "5", "", "1", "1", "1", "1", "1", "", "1", "Parameter Estimate",
      "", "", "-1.426", "16.837", "", "", "1.086", "", "", "-0.088",
      "-0.389", "0.151", "18.626", "-23.632", "", "-0.002", "Standard Error",
      "", "", "0.672", "1512.662", "", "", "0.588", "", "", "1.118",
      "1.17", "1.274", "17730.37", "17730.37", "", "0.037", "Odds Ratio",
      "", "", "0.24", ">999.99", "", "", "2.96", "", "", "0.92", "0.68",
      "1.16", ">999.99", "0", "", "1", "Wald 95% CI", "", "", "(0.06, 0.90)",
      "(0.00, >999.99)", "", "", "(0.94, 9.39)", "", "", "(0.10, 8.20)",
      "(0.07, 6.72)", "(0.10, 14.13)", "(0.00, >999.99)", "(0.00, >999.99)",
      "", "(0.93, 1.07)", "p-value", "0.1051", "", "0.0338", "0.9911",
      "", "", "0.0647", "0.9950", "", "0.9374", "0.7395", "0.9059",
      "0.9992", "0.9989", "", "0.9662"),
    .Dim = c(17L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LGRT02 with categorical interaction is produced correctly", {
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
    c("", "Age", "Age", "Planned Arm Code", "Reference ARM A, n = 134",
      "ARM B, n = 134", "Sex", "F", "M", "ARM C, n = 132", "Sex", "F",
      "M", "Sex", "Reference M, n = 169", "F, n = 231", "Planned Arm Code",
      "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Sex",
      "Reference ARM A or M, n = 248", "ARM B * F, n = 82", "ARM C * F, n = 70",
      "Degrees of Freedom", "", "1", "2", "", "1", "", "", "", "1",
      "", "", "", "", "", "1", "", "", "", "", "2", "", "1", "1", "Parameter Estimate",
      "", "0.009", "", "", "-2.302", "", "", "", "16.573", "", "",
      "", "", "", "-0.317", "", "", "", "", "", "", "1.609", "0.321",
      "Standard Error", "", "0.035", "", "", "1.082", "", "", "", "2251.04",
      "", "", "", "", "", "1.24", "", "", "", "", "", "", "1.394",
      "3091.24", "Odds Ratio", "", "1.01", "", "", "", "", "0.5", "0.1",
      "", "", ">999.99", ">999.99", "", "", "", "", "0.73", "3.64",
      "1", "", "", "", "", "Wald 95% CI", "", "(0.94, 1.08)", "", "",
      "", "", "(0.09, 2.82)", "(0.01, 0.83)", "", "", "(0.00, >999.99)",
      "(0.00, >999.99)", "", "", "", "", "(0.06, 8.28)", "(1.02, 13.00)",
      "(0.00, >999.99)", "", "", "", "", "p-value", "", "0.8031", "0.1041",
      "", "0.0334", "", "", "", "0.9941", "", "", "", "", "", "0.7983",
      "", "", "", "", "0.5135", "", "0.2482", "0.9999"),
    .Dim = c(24L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LGRT02 with continuous interaction is produced correctly", {
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
    c("", "Sex", "Reference M, n = 169", "F, n = 231",
      "Planned Arm Code", "Reference ARM A, n = 134", "ARM B, n = 134",
      "Age", "18", "65", "ARM C, n = 132", "Age", "18", "65", "Age",
      "Age", "Planned Arm Code", "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Age",
      "Reference ARM A, n = 134", "ARM B, n = 134", "ARM C, n = 132",
      "Degrees of Freedom", "", "", "1", "2", "", "1", "", "", "",
      "1", "", "", "", "", "1", "", "", "", "", "2", "", "1", "1",
      "Parameter Estimate", "", "", "0.934", "", "", "-4.031", "",
      "", "", "14.934", "", "", "", "", "-0.051", "", "", "", "", "",
      "", "0.072", "0.053", "Standard Error", "", "", "0.56", "", "",
      "3.447", "", "", "", "7081.178", "", "", "", "", "0.085", "",
      "", "", "", "", "", "0.093", "195.194", "Odds Ratio", "", "",
      "2.54", "", "", "", "", "0.06", "1.86", "", "", ">999.99", ">999.99",
      "", "", "", "0.95", "1.02", "1", "", "", "", "", "Wald 95% CI",
      "", "", "(0.85, 7.63)", "", "", "", "", "(<0.01, 2.32)", "(<0.01, 414.29)",
      "", "", "(0.00, >999.99)", "(0.00, >999.99)", "", "", "", "(0.80, 1.12)",
      "(0.94, 1.10)", "(<0.01, >999.99)", "", "", "", "", "p-value",
      "", "", "0.0956", "0.5045", "", "0.2421", "", "", "", "0.9983",
      "", "", "", "", "0.5446", "", "", "", "", "0.7445", "", "0.4424",
      "0.9998"),
    .Dim = c(24L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LGRT02 with setting values indicating an event and custom alpha level is produced correctly", {
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
    c("", "Planned Arm Code", "Reference ARM A, n = 134",
      "ARM B, n = 134", "ARM C, n = 132", "Sex", "Reference M, n = 169",
      "F, n = 231", "Age", "Age", "Race", "Reference AMERICAN INDIAN OR ALASKA NATIVE, n = 25",
      "ASIAN, n = 208", "BLACK OR AFRICAN AMERICAN, n = 91", "WHITE, n = 74",
      "MULTIPLE, n = 1", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, n = 1",
      "Degrees of Freedom", "2", "", "1", "1", "", "", "1", "", "1",
      "5", "", "1", "1", "1", "1", "1", "Parameter Estimate", "", "",
      "1.426", "-16.837", "", "", "-1.086", "", "0.002", "", "", "0.088",
      "0.389", "-0.151", "-18.626", "23.632", "Standard Error", "",
      "", "0.672", "1512.662", "", "", "0.588", "", "0.037", "", "",
      "1.118", "1.17", "1.274", "17730.37", "17730.37", "Odds Ratio",
      "", "", "4.16", "0", "", "", "0.34", "", "1", "", "", "1.09",
      "1.48", "0.86", "0", ">999.99", "Wald 90% CI", "", "", "(1.38, 12.57)",
      "(0.00, >999.99)", "", "", "(0.13, 0.89)", "", "(0.94, 1.06)",
      "", "", "(0.17, 6.87)", "(0.22, 10.12)", "(0.11, 6.99)", "(0.00, >999.99)",
      "(0.00, >999.99)", "p-value", "0.1051", "", "0.0338", "0.9911",
      "", "", "0.0647", "", "0.9662", "0.9950", "", "0.9374", "0.7395",
      "0.9059", "0.9992", "0.9989"),
    .Dim = c(17L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})
