
# Preparation of the test case.

# nolint start
library(tern)
library(scda)
library(broom)

ADTTE <- synthetic_cdisc_data("rcd_2022_02_28")$adtte
saved_labels <- formatters::var_labels(ADTTE)

ADTTE_f <- subset(ADTTE, PARAMCD == "OS") # _f: filtered
ADTTE_f <- within(
  data = subset(
    ADTTE_f,
    PARAMCD == "OS" &
      ARMCD %in% c("ARM A", "ARM B") &
      SEX %in% c("F", "M") &
      RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ),
  expr = {
    ARMCD <- droplevels(ARMCD)
    ARMCD <- stats::relevel(ARMCD, "ARM B")
    SEX <- droplevels(SEX)
    RACE <- droplevels(RACE)
  }
)
formatters::var_labels(ADTTE_f) <- saved_labels
ADTTE_f$event <- 1 - ADTTE_f$CNSR
# nolint end

testthat::test_that("1. Cox Regression", {
  mod1 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f
  )
  df <- broom::tidy(mod1)
  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "Race", "Age", "n", "", "247", "", "247", "247", "247",
      "Hazard Ratio", "", "0.70", "", "0.71", "0.70", "0.70", "95% CI",
      "", "(0.51, 0.96)", "", "(0.52, 0.98)", "(0.51, 0.97)", "(0.51, 0.97)",
      "p-value", "", "0.0293", "", "0.0370", "0.0318", "0.0321"
    ),
    .Dim = c(7L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("2. Cox Regression (with Interaction Term)", {
  mod2 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(interaction = TRUE)
  )
  df <- broom::tidy(mod2)
  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  34", "n", "", "247", "", "247", "", "",
      "247", "", "", "", "247", "", "Hazard Ratio", "", "0.70", "",
      "", "0.64", "0.82", "", "0.75", "0.66", "0.65", "", "0.70", "95% CI",
      "", "(0.51, 0.96)", "", "", "(0.42, 0.98)", "(0.50, 1.35)", "",
      "(0.48, 1.16)", "(0.34, 1.28)", "(0.33, 1.27)", "", "(0.51, 0.97)",
      "p-value", "", "0.0293", "", "", "", "", "", "",
      "", "", "", "", "Interaction p-value", "", "", "", "0.4635",
      "", "", "0.9197", "", "", "", "0.8626", ""
    ),
    .Dim = c(13L, 6L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("3. Cox Regression (specifying covariates)", {
  mod3 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(interaction = TRUE),
    at = list(AGE = c(30, 40, 50))
  )
  df <- broom::tidy(mod3)
  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  30", "  40", "  50", "n", "", "247", "",
      "247", "", "", "247", "", "", "", "247", "", "", "", "Hazard Ratio",
      "", "0.70", "", "", "0.64", "0.82", "", "0.75", "0.66", "0.65",
      "", "0.69", "0.72", "0.75", "95% CI", "", "(0.51, 0.96)", "",
      "", "(0.42, 0.98)", "(0.50, 1.35)", "", "(0.48, 1.16)", "(0.34, 1.28)",
      "(0.33, 1.27)", "", "(0.48, 1.00)", "(0.48, 1.08)", "(0.35, 1.61)",
      "p-value", "", "0.0293", "", "", "", "", "", "",
      "", "", "", "", "", "", "Interaction p-value", "", "",
      "", "0.4635", "", "", "0.9197", "", "", "", "0.8626", "", "",
      ""
    ),
    .Dim = c(15L, 6L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("4. Cox Regression (setting strata, ties, and alpha level)", {
  conf_level <- 0.90
  mod4 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(
      interaction = TRUE,
      conf_level = conf_level,
      ties = "efron"
    ),
    at = list(AGE = c(30, 40, 50))
  )
  df <- broom::tidy(mod4)
  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = conf_level, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  30", "  40", "  50", "n", "", "247", "",
      "247", "", "", "247", "", "", "", "247", "", "", "", "Hazard Ratio",
      "", "0.70", "", "", "0.64", "0.82", "", "0.75", "0.66", "0.65",
      "", "0.69", "0.72", "0.75", "90% CI", "", "(0.53, 0.92)", "",
      "", "(0.42, 0.98)", "(0.50, 1.35)", "", "(0.48, 1.16)", "(0.34, 1.28)",
      "(0.33, 1.27)", "", "(0.51, 0.94)", "(0.51, 1.02)", "(0.39, 1.42)",
      "p-value", "", "0.0293", "", "", "", "", "", "",
      "", "", "", "", "", "", "Interaction p-value", "", "",
      "", "0.4635", "", "", "0.9197", "", "", "", "0.8626", "", "",
      ""
    ),
    .Dim = c(15L, 6L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
