
# Preparation of the test case.

# nolint start
library(tern)
library(random.cdisc.data)
library(broom)
ADTTE <- radtte(cached = TRUE)
saved_labels <- var_labels(ADTTE)

ADTTE_f <- subset(ADTTE, PARAMCD == "OS") # _f: filtered
ADTTE_f <- within(
  data = subset(
    ADTTE_f,
    PARAMCD == "OS"
    & ARMCD %in% c("ARM A", "ARM B")
    & SEX %in% c("F", "M")
    & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ),
  expr = {
    ARMCD <- droplevels(ARMCD)
    ARMCD <- relevel(ARMCD, "ARM B")
    SEX <- droplevels(SEX)
    RACE <- droplevels(RACE)
  }
)
var_labels(ADTTE_f) <- saved_labels
ADTTE_f$event <- 1 - ADTTE_f$CNSR
# nolint end

test_that("1. Cox Regression", {
  mod1 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f
  )
  df <- tidy(mod1)
  result <- split_rows_by(lyt = NULL, "effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "Race", "Age", "n", "", "247", "", "247", "247", "247",
      "Hazard Ratio", "", "0.78", "", "0.77", "0.78", "0.75", "95% CI", "", "(0.57, 1.07)",
      "", "(0.56, 1.06)", "(0.57, 1.07)", "(0.54, 1.05)", "p-value", "",
      "0.1301", "", "0.1058", "0.1288", "0.0921"
    ),
    .Dim = c(7L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("2. Cox Regression (with Interaction Term)", {
  mod2 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(interaction = TRUE)
  )
  df <- tidy(mod2)
  result <- split_rows_by(lyt = NULL, "effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  34", "n", "", "247", "", "247", "", "",
      "247", "", "", "", "247", "", "Hazard Ratio", "", "0.78", "", "", "0.63",
      "1.05", "", "0.96", "0.56", "0.64", "", "0.75", "95% CI", "",
      "(0.57, 1.07)", "", "", "(0.42, 0.95)", "(0.63, 1.72)", "", "(0.63, 1.46)",
      "(0.3, 1.08)", "(0.31, 1.3)", "", "(0.54, 1.03)", "p-value", "",
      "0.1301", "", "0.0265", "", "", "0.8604", "", "", "", "0.0048",
      "", "Interaction p-value", "", "", "", "0.1243", "", "", "0.3249", "",
      "", "", "0.0108", ""
    ),
    .Dim = c(13L, 6L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("3. Cox Regression (specifying covariates)", {
  mod3 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(interaction = TRUE),
    at = list(AGE = c(30, 40, 50))
  )
  df <- tidy(mod3)
  result <- split_rows_by(lyt = NULL, "effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  30", "  40", "  50", "n", "", "247", "",
      "247", "", "", "247", "", "", "", "247", "", "", "", "Hazard Ratio", "",
      "0.78", "", "", "0.63", "1.05", "", "0.96", "0.56", "0.64", "",
      "0.59", "1.07", "1.95", "95% CI", "", "(0.57, 1.07)", "", "",
      "(0.42, 0.95)", "(0.63, 1.72)", "", "(0.63, 1.46)", "(0.3, 1.08)",
      "(0.31, 1.3)", "", "(0.4, 0.86)", "(0.71, 1.61)", "(0.89, 4.25)",
      "p-value", "", "0.1301", "", "0.0265", "", "", "0.8604", "", "",
      "", "0.0048", "", "", "", "Interaction p-value", "", "", "", "0.1243",
      "", "", "0.3249", "", "", "", "0.0108", "", "", ""
    ),
    .Dim = c(15L, 6L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("4. Cox Regression (setting strata, ties, and alpha level)", {
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
  df <- tidy(mod4)
  result <- split_rows_by(lyt = NULL, "effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = conf_level, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  30", "  40", "  50", "n", "", "247", "",
      "247", "", "", "247", "", "", "", "247", "", "", "", "Hazard Ratio", "",
      "0.78", "", "", "0.63", "1.05", "", "0.96", "0.56", "0.64", "",
      "0.59", "1.07", "1.95", "90% CI", "", "(0.6, 1.02)", "", "",
      "(0.42, 0.95)", "(0.63, 1.72)", "", "(0.63, 1.46)", "(0.3, 1.08)",
      "(0.31, 1.3)", "", "(0.43, 0.81)", "(0.76, 1.51)", "(1.01, 3.75)",
      "p-value", "", "0.1301", "", "0.0265", "", "", "0.8604", "", "",
      "", "0.0048", "", "", "", "Interaction p-value", "", "", "", "0.1243",
      "", "", "0.3249", "", "", "", "0.0108", "", "", ""
    ),
    .Dim = c(15L, 6L)
  )

  expect_identical(result_matrix, expected_matrix)
})
