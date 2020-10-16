
# Preparation of the test case.

# nolint start
library(tern)
library(random.cdisc.data)
ADTTE <- radtte(cached = TRUE)
saved_labels <- var_labels(ADTTE)

ADTTE_f <- subset(ADTTE, PARAMCD == "OS")   # _f: filtered
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
  df <- h_coxreg(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f
  )
  result <- split_rows_by(lyt = NULL, "effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    fit_coxreg(conf_level = .95) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "Race", "Age", "n", "", "247", "", "247", "247", "247",
      "HR", "", "0.97", "", "0.97", "0.97", "0.92", "95% CI", "", "(0.71, 1.32)",
      "", "(0.71, 1.31)", "(0.71, 1.33)", "(0.68, 1.26)", "pval", "",
      "0.8243", "", "0.8224", "0.8701", "0.6084"
    ),
    .Dim = c(7L, 5L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("2. Cox Regression (with Interaction Term)", {
  df <- h_coxreg(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(interaction = TRUE)
  )
  result <- expect_warning(
    split_rows_by(lyt = NULL, "effect") %>%
      split_rows_by("term", child_labels = "hidden") %>%
      fit_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
      build_table(df = df)
  )
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  34", "n", "", "247", "", "247", "", "",
      "247", "", "", "", "247", "", "HR", "", "0.97", "", "", "0.87",
      "1.15", "", "1.03", "0.78", "1.06", "", "0.92", "95% CI", "",
      "(0.71, 1.32)", "", "", "(0.58, 1.28)", "(0.7, 1.91)", "", "(0.68, 1.57)",
      "(0.41, 1.49)", "(0.55, 2.04)", "", "(0.68, 1.26)", "pval", "",
      "0.8243", "", "0.4731", "", "", "0.8721", "", "", "", "0.7137",
      "", "pval_inter", "", "", "", "0.3767", "", "", "0.7441", "",
      "", "", "0.7832", ""
    ),
    .Dim = c(13L, 6L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("3. Cox Regression (specifying covariates)", {
  df <- h_coxreg(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = ADTTE_f,
    control = control_coxreg(interaction = TRUE),
    at = list(AGE = c(30, 40, 50))
  )
  result <- expect_warning(
    split_rows_by(lyt = NULL, "effect") %>%
      split_rows_by("term", child_labels = "hidden") %>%
      fit_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
      build_table(df = df)
  )
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  30", "  40", "  50", "n", "", "247", "",
      "247", "", "", "247", "", "", "", "247", "", "", "", "HR", "",
      "0.97", "", "", "0.87", "1.15", "", "1.03", "0.78", "1.06", "",
      "0.9", "0.96", "1.03", "95% CI", "", "(0.71, 1.32)", "", "",
      "(0.58, 1.28)", "(0.7, 1.91)", "", "(0.68, 1.57)", "(0.41, 1.49)",
      "(0.55, 2.04)", "", "(0.63, 1.28)", "(0.62, 1.49)", "(0.44, 2.41)",
      "pval", "", "0.8243", "", "0.4731", "", "", "0.8721", "", "",
      "", "0.7137", "", "", "", "pval_inter", "", "", "", "0.3767",
      "", "", "0.7441", "", "", "", "0.7832", "", "", ""
    ),
    .Dim = c(15L, 6L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("4. Cox Regression (setting strata, ties, and alpha level)", {
  conf_level <- 0.90
  df <- h_coxreg(
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
  result <- expect_warning(
    split_rows_by(lyt = NULL, "effect") %>%
      split_rows_by("term", child_labels = "hidden") %>%
      fit_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
      build_table(df = df)
  )
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Treatment:", "ARM A vs control (ARM B)", "Covariate:",
      "Sex", "  F", "  M", "Race", "  ASIAN", "  BLACK OR AFRICAN AMERICAN",
      "  WHITE", "Age", "  30", "  40", "  50", "n", "", "247", "",
      "247", "", "", "247", "", "", "", "247", "", "", "", "HR", "",
      "0.97", "", "", "0.87", "1.15", "", "1.03", "0.78", "1.06", "",
      "0.9", "0.96", "1.03", "95% CI", "", "(0.74, 1.25)", "", "",
      "(0.58, 1.28)", "(0.7, 1.91)", "", "(0.68, 1.57)", "(0.41, 1.49)",
      "(0.55, 2.04)", "", "(0.67, 1.21)", "(0.67, 1.39)", "(0.5, 2.1)",
      "pval", "", "0.8243", "", "0.4731", "", "", "0.8721", "", "",
      "", "0.7137", "", "", "", "pval_inter", "", "", "", "0.3767",
      "", "", "0.7441", "", "", "", "0.7832", "", "", ""
    ),
    .Dim = c(15L, 6L)
  )
})
