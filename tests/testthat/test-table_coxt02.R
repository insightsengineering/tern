# Tests the variants for COXT02

library(tern)
library(random.cdisc.data)
library(dplyr)
library(broom)
adtte   <- radtte(cached = TRUE)
adtte_f <- subset(adtte, PARAMCD == "OS")   # _f: filtered
adtte_f <- within( # nolint
  data = subset(
    adtte_f,
    PARAMCD == "OS"
    & SEX %in% c("F", "M")
    & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ),
  expr = { # nolint start
    set.seed(1)
    ARMCD <- relevel(ARMCD, "ARM B")
    SEX <- droplevels(SEX)
    RACE <- droplevels(RACE)
    X <- rnorm(n = length(ARM))
  }  # nolint end
) %>%
  mutate(event = 1 - CNSR) %>%
  rtables::var_relabel(
    SEX = "Sex",
    AGE = "Age"
  )

test_that("COXT02 default variant 1 is produced correctly", {
  multivar_model <- fit_coxreg_multivar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "AGE")
    ),
    data = adtte_f
  )
  df <- broom::tidy(multivar_model)
  result <- basic_table() %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(multivar = TRUE, conf_level = .95) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "ARMCD (reference = ARM B)", "ARM A", "ARM C",
      "Sex (reference = F)", "M", "Age", "Hazard Ratio", "", "0.75",
      "1.74", "", "0.84", "0.99", "95% CI", "", "(0.55, 1.04)", "(1.3, 2.34)",
      "", "(0.65, 1.07)", "(0.98, 1.01)", "p-value", "<0.0001", "0.0852",
      "0.0002", "", "0.1605", "0.3103"
    ),
    .Dim = c(7L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("COXT02 variant 5 is produced correctly", {
  multivar_model <- fit_coxreg_multivar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "AGE"), strata = "RACE"
    ),
    control = control_coxreg(
      conf_level = 0.9,
      ties = "efron"
    ),
    data = adtte_f
  )
  df <- broom::tidy(multivar_model)
  result <- basic_table() %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(multivar = TRUE, conf_level = 0.9, vars = c("hr", "ci")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "ARMCD (reference = ARM B)", "ARM A", "ARM C",
      "Sex (reference = F)", "M", "Age", "Hazard Ratio", "", "0.72",
      "1.78", "", "0.82", "0.99", "90% CI", "", "(0.54, 0.95)", "(1.38, 2.28)",
      "", "(0.67, 1.02)", "(0.98, 1)"
    ),
    .Dim = c(7L, 3L)
  )

  expect_identical(result_matrix, expected_matrix)
})
