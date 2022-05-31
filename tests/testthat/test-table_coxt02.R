# Tests the variants for COXT02

library(tern)
library(scda)
library(dplyr)
library(broom)

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte
adtte_f <- subset(adtte, PARAMCD == "OS") # _f: filtered
adtte_f <- within( # nolint
  data = subset(
    adtte_f,
    PARAMCD == "OS" &
      SEX %in% c("F", "M") &
      RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
  ),
  expr = { # nolint start
    set.seed(1)
    ARMCD <- stats::relevel(ARMCD, "ARM B")
    SEX <- droplevels(SEX)
    RACE <- droplevels(RACE)
    X <- stats::rnorm(n = length(ARM))
  } # nolint end
) %>%
  dplyr::mutate(event = 1 - CNSR)

columns <- c("SEX", "AGE")
labels <- c("Sex", "Age")
for (i in seq_along(columns)) {
  attr(adtte_f[[columns[i]]], "label") <- labels[i]
}

testthat::test_that("COXT02 default variant 1 is produced correctly", {
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
      "Sex (reference = F)", "M", "Age", "Hazard Ratio", "", "0.72",
      "1.93", "", "1.17", "1.01", "95% CI", "", "(0.52, 0.99)", "(1.44, 2.59)",
      "", "(0.91, 1.51)", "(0.99, 1.02)", "p-value", "<0.0001", "0.0417",
      "<0.0001", "", "0.2142", "0.4785"
    ),
    .Dim = c(7L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("COXT02 variant 5 is produced correctly", {
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
      "1.96", "", "1.21", "1.01", "90% CI", "", "(0.55, 0.95)", "(1.52, 2.51)",
      "", "(0.98, 1.49)", "(0.99, 1.02)"
    ),
    .Dim = c(7L, 3L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
