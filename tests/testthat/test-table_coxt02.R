# Tests the variants for COXT02

adtte <- adtte_raw
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
})
