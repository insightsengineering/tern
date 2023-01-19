# nolint start

adtte <- adtte_raw
saved_labels <- formatters::var_labels(adtte)

adtte_f <- subset(adtte, PARAMCD == "OS") # _f: filtered
adtte_f <- within(
  data = subset(
    adtte_f,
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
formatters::var_labels(adtte_f) <- saved_labels
adtte_f$event <- 1 - adtte_f$CNSR
# nolint end

testthat::test_that("1. Cox Regression", {
  mod1 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = adtte_f
  )
  df <- broom::tidy(mod1)

  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("2. Cox Regression (with Interaction Term)", {
  mod2 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = adtte_f,
    control = control_coxreg(interaction = TRUE)
  )
  df <- broom::tidy(mod2)

  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})


testthat::test_that("3. Cox Regression (specifying covariates)", {
  mod3 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = adtte_f,
    control = control_coxreg(interaction = TRUE),
    at = list(AGE = c(30, 40, 50))
  )
  df <- broom::tidy(mod3)

  result <- basic_table() %>%
    split_rows_by("effect") %>%
    split_rows_by("term", child_labels = "hidden") %>%
    summarize_coxreg(conf_level = .95, vars = c("n", "hr", "ci", "pval", "pval_inter")) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})


testthat::test_that("4. Cox Regression (setting strata, ties, and alpha level)", {
  conf_level <- 0.90
  mod4 <- fit_coxreg_univar(
    variables = list(
      time = "AVAL", event = "event", arm = "ARMCD",
      covariates = c("SEX", "RACE", "AGE")
    ),
    data = adtte_f,
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
