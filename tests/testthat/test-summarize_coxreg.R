# Bladder data from survival
dta_bladder <- local({
  # Setting general random for data generation
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- with(
    data = survival::bladder[survival::bladder$enum < 5, ],
    tibble(
      time = stop,
      status = event,
      arm = paste("ARM:", as.factor(rx)),
      armcd = formatters::with_label(as.factor(rx), "ARM"),
      covar1 = formatters::with_label(as.factor(enum), "A Covariate Label"),
      covar2 = formatters::with_label(
        factor(sample(as.factor(enum)), levels = 1:4, labels = c("F", "F", "M", "M")), "Sex (F/M)"
      ),
      age = sample(20:60, size = 340, replace = TRUE),
      STUDYID = factor("X")
    )
  )
  dta_bladder
})

# s_coxreg ----

testthat::test_that("s_coxreg converts tabulated results in a list", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = dta_bladder_raw
  ) %>% broom::tidy(univar_model)
  result <- s_coxreg(df = df, .stat = "hr")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_coxreg works with which_vars and var_nms arguments", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    control = control_coxreg(interaction = TRUE),
    data = dta_bladder_raw
  ) %>% broom::tidy(univar_model)
  result <- s_coxreg(df = df, .stat = "hr", which_vars = "inter", var_nms = "covar2")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# a_coxreg ----

testthat::test_that("a_coxreg works as expected", {
  result <- a_coxreg(
    df = dta_bladder,
    labelstr = "Label 1",
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    .spl_context = list(value = "covar1"),
    .stats = "n",
    .formats = "xx"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# summarize_coxreg ----

testthat::test_that("summarize_coxreg adds the univariable Cox regression layer to rtables", {
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  )
  control <- control_coxreg(ties = "breslow", conf_level = 0.90)

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      control = control
    ) %>%
    build_table(df = dta_bladder)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # custom covariate labels
  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      varlabels = c("First Covariate", "Second Covariate")
    ) %>%
    build_table(df = dta_bladder)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_coxreg works with interactions in univariable case", {
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  )
  control <- control_coxreg(interaction = TRUE)

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      control = control
    ) %>%
    build_table(df = dta_bladder)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_coxreg works without treatment arm in univariable case", {
  variables <- list(
    time = "time", event = "status",
    covariates = c("covar1", "covar2")
  )
  control <- control_coxreg(conf_level = 0.90)

  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      control = control
    ) %>%
    build_table(df = dta_bladder)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_coxreg adds the multivariable Cox regression layer to rtables", {
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  )

  result <- basic_table() %>%
    summarize_coxreg(variables = variables, multivar = TRUE) %>%
    build_table(df = dta_bladder)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # custom covariate labels
  result <- basic_table() %>%
    summarize_coxreg(
      variables = variables,
      multivar = TRUE,
      varlabels = c("First Covariate", "Second Covariate")
    ) %>%
    build_table(df = dta_bladder)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
