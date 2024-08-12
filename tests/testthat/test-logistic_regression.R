# Local data pre-processing
adrs_local <- tern_ex_adrs %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(tern_ex_adrs))

adrs_example <- adrs_local %>%
  dplyr::filter(
    PARAMCD == "BESRSPI",
    RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")
  ) %>%
  dplyr::mutate(Response = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0)) %>%
  reapply_varlabels(formatters::var_labels(adrs_local))

# fit_logistic ----

testthat::test_that("fit_logistic works with default paramters", {
  data <- adrs_example
  result_model <- fit_logistic(data, variables = list(response = "Response", arm = "ARMCD"))
  expected_model <- stats::glm(formula = Response ~ ARMCD, data = data, family = "binomial")

  result1 <- summary(result_model)$coefficients
  expected1 <- summary(expected_model)$coefficients
  testthat::expect_identical(result1, expected1)

  result2 <- car::Anova(result_model, type = 3, test.statistic = "Wald")
  expected2 <- car::Anova(expected_model, type = 3, test.statistic = "Wald")
  testthat::expect_identical(result2, expected2)

  result3 <- stats::vcov(result_model)
  expected3 <- stats::vcov(expected_model)
  testthat::expect_identical(result3, expected3)
})

testthat::test_that("fit_logistic works with covariates and interaction", {
  data <- adrs_example
  result_model <- fit_logistic(
    data,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  expected_model <- stats::glm(
    formula = Response ~ ARMCD + AGE + RACE + ARMCD:RACE,
    data = data, family = "binomial"
  )
  result1 <- summary(result_model)$coefficients
  expected1 <- summary(expected_model)$coefficients
  testthat::expect_identical(result1, expected1)

  result2 <- car::Anova(result_model, type = 3, test.statistic = "Wald")
  expected2 <- car::Anova(expected_model, type = 3, test.statistic = "Wald")
  testthat::expect_identical(result2, expected2)

  result3 <- stats::vcov(result_model)
  expected3 <- stats::vcov(expected_model)
  testthat::expect_identical(result3, expected3)
})

testthat::test_that("fit_logistic works with different response definition", {
  data <- adrs_example
  result_model <- fit_logistic(
    data,
    variables = list(response = "Response", arm = "ARMCD"),
    response_definition = "1 - response"
  )
  testthat::expect_equal(
    result_model$formula,
    1 - Response ~ ARMCD,
    ignore_attr = TRUE
  )
})

testthat::test_that("fit_logistic works with a single stratification variable", {
  data <- data.frame(
    Response = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    ARMCD = letters[c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 2, 2)],
    STRATA1 = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
    STRATA2 = LETTERS[c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)],
    AGE = c(45, 67, 23, 17, 87, 66, 45, 34, 32, 34, 67, 65, 64, 66, 24, 35, 46, 78, 10, 45),
    SEX = rep(c("M", "F"), 10),
    RACE = LETTERS[c(rep(c(3, 4), 10))]
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- testthat::expect_silent(fit_logistic(
      data,
      variables = list(
        response = "Response",
        arm = "ARMCD",
        covariates = c("RACE", "AGE"),
        strata = "STRATA1"
      )
    ))
  )
  testthat::expect_s3_class(result, c("clogit", "coxph"))

  expected_formula <- Surv(rep(1, 20L), Response) ~ ARMCD + RACE + AGE + strata(STRATA1)
  result_formula <- result$formula
  testthat::expect_equal(result_formula, expected_formula, ignore_attr = TRUE)
})

testthat::test_that("fit_logistic works with two stratification variables", {
  data <- data.frame(
    Response = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    ARMCD = letters[c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 2, 2)],
    STRATA1 = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
    STRATA2 = LETTERS[c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)],
    AGE = c(45, 67, 23, 17, 87, 66, 45, 34, 32, 34, 67, 65, 64, 66, 24, 35, 46, 78, 10, 45),
    SEX = rep(c("M", "F"), 10),
    RACE = LETTERS[c(rep(c(3, 4), 10))]
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- testthat::expect_silent(fit_logistic(
      data,
      variables = list(
        response = "Response",
        arm = "ARMCD",
        covariates = c("RACE", "AGE"),
        strata = c("STRATA1", "STRATA2")
      )
    ))
  )
  testthat::expect_s3_class(result, c("clogit", "coxph"))

  expected_formula <- Surv(rep(1, 20L), Response) ~ ARMCD + RACE + AGE +
    strata(I(interaction(STRATA1, STRATA2)))
  result_formula <- result$formula
  testthat::expect_equal(result_formula, expected_formula, ignore_attr = TRUE)
})

# tidy.glm ----

testthat::test_that("tidy.glm works as expected for simple case", {
  skip_if_too_deep(3)

  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "SEX")
    )
  )
  result <- broom::tidy(mod1, conf_level = 0.99)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tidy.glm works as expected for interaction case", {
  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "SEX"),
      interaction = "AGE"
    )
  )
  result <- broom::tidy(mod1, conf_level = 0.99)
  result <- result[, c("variable", "term", "interaction", "reference", "estimate", "std_error")]
  row.names(result) <- seq(dim(result)[1])

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
  testthat::expect_s3_class(result, "data.frame")
})

# logistic_regression_cols ----

testthat::test_that("logistic_regression_cols works as expected", {
  result <- basic_table() %>%
    logistic_regression_cols(conf_level = 0.75) %>%
    analyze_colvars(afun = list(
      function(df) "df",
      function(df) "estimate",
      function(df) "se",
      function(df) "or",
      function(df) "ci",
      function(df) "p"
    )) %>%
    build_table(data.frame(
      df = NA,
      estimate = NA,
      std_error = NA,
      odds_ratio = NA,
      ci = NA,
      pvalue = NA
    ))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# logistic_summary_by_flag ----

testthat::test_that("logistic_summary_by_flag works", {
  layout_fun <- logistic_summary_by_flag("is_variable")
  testthat::expect_type(layout_fun, "closure")
  testthat::expect_silent(layout_fun(basic_table()))
})

# summarize_logistic ----

testthat::test_that("summarize_logistic works as expected for interaction model with continuous variable", {
  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "SEX"),
      interaction = "AGE"
    )
  )
  df <- broom::tidy(mod1, conf_level = 0.99) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = 0.99,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_logistic works as expected for interaction model with categorical variable", {
  adrs <- adrs_example
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "SEX"),
      interaction = "SEX"
    )
  )
  df <- broom::tidy(model, conf_level = 0.99) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = 0.99,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_logistic works as expected for simple model without interactions", {
  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(response = "Response", arm = "ARMCD", covariates = "AGE")
  )
  df <- broom::tidy(mod1, conf_level = 0.99) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = 0.99,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
