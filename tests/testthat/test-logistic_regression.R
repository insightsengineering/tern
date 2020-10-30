library(random.cdisc.data)
library(rtables)
library(dplyr)

get_adrs <- function(){
  adsl <- radsl(cached = TRUE)
  adsl <- adsl %>% dplyr::filter(SEX %in% c("F", "M")) #nolint
  adrs <- radrs(adsl, seed = 2)
  adrs_f <- adrs %>%
    dplyr::filter(
      PARAMCD == "BESRSPI",
      RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")
    ) %>%
    dplyr::mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0))
  adrs_f
}

test_that("fit_logistic works with default paramters", {
  data <- get_adrs()
  result_model <- fit_logistic(data, variables = list(response = "Response", arm = "ARMCD"))
  expected_model <- glm(formula = Response ~ ARMCD, data = data, family = "binomial")

  result1 <- summary(result_model)$coefficients
  expected1 <- summary(expected_model)$coefficients
  expect_identical(result1, expected1)

  result2 <- car::Anova(result_model, type = 3, test.statistic = "Wald")
  expected2 <- car::Anova(expected_model, type = 3, test.statistic = "Wald")
  expect_identical(result2, expected2)

  result3 <- vcov(result_model)
  expected3 <- vcov(expected_model)
  expect_identical(result3, expected3)

})

test_that("fit_logistic works with covariates and interaction", {
  data <- get_adrs()
  result_model <- fit_logistic(
    data,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  expected_model <- glm(
    formula = Response ~ ARMCD + AGE + RACE + ARMCD:RACE,
    data = data, family = "binomial"
  )
  result1 <- summary(result_model)$coefficients
  expected1 <- summary(expected_model)$coefficients
  expect_identical(result1, expected1)

  result2 <- car::Anova(result_model, type = 3, test.statistic = "Wald")
  expected2 <- car::Anova(expected_model, type = 3, test.statistic = "Wald")
  expect_identical(result2, expected2)

  result3 <- vcov(result_model)
  expected3 <- vcov(expected_model)
  expect_identical(result3, expected3)
})

test_that("glm_simple_term_extract works", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD", covariates = c("AGE", "RACE")
    )
  )
  result1 <- glm_simple_term_extract("RACE", mod1)
  expected1 <- data.frame(
    variable = rep("RACE", 3),
    term = c("RACE Reference = ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"),
    stringsAsFactors = FALSE
  )
  expected1$estimate <- list(numeric(0), -0.33522939569033, 0.296292628380204)
  expected1$std_error <- list(numeric(0), 0.65673817912887, 0.828929059143728)
  expected1$df <- list(2, 1, 1)
  expected1$pvalue <- list(0.7633307, 0.6097390, 0.7207622)
  expect_equal(result1, expected1, tolerance = 0.000001)

  result2 <- glm_simple_term_extract("AGE", mod1)

  expected2 <- data.frame(
    variable = "AGE",
    term = "AGE",
    stringsAsFactors = FALSE
  )
  expected2$estimate <- list(-0.0154436872166314)
  expected2$std_error <- list(0.0370085242079029)
  expected2$df <- list(1)
  expected2$pvalue <- list(0.6764584)
  expect_equal(result2, expected2, tolerance = 0.000001)

})

test_that("glm_interaction_extract works", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  result <- glm_interaction_extract("ARMCD:RACE", mod1)
  expected <- data.frame(
    variable = rep("ARMCD:RACE", 5),
    term = c(
      "ARMCD * RACE", "ARM B * BLACK OR AFRICAN AMERICAN",
      "ARM C * BLACK OR AFRICAN AMERICAN", "ARM B * WHITE", "ARM C * WHITE"
    ),
    stringsAsFactors = FALSE
  )
  expected$estimate <- list(numeric(0), 1.722828, 1.469858, -16.3095, -16.49343)
  expected$std_error <- list(numeric(0), 1.513541, 3755.583, 3409.299, 5556.542)
  expected$df <- list(4, 1, 1, 1, 1)
  expected$pvalue <- list(0.8621049, 0.2550051, 0.9996877, 0.9961831, 0.9976317)
  expect_equal(result, expected, tolerance = 0.000001)
})


test_that("h_logistic_simple_terms works", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE", "SEX"), interaction = "RACE"
    )
  )
  result1 <- h_logistic_simple_terms(c("AGE", "SEX"), mod1)
  expected1 <- data.frame(
    variable = c("AGE", "SEX", "SEX"),
    term = c("AGE", "SEX Reference = F", "M"),
    stringsAsFactors = FALSE
  )
  expected1$estimate <- list(-0.001764094, numeric(0), -1.34854854729683)
  expected1$std_error <- list(0.03810346, numeric(0), 0.632485325778419)
  expected1$df <- list(1, numeric(0), 1)
  expected1$pvalue <- list(0.9630732, numeric(0), 0.0329951722214885)
  expected1$odds_ratio <- list(0.9982375, numeric(0), 0.2596168)
  expected1$lcl <- list(0.9264034, numeric(0), 0.07515542)
  expected1$ucl <- list(1.075642, numeric(0), 0.89682)
  expected1$ci <- list(c(0.9264034, 1.0756416), numeric(0), c(0.07515542, 0.89682002))
  expect_equal(result1, expected1, tolerance = 0.000001)

  result2 <- h_logistic_simple_terms("SEX", mod1, conf_level = 0.99)
  expected2 <- data.frame(
    variable = c("SEX", "SEX"),
    term = c("SEX Reference = F", "M"),
    stringsAsFactors = FALSE
  )
  expected2$estimate <- list(numeric(0), -1.34854854729683)
  expected2$std_error <- list(numeric(0), 0.632485325778419)
  expected2$df <- list(numeric(0), 1)
  expected2$pvalue <- list(numeric(0), 0.0329951722214885)
  expected2$odds_ratio <- list(numeric(0), 0.259616808827808)
  expected2$lcl <- list(numeric(0),  0.0509086319427838)
  expected2$ucl <- list(numeric(0), 1.32395793903255)
  expected2$ci <- list(numeric(0), c(0.0509086319427838, 1.32395793903255))
  expect_equal(result2, expected2, tolerance = 0.000001)
})


test_that("tidy.glm works as expected for simple case", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "SEX")
    )
  )
  result <- broom::tidy(mod1, conf_level = 0.99)
  expected <- data.frame(
    variable = c(rep("ARMCD", 3), "AGE", rep("SEX", 2)),
    term = c("ARMCD Reference = ARM A", "ARM B", "ARM C", "AGE", "SEX Reference = F", "M"),
    stringsAsFactors = FALSE
  )
  expected$estimate <- list(numeric(0), -1.344506, 16.91221, -0.0001172816, numeric(0), -1.352468)
  expected$std_error <- list(numeric(0), 0.680919, 1539.07, 0.03697361, numeric(0), 0.6311038)
  expected$df <- list(2, 1, 1, 1, numeric(0), 1)
  expected$pvalue <- list(0.1423476, 0.0483197, 0.9912325, 0.9974691, numeric(0), 0.03211152)
  expected$odds_ratio <- list(numeric(0), 0.2606685, 22124846, 0.9998827, numeric(0), 0.2586013)
  expected$lcl <- list(numeric(0), 0.04511967, 0, 0.9090502, numeric(0), 0.05089026)
  expected$ucl <- list(numeric(0), 1.505952, Inf, 1.099791, numeric(0), 1.314095)
  expected$ci <- list(
    numeric(0), c(0.04511967, 1.50595200), c(0, Inf),
    c(0.9090502, 1.0997913), numeric(0), c(0.05089026, 1.31409454)
  )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_logistic works with simple case", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "SEX")
    )
  )
  df <- broom::tidy(mod1, conf_level = 0.99)
  result1 <- s_logistic(df, .var = "estimate")
  expected1 <- list(
    estimate = setNames(list(numeric(0)), "ARMCD Reference = ARM A"),
    estimate = setNames(list(-1.344506), "ARM B"),
    estimate = setNames(list(16.91221), "ARM C"),
    estimate = setNames(list(-0.0001172816), "AGE"),
    estimate = setNames(list(numeric(0)), "SEX Reference = F"),
    estimate = setNames(list(-1.352468), "M")
  )
  expect_equal(result1, expected1, tolerance = 0.000001)

  result2 <- s_logistic(df, .var = "ci")
  expected2 <- list(
    ci = setNames(list(numeric(0)), "ARMCD Reference = ARM A"),
    ci = setNames(list(c(0.04511967, 1.50595200)), "ARM B"),
    ci = setNames(list(c(0, Inf)), "ARM C"),
    ci = setNames(list(c(0.9090502, 1.0997913)), "AGE"),
    ci = setNames(list(numeric(0)), "SEX Reference = F"),
    ci = setNames(list(c(0.05089026, 1.31409454)), "M")
  )
  expect_equal(result2, expected2, tolerance = 0.000001)
})

test_that("summarize_logistic adds logistic regression layer to rtables", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(response = "Response", arm = "ARMCD", covariates = "AGE")
  )
  df <- broom::tidy(mod1, conf_level = 0.99)
  result <- basic_table() %>%
    split_rows_by("variable") %>%
    split_rows_by("term", split_fun = drop_split_levels) %>%
    summarize_logistic(conf_level = .99) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "ARMCD", "", "ARMCD Reference = ARM A", "", "ARM B", "", "ARM C", "AGE", "", "AGE",
      "Degrees of Freedom", "", "", "2", "", "1", "", "1", "", "", "1",
      "Parameter Estimate", "", "", "", "", "-1.276", "", "16.883", "", "", "-0.016",
      "Standard Error", "", "", "", "", "0.676", "", "1577.857", "", "", "0.036",
      "Odds Ratio", "", "", "", "", "0.28", "", ">999.99", "", "", "0.98",
      "Wald 99% CI", "", "", "", "", "(0.05, 1.59)", "", "(0, Inf)", "", "", "(0.9, 1.08)",
      "p-value", "", "", "0.1681", "", "0.0590", "", "0.9915", "", "", "0.6602"
    ),
    .Dim = c(11L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})
