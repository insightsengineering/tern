library(scda)
library(rtables)
library(dplyr)

adsl_cached <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adsl_cached <- adsl_cached %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(adsl_cached))

adrs_cached <- synthetic_cdisc_data("rcd_2022_02_28")$adrs
adrs_cached <- adrs_cached %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(adrs_cached))

adrs_example <- local({
  adrs_cached %>%
    dplyr::filter(
      PARAMCD == "BESRSPI",
      RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")
    ) %>%
    dplyr::mutate(Response = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0)) %>%
    reapply_varlabels(formatters::var_labels(adrs_cached))
})

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
    1 - Response ~ ARMCD
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

  result <- testthat::expect_silent(fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("RACE", "AGE"),
      strata = "STRATA1"
    )
  ))
  testthat::expect_s3_class(result, c("clogit", "coxph"))

  expected_formula <- Surv(rep(1, 20L), Response) ~ ARMCD + RACE + AGE + strata(STRATA1)
  result_formula <- result$formula
  testthat::expect_equal(result_formula, expected_formula)
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

  result <- testthat::expect_silent(fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("RACE", "AGE"),
      strata = c("STRATA1", "STRATA2")
    )
  ))
  testthat::expect_s3_class(result, c("clogit", "coxph"))

  expected_formula <- Surv(rep(1, 20L), Response) ~ ARMCD + RACE + AGE +
    strata(I(interaction(STRATA1, STRATA2)))
  result_formula <- result$formula
  testthat::expect_equal(result_formula, expected_formula)
})


# h_get_interaction_vars ----

testthat::test_that("h_get_interaction_vars works as expected", {
  data <- adrs_example
  model <- fit_logistic(
    data,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  result <- h_get_interaction_vars(model)
  expected <- c("ARMCD", "RACE")
  testthat::expect_setequal(result, expected)
})

# h_interaction_coef_name ----

testthat::test_that("h_interaction_coef_name works as expected", {
  data <- adrs_example
  model <- fit_logistic(
    data,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  result <- h_interaction_coef_name(
    interaction_vars = c("ARMCD", "RACE"),
    first_var_with_level = c("RACE", "WHITE"),
    second_var_with_level = c("ARMCD", "ARM B")
  )
  expected <- "ARMCDARM B:RACEWHITE"
  testthat::expect_identical(result, expected)
})

# h_or_cat_interaction ----

testthat::test_that("h_or_cat_interaction works as expected", {
  data <- adrs_example
  model <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "RACE"
    )
  )
  result_armcd <- h_or_cat_interaction("ARMCD", "RACE", model)
  testthat::expect_is(result_armcd, "list")
  testthat::expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    testthat::expect_is(res, "list")
    testthat::expect_named(res, levels(droplevels(data$RACE)))
  }
  testthat::expect_equivalent(
    result_armcd[["ARM B"]][["ASIAN"]],
    list(
      or = 0.1503174,
      ci = c(0.01669928, 1.35307118)
    ),
    tol = 1e-4
  )
  result_race <- h_or_cat_interaction("RACE", "ARMCD", model)
  testthat::expect_is(result_race, "list")
  testthat::expect_named(result_race, levels(droplevels(data$RACE))[-1])
  for (res in result_race) {
    testthat::expect_is(res, "list")
    testthat::expect_named(res, levels(data$ARMCD))
  }
})

# h_or_cont_interaction ----

testthat::test_that("h_or_cont_interaction works as expected with median increment", {
  data <- adrs_example
  model <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  )
  result_armcd <- h_or_cont_interaction("ARMCD", "AGE", model)
  testthat::expect_is(result_armcd, "list")
  testthat::expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    testthat::expect_is(res, "list")
    testthat::expect_named(res, as.character(stats::median(data$AGE)))
  }
  testthat::expect_equivalent(
    result_armcd[["ARM B"]][["34"]],
    list(
      or = 9.284028e-05,
      ci = c(1.483171e-10, 5.811413e+01)
    ),
    tol = 1e-4
  )
  result_age <- h_or_cont_interaction("AGE", "ARMCD", model)
  testthat::expect_is(result_age, "list")
  testthat::expect_named(result_age, levels(data$ARMCD))
  for (res in result_age) {
    testthat::expect_is(res, "list")
    testthat::expect_named(res, c("or", "ci"))
  }
  testthat::expect_equivalent(
    result_age[["ARM B"]],
    list(
      or = 1.028141,
      ci = c(0.9286094, 1.1383411)
    ),
    tol = 1e-4
  )
})

testthat::test_that("h_or_cont_interaction works as expected with custom increments", {
  data <- adrs_example
  model <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  )
  result_armcd <- h_or_cont_interaction(
    "ARMCD",
    "AGE",
    model,
    at = c(25, 30, 40),
    conf_level = 0.8
  )
  testthat::expect_is(result_armcd, "list")
  testthat::expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    testthat::expect_is(res, "list")
    testthat::expect_named(res, as.character(c(25, 30, 40)))
  }
  # We are not allowed to specify increments when the interaction variable is a factor.
  testthat::expect_error(h_or_cont_interaction(
    "AGE",
    "ARMCD",
    model,
    at = c(25, 30, 40)
  ))
})

# h_or_interaction ----

testthat::test_that("h_or_interaction works as expected", {
  data <- adrs_example
  model_cont <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  )
  result_cont <- h_or_interaction(
    "ARMCD",
    "AGE",
    model_cont,
    at = c(20, 30),
    conf_level = 0.9
  )
  testthat::expect_is(result_cont, "list")
  model_cat <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "RACE"
    )
  )
  result_cat <- h_or_interaction(
    "ARMCD",
    "RACE",
    model_cat
  )
  testthat::expect_is(result_cat, "list")
})

# h_simple_term_labels ----

testthat::test_that("h_simple_term_labels works correctly with factor input", {
  x <- c("A", "B", "C", "B", "A")
  terms <- factor(c("B", "C"))
  table <- table(x)
  result <- h_simple_term_labels(terms, table)
  expected <- c("B, n = 2", "C, n = 1")
  testthat::expect_identical(result, expected)
})

# h_interaction_term_labels ----

testthat::test_that("h_interaction_term_labels works correctly with factor input", {
  x <- c("A", "B", "C", "B", "A")
  y <- c("1", "3", "2", "2", "1")
  terms1 <- factor(c("B", "C"))
  terms2 <- factor(c("2", "3"), levels = c("3", "2"))
  table <- table(x, y)
  result <- h_interaction_term_labels(terms1, terms2, table)
  expected <- c("B * 2, n = 1", "C * 3, n = 0")
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_interaction_term_labels works correctly when any term can be fulfilled", {
  x <- c("A", "B", "C", "B", "A")
  y <- c("1", "3", "2", "2", "1")
  terms1 <- "B"
  terms2 <- "2"
  table <- table(x, y)
  result <- h_interaction_term_labels(terms1, terms2, table, any = TRUE)
  expected <- "B or 2, n = 3"
  testthat::expect_identical(result, expected)
})

# h_glm_simple_term_extract ----

testthat::test_that("h_glm_simple_term_extract works for factor and numeric variables", {
  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD", covariates = c("AGE", "RACE")
    )
  )

  result1 <- h_glm_simple_term_extract("RACE", mod1)
  expected1 <- data.frame(
    variable = rep("RACE", 3),
    variable_label = rep("Race", 3),
    term = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"),
    term_label = c("Reference ASIAN, n = 208", "BLACK OR AFRICAN AMERICAN, n = 91", "WHITE, n = 74"),
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    is_variable_summary = c(TRUE, FALSE, FALSE),
    is_term_summary = c(FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  expected1$df <- list(2, 1, 1)
  expected1$pvalue <- list(0.636928085707871, 0.349760676147602, 0.729420300119224)
  expected1$estimate <- list(numeric(0), 1.02678193759971, 0.294048000380143)
  expected1$std_error <- list(numeric(0), 1.09809954779278, 0.850103355568239)

  testthat::expect_equal(result1, expected1[, names(result1)], tolerance = 0.000001)

  result2 <- h_glm_simple_term_extract("AGE", mod1)
  expected2 <- data.frame(
    variable = "AGE",
    variable_label = "Age",
    term = "AGE",
    term_label = "Age",
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    is_variable_summary = FALSE,
    is_term_summary = TRUE,
    stringsAsFactors = FALSE
  )
  expected2$estimate <- list(0.0674615539231469)
  expected2$std_error <- list(0.0534337742090727)
  expected2$df <- list(1)
  expected2$pvalue <- list(0.206759410411742)
  testthat::expect_equal(result2, expected2[, names(result2)], tolerance = 0.00001)
})

testthat::test_that("h_glm_simple_term_extract can extract continuous variable results from clogit objects", {
  data <- data.frame(
    Response = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    ARMCD = letters[c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 2, 2)],
    STRATA1 = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
    STRATA2 = LETTERS[c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)],
    AGE = c(45, 67, 23, 17, 87, 66, 45, 34, 32, 34, 67, 65, 64, 66, 24, 35, 46, 78, 10, 45),
    SEX = rep(c("M", "F"), 10),
    RACE = LETTERS[c(rep(c(3, 4), 10))]
  )

  mod <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      strata = "STRATA1"
    )
  )

  result <- testthat::expect_silent(h_glm_simple_term_extract("AGE", mod))
  expected <- data.frame(
    variable = "AGE",
    variable_label = "AGE",
    term = "AGE",
    term_label = "AGE",
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    estimate = list(0.01843522),
    std_error = list(0.02429352),
    df = list(1),
    pvalue = list(0.4479403),
    is_variable_summary = FALSE,
    is_term_summary = TRUE,
    stringsAsFactors = FALSE
  )
  testthat::expect_equivalent(result, expected, tolerance = 0.000001)
})

# h_glm_interaction_extract ----

testthat::test_that("h_glm_interaction_extract works for categorical interaction", {
  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  result <- h_glm_interaction_extract("ARMCD:RACE", mod1)
  expected <- data.frame(
    variable = rep("ARMCD:RACE", 5),
    variable_label = rep("Interaction of Planned Arm Code * Race", 5),
    term = c(
      "ARMCD * RACE", "ARM B * BLACK OR AFRICAN AMERICAN",
      "ARM C * BLACK OR AFRICAN AMERICAN", "ARM B * WHITE", "ARM C * WHITE"
    ),
    term_label = c(
      "Reference ARM A or ASIAN, n = 266", "ARM B * BLACK OR AFRICAN AMERICAN, n = 28",
      "ARM C * BLACK OR AFRICAN AMERICAN, n = 32", "ARM B * WHITE, n = 26",
      "ARM C * WHITE, n = 21"
    ),
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    is_variable_summary = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    is_term_summary = c(FALSE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  expected$estimate <- list(numeric(0), -15.40581, -16.09859, -15.27831, -33.46005)
  expected$std_error <- list(numeric(0), 3162.069, 4876.011, 3369.775, 3944.697)
  expected$df <- list(4, 1, 1, 1, 1)
  expected$pvalue <- list(1, 0.9961127, 0.9973657, 0.9963825, 0.9932322)
  testthat::expect_equal(result, expected[, names(result)], tolerance = 0.000001)
})

testthat::test_that("h_glm_interaction_extract works for continuous interaction", {
  adrs <- adrs_example
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  )
  result <- h_glm_interaction_extract("ARMCD:AGE", model)
  expected <- structure(
    list(
      variable = rep("ARMCD:AGE", 3),
      variable_label = rep("Interaction of Planned Arm Code * Age", 3),
      term = c("ARM A", "ARM B", "ARM C"),
      term_label = c("Reference ARM A, n = 126", "ARM B, n = 121", "ARM C, n = 126"),
      interaction = rep("", 3),
      interaction_label = rep("", 3),
      reference = rep("", 3),
      reference_label = rep("", 3),
      estimate = list(numeric(0), -0.868231277534066, -0.617569013085646),
      std_error = list(numeric(0), 0.629837882151419, 0.67144179738645),
      df = list(2, 1, 1),
      pvalue = list(0.230620398951395, 0.168049086898722, 0.357695305920641),
      is_variable_summary = c(TRUE, FALSE, FALSE),
      is_term_summary = c(FALSE, TRUE, TRUE)
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  testthat::expect_equivalent(result, expected)
})

# h_logistic_simple_terms ----

testthat::test_that("h_logistic_simple_terms works", {
  skip_if_too_deep(3)

  adrs <- adrs_example
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
    variable_label = c("Age", "Sex", "Sex"),
    term = c("AGE", "F", "M"),
    term_label = c("Age", "Reference F, n = 216", "M, n = 157"),
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    is_variable_summary = c(FALSE, TRUE, FALSE),
    is_term_summary = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  expected1$estimate <- list(0.0661119158842212, numeric(0), -0.325646036403045)
  expected1$std_error <- list(0.0541759122402904, numeric(0), 0.705657114685314)
  expected1$df <- list(1, numeric(0), 1)
  expected1$pvalue <- list(0.222343824158412, numeric(0), 0.644454886077627)
  expected1$odds_ratio <- list(1.0683462753937, numeric(0), 0.722060725415064)
  expected1$lcl <- list(0.960721301481885, numeric(0), 0.181099519627827)
  expected1$ucl <- list(1.18802795606496, numeric(0), 2.87892365622165)
  expected1$ci <- list(
    c(0.960721301481885, 1.18802795606496), numeric(0),
    c(0.181099519627827, 2.87892365622165)
  )
  testthat::expect_equal(result1, expected1[, names(result1)], tolerance = 0.000001)

  result2 <- h_logistic_simple_terms("SEX", mod1, conf_level = 0.99)
  expected2 <- data.frame(
    variable = rep("SEX", 2),
    variable_label = rep("Sex", 2),
    term = c("F", "M"),
    term_label = c("Reference F, n = 216", "M, n = 157"),
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    is_variable_summary = c(TRUE, FALSE),
    is_term_summary = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  expected2$estimate <- list(numeric(0), -0.325646036403045)
  expected2$std_error <- list(numeric(0), 0.705657114685314)
  expected2$df <- list(numeric(0), 1)
  expected2$pvalue <- list(numeric(0), 0.644454886077627)
  expected2$odds_ratio <- list(numeric(0), 0.722060725415064)
  expected2$lcl <- list(numeric(0), 0.117267420069842)
  expected2$ucl <- list(numeric(0), 4.44600632363543)
  expected2$ci <- list(numeric(0), c(0.117267420069842, 4.44600632363543))
  testthat::expect_equal(result2, expected2[, names(result2)], tolerance = 0.000001)
})

testthat::test_that("h_logistic_simple_terms can extract continuous variable results from clogit objects", {
  skip_if_too_deep(3)

  data <- data.frame(
    Response = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    ARMCD = letters[c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 2, 2)],
    STRATA1 = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
    STRATA2 = LETTERS[c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)],
    AGE = c(45, 67, 23, 17, 87, 66, 45, 34, 32, 34, 67, 65, 64, 66, 24, 35, 46, 78, 10, 45),
    SEX = rep(c("M", "F"), 10),
    RACE = LETTERS[c(rep(c(3, 4), 10))]
  )

  mod <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE", "SEX"),
      strata = "STRATA1"
    )
  )
  result <- testthat::expect_silent(h_logistic_simple_terms("AGE", mod))
  expected <- data.frame(
    variable = "AGE",
    variable_label = "AGE",
    term = "AGE",
    term_label = "AGE",
    interaction = "",
    interaction_label = "",
    reference = "",
    reference_label = "",
    estimate = list(0.01843522),
    std_error = list(0.02429352),
    df = list(1),
    pvalue = list(0.4479403),
    is_variable_summary = FALSE,
    is_term_summary = TRUE,
    odds_ratio = list(1.018606),
    lcl = list(0.9712424),
    ucl = list(1.06828),
    stringsAsFactors = FALSE
  )
  expected$ci <- list(c(
    0.9712424,
    1.0682798
  ))
  testthat::expect_equivalent(result, expected, tolerance = 0.000001)
})

# h_glm_inter_term_extract ----

testthat::test_that("h_glm_inter_term_extract works as expected with categorical interaction", {
  adrs <- adrs_example
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  result <- h_glm_inter_term_extract(
    odds_ratio_var = "ARMCD",
    interaction_var = "RACE",
    model,
    conf_level = 0.95
  )
  expected <- structure(
    list(
      variable = rep("ARMCD", 9),
      variable_label = rep("Planned Arm Code", 9),
      term = c(
        "ARM A",
        "ARM B",
        "ARM B",
        "ARM B",
        "ARM B",
        "ARM C",
        "ARM C",
        "ARM C",
        "ARM C"
      ),
      term_label = c(
        "Reference ARM A, n = 126", "ARM B, n = 121", "ARM B, n = 121",
        "ARM B, n = 121", "ARM B, n = 121", "ARM C, n = 126", "ARM C, n = 126",
        "ARM C, n = 126", "ARM C, n = 126"
      ),
      interaction = c("", "", "RACE", "RACE", "RACE", "", "RACE", "RACE", "RACE"),
      interaction_label = c("", "", "Race", "Race", "Race", "", "Race", "Race", "Race"),
      reference = c(
        "", "", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "",
        "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"
      ),
      reference_label = c(
        "", "", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "",
        "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"
      ),
      estimate = list(
        numeric(0),
        -1.89500640455506,
        NA,
        NA,
        NA,
        16.104321554579,
        NA,
        NA, NA
      ),
      std_error = list(
        numeric(0),
        1.1211345604689,
        NA,
        NA,
        NA,
        2050.67134278974,
        NA,
        NA,
        NA
      ),
      odds_ratio = c(
        NA,
        NA,
        0.150317372327429,
        3.06445027971716e-08,
        3.48115457572539e-08,
        NA,
        9863203.3243185,
        1.00574842606045,
        2.90070240841069e-08
      ),
      lcl = c(
        NA, NA, 0.0166992784864452, 0,
        0, NA, 0, 0, 0
      ),
      ucl = c(
        NA, NA, 1.35307117859994, Inf,
        Inf, NA, Inf, Inf, Inf
      ),
      df = list(
        2, 1, NA, NA, NA, 1, NA,
        NA, NA
      ),
      pvalue = list(
        0.239662687330642,
        0.0909786500513382,
        NA,
        NA,
        NA,
        0.993734121510509,
        NA,
        NA,
        NA
      ),
      is_variable_summary = c(
        TRUE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
      ),
      is_term_summary = c(
        FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
        FALSE, FALSE, FALSE
      ),
      is_reference_summary = c(
        FALSE, FALSE,
        TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE
      )
    ),
    row.names = c(
      "1",
      "2", "ARM B1", "ARM B2", "ARM B3", "3", "ARM C1", "ARM C2", "ARM C3"
    ),
    class = "data.frame"
  )
  testthat::expect_equivalent(result, expected)
})

testthat::test_that("h_glm_inter_term_extract works as expected with continuous interaction", {
  adrs <- adrs_example
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "AGE"
    )
  )
  result1 <- h_glm_inter_term_extract(
    odds_ratio_var = "ARMCD",
    interaction_var = "AGE",
    model,
    conf_level = 0.9
  )
  testthat::expect_is(result1, "data.frame")
  result2 <- h_glm_inter_term_extract(
    odds_ratio_var = "AGE",
    interaction_var = "ARMCD",
    model,
    conf_level = 0.9
  )
  testthat::expect_is(result2, "data.frame")
})

testthat::test_that("h_logistic_inter_terms works as expected", {
  adrs <- adrs_example
  model_cat <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "SEX"),
      interaction = "SEX"
    )
  )
  result_cat <- h_logistic_inter_terms(
    x = c("ARMCD", "AGE", "SEX", "ARMCD:SEX"),
    fit_glm = model_cat,
    conf_level = 0.8
  )
  testthat::expect_is(result_cat, "data.frame")
  testthat::expect_identical(
    result_cat$variable,
    c(
      "AGE", "ARMCD", "ARMCD", "ARMCD", "ARMCD", "ARMCD", "ARMCD",
      "ARMCD", "SEX", "SEX", "SEX", "SEX", "SEX", "ARMCD:SEX", "ARMCD:SEX",
      "ARMCD:SEX"
    )
  )
  testthat::expect_identical(
    result_cat$term_label,
    c(
      "Age", "Reference ARM A, n = 126", "ARM B, n = 121", "ARM B, n = 121",
      "ARM B, n = 121", "ARM C, n = 126", "ARM C, n = 126", "ARM C, n = 126",
      "Reference F, n = 216", "M, n = 157", "M, n = 157", "M, n = 157",
      "M, n = 157", "Reference ARM A or F, n = 266", "ARM B * M, n = 47",
      "ARM C * M, n = 60"
    )
  )
  model_cont <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "SEX"),
      interaction = "AGE"
    )
  )
  result_cont <- h_logistic_inter_terms(
    x = c("ARMCD", "AGE", "SEX", "ARMCD:AGE"),
    fit_glm = model_cont,
    conf_level = 0.8
  )
  testthat::expect_is(result_cont, "data.frame")
  testthat::expect_identical(
    result_cont$term,
    c(
      "F", "M", "ARM A", "ARM B",
      "ARM B", "ARM C", "ARM C", "AGE", "AGE", "AGE", "AGE", "ARM A",
      "ARM B", "ARM C"
    )
  )
  testthat::expect_identical(
    result_cont$term_label,
    c(
      "Reference F, n = 216", "M, n = 157", "Reference ARM A, n = 126",
      "ARM B, n = 121", "ARM B, n = 121", "ARM C, n = 126", "ARM C, n = 126",
      "Age", "Age", "Age", "Age", "Reference ARM A, n = 126", "ARM B, n = 121",
      "ARM C, n = 126"
    )
  )
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
  expected <- data.frame(
    variable = factor(
      c(rep("ARMCD", 3), "AGE", rep("SEX", 2)),
      levels = c("ARMCD", "AGE", "SEX")
    ),
    variable_label = c(rep("Planned Arm Code", 3), "Age", rep("Sex", 2)),
    term = factor(
      c("ARM A", "ARM B", "ARM C", "AGE", "F", "M"),
      levels = c("ARM A", "ARM B", "ARM C", "AGE", "F", "M")
    ),
    term_label = c(
      "Reference ARM A, n = 126", "ARM B, n = 121", "ARM C, n = 126",
      "Age", "Reference F, n = 216", "M, n = 157"
    ),
    interaction = factor(""),
    interaction_label = "",
    reference = factor(""),
    reference_label = "",
    is_variable_summary = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
    is_term_summary = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  expected$estimate <- list(
    numeric(0), -2.13997014929601, -0.0581408215972514, 0.0686993293696756,
    numeric(0), -0.342159921912373
  )
  expected$std_error <- list(
    numeric(0), 1.08023696449696, 1.42359779774611, 0.0533593646062628,
    numeric(0), 0.698571835198751
  )
  expected$df <- list(2, 1, 1, 1, numeric(0), 1)
  expected$pvalue <- list(
    0.0346348266698999, 0.0475891223245196, 0.967422841532196,
    0.197925635097254, numeric(0), 0.624276001036544
  )
  expected$odds_ratio <- list(
    numeric(0), 0.117658355154091, 0.943517070500145, 1.07111410817357,
    numeric(0), 0.710234613548929
  )
  expected$lcl <- list(
    numeric(0), 0.00728107926391354, 0.0241110329256203, 0.933564442101182,
    numeric(0), 0.117471241840093
  )
  expected$ucl <- list(
    numeric(0), 1.9012962276317, 36.9218716208225, 1.22893008879628,
    numeric(0), 4.29409954625027
  )
  expected$ci <- list(
    numeric(0), c(0.00728107926391354, 1.9012962276317),
    c(0.0241110329256203, 36.9218716208225), c(0.933564442101182, 1.22893008879628),
    numeric(0), c(0.117471241840093, 4.29409954625027)
  )
  testthat::expect_equal(result, expected[, names(result)], tolerance = 0.000001)
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
  testthat::expect_is(result, "data.frame")
  expected <- data.frame(
    variable = factor(
      c(rep("SEX", 2), rep("ARMCD", 5), rep("AGE", 4), rep("ARMCD:AGE", 3)),
      levels = c("SEX", "ARMCD", "AGE", "ARMCD:AGE")
    ),
    term = factor(
      c(
        "F", "M", "ARM A", "ARM B", "ARM B", "ARM C", "ARM C",
        rep("AGE", 4), "ARM A", "ARM B", "ARM C"
      ),
      levels = c("F", "M", "ARM A", "ARM B", "ARM C", "AGE")
    ),
    interaction = factor(
      c(rep("", 4), "AGE", "", "AGE", "", rep("ARMCD", 3), rep("", 3)),
      levels = c("", "AGE", "ARMCD")
    ),
    reference = factor(
      c(rep("", 4), "34", "", "34", "", "ARM A", "ARM B", "ARM C", rep("", 3)),
      levels = c("", "34", "ARM A", "ARM B", "ARM C")
    )
  )
  expected$estimate <- list(
    numeric(0), -0.379654068603096, numeric(0), 19.6400646633339,
    NA, 15.4488986205526, NA, 0.86399679862127, NA, NA, NA, numeric(0),
    -0.836486811025185, -0.628305227524577
  )
  expected$std_error <- list(
    numeric(0), 0.711864825000689, numeric(0), 13.6647262772159,
    NA, 14.7450624370364, NA, 0.578765963526213, NA, NA, NA,
    numeric(0), 0.580672630722349, 0.615655368309798
  )
  testthat::expect_equal(result, expected, tolerance = 0.000001)
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Degrees of Freedom", "df", "Parameter Estimate",
      "estimate", "Standard Error", "se", "Odds Ratio", "or", "Wald 75% CI",
      "ci", "p-value", "p"
    ),
    .Dim = c(2L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# logistic_summary_by_flag ----

testthat::test_that("logistic_summary_by_flag works", {
  layout_fun <- logistic_summary_by_flag("is_variable")
  testthat::expect_is(layout_fun, "function")
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
  df <- broom::tidy(mod1, conf_level = 0.99)
  result <- basic_table() %>%
    summarize_logistic(conf_level = 0.99) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Sex", "Reference F, n = 216", "M, n = 157",
      "Planned Arm Code", "Reference ARM A, n = 126", "ARM B, n = 121",
      "Age", "34", "ARM C, n = 126", "Age", "34", "Age", "Age", "Planned Arm Code",
      "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Age",
      "Reference ARM A, n = 126", "ARM B, n = 121", "ARM C, n = 126",
      "Degrees of Freedom", "", "", "1", "2", "", "1", "", "", "1",
      "", "", "", "1", "", "", "", "", "2", "", "1", "1", "Parameter Estimate",
      "", "", "-0.380", "", "", "19.640", "", "", "15.449", "", "", "",
      "0.864", "", "", "", "", "", "", "-0.836", "-0.628", "Standard Error",
      "", "", "0.712", "", "", "13.665", "", "", "14.745", "", "",
      "", "0.579", "", "", "", "", "", "", "0.581", "0.616", "Odds Ratio",
      "", "", "0.68", "", "", "", "", "0.00", "", "", "0.00", "", "", "",
      "2.37", "1.03", "1.27", "", "", "", "", "Wald 99% CI", "", "",
      "(0.11, 4.28)", "", "", "", "", "(<0.01, >999.99)", "", "", "(<0.01, >999.99)",
      "", "", "", "(0.53, 10.54)", "(0.90, 1.18)", "(0.74, 2.18)",
      "", "", "", "", "p-value", "", "", "0.5938", "0.2910", "", "0.1506",
      "", "", "0.2948", "", "", "", "0.1355", "", "", "", "", "0.2297",
      "", "0.1497", "0.3075"
    ),
    .Dim = c(22L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
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
  df <- broom::tidy(model, conf_level = 0.99)
  result <- basic_table() %>%
    summarize_logistic(conf_level = 0.99) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Age", "Age", "Planned Arm Code", "Reference ARM A, n = 126",
      "ARM B, n = 121", "Sex", "F", "M", "ARM C, n = 126", "Sex", "F",
      "M", "Sex", "Reference F, n = 216", "M, n = 157", "Planned Arm Code",
      "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Sex",
      "Reference ARM A or F, n = 266", "ARM B * M, n = 47", "ARM C * M, n = 60",
      "Degrees of Freedom", "", "1", "2", "", "1", "", "", "", "1",
      "", "", "", "", "", "1", "", "", "", "", "2", "", "1", "1", "Parameter Estimate",
      "", "0.067", "", "", "-1.549", "", "", "", "16.089", "", "",
      "", "", "", "16.075", "", "", "", "", "", "", "-16.410", "-32.572",
      "Standard Error", "", "0.053", "", "", "1.134", "", "", "", "2155.353",
      "", "", "", "", "", "2477.888", "", "", "", "", "", "", "2477.888",
      "3284.125", "Odds Ratio", "", "1.07", "", "", "", "", "0.21",
      "0.00", "", "", ">999.99", "0.00", "", "", "", "", ">999.99", "0.72",
      "0.00", "", "", "", "", "Wald 99% CI", "", "(0.93, 1.23)", "", "",
      "", "", "(0.01, 3.94)", "(0.00, >999.99)", "", "", "(0.00, >999.99)",
      "(0.00, >999.99)", "", "", "", "", "(0.00, >999.99)", "(0.09, 5.59)",
      "(0.00, >999.99)", "", "", "", "", "p-value", "", "0.2109", "0.3932",
      "", "0.1719", "", "", "", "0.9940", "", "", "", "", "", "0.9948",
      "", "", "", "", "0.9999", "", "0.9947", "0.9921"
    ),
    .Dim = c(24L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_logistic works as expected for simple model without interactions", {
  adrs <- adrs_example
  mod1 <- fit_logistic(
    adrs,
    variables = list(response = "Response", arm = "ARMCD", covariates = "AGE")
  )
  df <- broom::tidy(mod1, conf_level = 0.99)
  result <- basic_table() %>%
    summarize_logistic(conf_level = 0.99) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Planned Arm Code", "Reference ARM A, n = 126",
      "ARM B, n = 121", "ARM C, n = 126", "Age", "Age", "Degrees of Freedom",
      "2", "", "1", "1", "", "1", "Parameter Estimate", "", "", "-2.133",
      "-0.093", "", "0.066", "Standard Error", "", "", "1.080", "1.422",
      "", "0.053", "Odds Ratio", "", "", "0.12", "0.91", "", "1.07",
      "Wald 99% CI", "", "", "(<0.01, 1.91)", "(0.02, 35.52)", "",
      "(0.93, 1.23)", "p-value", "0.0367", "", "0.0483", "0.9481",
      "", "0.2185"
    ),
    .Dim = c(7L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
