library(random.cdisc.data)
library(rtables)
library(dplyr)

adsl_cached <- radsl(cached = TRUE) %>% dplyr::filter(SEX %in% c("F", "M"))
adrs_cached <- radrs(adsl_cached, seed = 2)
get_adrs <- function() {
  adrs_cached %>%
    dplyr::filter(
      PARAMCD == "BESRSPI",
      RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")
    ) %>%
    dplyr::mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0)) %>%
    reapply_varlabels(var_labels(adrs_cached))
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

test_that("fit_logistic works with different response definition", {
  data <- get_adrs()
  result_model <- fit_logistic(
    data,
    variables = list(response = "Response", arm = "ARMCD"),
    response_definition = "1 - response"
  )
  expect_equal(
    result_model$formula,
    1 - Response ~ ARMCD
  )
})

test_that("h_get_interaction_vars works as expected", {
  data <- get_adrs()
  model <- fit_logistic(
    data,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "RACE"
    )
  )
  result <- h_get_interaction_vars(model)
  expected <- c("ARMCD", "RACE")
  expect_setequal(result, expected)
})

test_that("h_interaction_coef_name works as expected", {
  data <- get_adrs()
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
  expect_identical(result, expected)
})

test_that("h_or_cat_interaction works as expected", {
  data <- get_adrs()
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
  expect_is(result_armcd, "list")
  expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    expect_is(res, "list")
    expect_named(res, levels(droplevels(data$RACE)))
  }
  expect_equivalent(
    result_armcd[["ARM B"]][["ASIAN"]],
    list(
      or = 0.16232,
      ci = c(0.01849, 1.42441)
    ),
    tol = 1e-4
  )
  result_race <- h_or_cat_interaction("RACE", "ARMCD", model)
  expect_is(result_race, "list")
  expect_named(result_race, levels(droplevels(data$RACE))[-1])
  for (res in result_race) {
    expect_is(res, "list")
    expect_named(res, levels(data$ARMCD))
  }
})

test_that("h_or_cont_interaction works as expected with median increment", {
  data <- get_adrs()
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
  expect_is(result_armcd, "list")
  expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    expect_is(res, "list")
    expect_named(res, as.character(median(data$AGE)))
  }
  expect_equivalent(
    result_armcd[["ARM B"]][["34"]],
    list(
      or = 0.24866,
      ci = c(0.06114, 1.01139)
    ),
    tol = 1e-4
  )
  result_age <- h_or_cont_interaction("AGE", "ARMCD", model)
  expect_is(result_age, "list")
  expect_named(result_age, levels(data$ARMCD))
  for (res in result_age) {
    expect_is(res, "list")
    expect_named(res, c("or", "ci"))
  }
  expect_equivalent(
    result_age[["ARM B"]],
    list(
      or = 0.99796,
      ci = c(0.91931, 1.08333)
    ),
    tol = 1e-4
  )
})

test_that("h_or_cont_interaction works as expected with custom increments", {
  data <- get_adrs()
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
  expect_is(result_armcd, "list")
  expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    expect_is(res, "list")
    expect_named(res, as.character(c(25, 30, 40)))
  }
  # We are not allowed to specify increments when the interaction variable is a factor.
  expect_error(h_or_cont_interaction(
    "AGE",
    "ARMCD",
    model,
    at = c(25, 30, 40)
  ))
})

test_that("h_or_interaction works as expected", {
  data <- get_adrs()
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
  expect_is(result_cont, "list")
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
  expect_is(result_cat, "list")
})

test_that("h_simple_term_labels works correctly with factor input", {
  x <- c("A", "B", "C", "B", "A")
  terms <- factor(c("B", "C"))
  table <- table(x)
  result <- h_simple_term_labels(terms, table)
  expected <- c("B, n = 2", "C, n = 1")
  expect_identical(result, expected)
})

test_that("h_interaction_term_labels works correctly with factor input", {
  x <- c("A", "B", "C", "B", "A")
  y <- c("1", "3", "2", "2", "1")
  terms1 <- factor(c("B", "C"))
  terms2 <- factor(c("2", "3"), levels = c("3", "2"))
  table <- table(x, y)
  result <- h_interaction_term_labels(terms1, terms2, table)
  expected <- c("B * 2, n = 1", "C * 3, n = 0")
  expect_identical(result, expected)
})

test_that("h_interaction_term_labels works correctly when any term can be fulfilled", {
  x <- c("A", "B", "C", "B", "A")
  y <- c("1", "3", "2", "2", "1")
  terms1 <- "B"
  terms2 <- "2"
  table <- table(x, y)
  result <- h_interaction_term_labels(terms1, terms2, table, any = TRUE)
  expected <- "B or 2, n = 3"
  expect_identical(result, expected)
})

test_that("h_glm_simple_term_extract works for factor and numeric variables", {
  adrs <- get_adrs()
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
  expected1$pvalue <- list(0.7633307, 0.6097390, 0.7207622)
  expected1$estimate <- list(numeric(0), -0.33522939569033, 0.296292628380204)
  expected1$std_error <- list(numeric(0), 0.65673817912887, 0.828929059143728)

  expect_equal(result1, expected1[, names(result1)], tolerance = 0.000001)

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
  expected2$estimate <- list(-0.0154436872166314)
  expected2$std_error <- list(0.0370085242079029)
  expected2$df <- list(1)
  expected2$pvalue <- list(0.6764584)
  expect_equal(result2, expected2[, names(result2)], tolerance = 0.00001)
})

test_that("h_glm_interaction_extract works for categorical interaction", {
  adrs <- get_adrs()
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
  expected$estimate <- list(numeric(0), 1.722828, 1.469858, -16.3095, -16.49343)
  expected$std_error <- list(numeric(0), 1.513541, 3755.583, 3409.299, 5556.542)
  expected$df <- list(4, 1, 1, 1, 1)
  expected$pvalue <- list(0.8621049, 0.2550051, 0.9996877, 0.9961831, 0.9976317)
  expect_equal(result, expected[, names(result)], tolerance = 0.000001)
})

test_that("h_glm_interaction_extract works for continuous interaction", {
  adrs <- get_adrs()
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
      estimate = list(numeric(0), 0.0765168047633205, 0.0806134116654586),
      std_error = list(numeric(0), 0.100660238964565, 202.171397083027),
      df = list(2, 1, 1),
      pvalue = list(0.749077041429099, 0.447165378840067, 0.999681853141517),
      is_variable_summary = c(TRUE, FALSE, FALSE),
      is_term_summary = c(FALSE, TRUE, TRUE)
    ),
    row.names = c(NA, -3L),
    class = "data.frame"
  )
  expect_equivalent(result, expected)
})

test_that("h_logistic_simple_terms works", {

  skip_if_too_deep(2)

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
  expected1$estimate <- list(-0.001764094, numeric(0), -1.34854854729683)
  expected1$std_error <- list(0.03810346, numeric(0), 0.632485325778419)
  expected1$df <- list(1, numeric(0), 1)
  expected1$pvalue <- list(0.9630732, numeric(0), 0.0329951722214885)
  expected1$odds_ratio <- list(0.9982375, numeric(0), 0.2596168)
  expected1$lcl <- list(0.9264034, numeric(0), 0.07515542)
  expected1$ucl <- list(1.075642, numeric(0), 0.89682)
  expected1$ci <- list(c(0.9264034, 1.0756416), numeric(0), c(0.07515542, 0.89682002))
  expect_equal(result1, expected1[, names(result1)], tolerance = 0.000001)

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
  expected2$estimate <- list(numeric(0), -1.34854854729683)
  expected2$std_error <- list(numeric(0), 0.632485325778419)
  expected2$df <- list(numeric(0), 1)
  expected2$pvalue <- list(numeric(0), 0.0329951722214885)
  expected2$odds_ratio <- list(numeric(0), 0.259616808827808)
  expected2$lcl <- list(numeric(0),  0.0509086319427838)
  expected2$ucl <- list(numeric(0), 1.32395793903255)
  expected2$ci <- list(numeric(0), c(0.0509086319427838, 1.32395793903255))
  expect_equal(result2, expected2[, names(result2)], tolerance = 0.000001)
})

test_that("h_glm_inter_term_extract works as expected with categorical interaction", {
  adrs <- get_adrs()
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
        -1.81816206889842,
        NA,
        NA,
        NA,
        16.4311487182376,
        NA,
        NA,
        NA
      ),
      std_error = list(
        numeric(0),
        1.10814323422898,
        NA,
        NA,
        NA,
        2072.90323550604,
        NA,
        NA,
        NA
      ),
      odds_ratio = c(
        NA,
        NA,
        0.162323816929255,
        0.909069078703235,
        1.34046224750204e-08,
        NA,
        13675941.0360017,
        59471465.9937293,
        0.939619768714003
      ),
      lcl = c(
        NA, NA, 0.0184981805996508, 0.119168317385949,
        0, NA, 0, 0, 0
      ),
      ucl = c(
        NA, NA, 1.42441151985399, 6.93478441235242,
        Inf, NA, Inf, Inf, Inf
      ),
      df = list(
        2, 1, NA, NA, NA, 1, NA,
        NA, NA
      ),
      pvalue = list(
        0.260270851480672,
        0.100853808130027,
        NA,
        NA,
        NA,
        0.993675526012668,
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
  expect_equivalent(result, expected)
})

test_that("h_glm_inter_term_extract works as expected with continuous interaction", {
  adrs <- get_adrs()
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
  expect_is(result1, "data.frame")
  result2 <- h_glm_inter_term_extract(
    odds_ratio_var = "AGE",
    interaction_var = "ARMCD",
    model,
    conf_level = 0.9
  )
  expect_is(result2, "data.frame")
})

test_that("h_logistic_inter_terms works as expected", {
  adrs <- get_adrs()
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
  expect_is(result_cat, "data.frame")
  expect_identical(
    result_cat$variable,
    c("AGE", "ARMCD", "ARMCD", "ARMCD", "ARMCD", "ARMCD", "ARMCD",
      "ARMCD", "SEX", "SEX", "SEX", "SEX", "SEX", "ARMCD:SEX", "ARMCD:SEX",
      "ARMCD:SEX")
  )
  expect_identical(
    result_cat$term_label,
    c("Age", "Reference ARM A, n = 126", "ARM B, n = 121", "ARM B, n = 121",
      "ARM B, n = 121", "ARM C, n = 126", "ARM C, n = 126", "ARM C, n = 126",
      "Reference F, n = 216", "M, n = 157", "M, n = 157", "M, n = 157",
      "M, n = 157", "Reference ARM A or F, n = 266", "ARM B * M, n = 47",
      "ARM C * M, n = 60")
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
  expect_is(result_cont, "data.frame")
  expect_identical(
    result_cont$term,
    c("F", "M", "ARM A", "ARM B",
      "ARM B", "ARM C", "ARM C", "AGE", "AGE", "AGE", "AGE", "ARM A",
      "ARM B", "ARM C")
  )
  expect_identical(
    result_cont$term_label,
    c("Reference F, n = 216", "M, n = 157", "Reference ARM A, n = 126",
      "ARM B, n = 121", "ARM B, n = 121", "ARM C, n = 126", "ARM C, n = 126",
      "Age", "Age", "Age", "Age", "Reference ARM A, n = 126", "ARM B, n = 121",
      "ARM C, n = 126")
  )
})

test_that("tidy.glm works as expected for simple case", {

  skip_if_too_deep(2)

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
  expect_equal(result, expected[, names(result)], tolerance = 0.000001)
})

test_that("tidy.glm works as expected for interaction case", {
  adrs <- get_adrs()
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
  expect_is(result, "data.frame")
  expected <- data.frame(
    variable = factor(
      c(rep("SEX", 2), rep("ARMCD", 5), rep("AGE", 4), rep("ARMCD:AGE", 3)),
      levels = c("SEX", "ARMCD", "AGE", "ARMCD:AGE")
    ),
    term = factor(
      c("F", "M", "ARM A", "ARM B", "ARM B", "ARM C", "ARM C",
        rep("AGE", 4), "ARM A", "ARM B", "ARM C"),
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
    numeric(0), -1.344396, numeric(0), -3.439949, NA, 15.07209, NA, -0.04828184,
    NA, NA, NA, numeric(0), 0.05889316, 0.05183692
  )
  expected$std_error <- list(
    numeric(0), 0.6334459, numeric(0), 3.496053, NA, 7084.929, NA, 0.08522134,
    NA, NA, NA, numeric(0), 0.09426531, 194.9034
  )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("logistic_regression_cols works as expected", {
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
    c("", "", "Degrees of Freedom", "df", "Parameter Estimate",
      "estimate", "Standard Error", "se", "Odds Ratio", "or", "Wald 75% CI",
      "ci", "p-value", "p"),
    .Dim = c(2L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("logistic_summary_by_flag works", {
  layout_fun <- logistic_summary_by_flag("is_variable")
  expect_is(layout_fun, "function")
  expect_silent(layout_fun(basic_table()))
})

test_that("summarize_logistic works as expected for interaction model with continuous variable", {
  adrs <- get_adrs()
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
    c("", "Sex", "Reference F, n = 216", "M, n = 157",
      "Planned Arm Code", "Reference ARM A, n = 126", "ARM B, n = 121",
      "Age", "34", "ARM C, n = 126", "Age", "34", "Age", "Age", "Planned Arm Code",
      "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Age",
      "Reference ARM A, n = 126", "ARM B, n = 121", "ARM C, n = 126",
      "Degrees of Freedom", "", "", "1", "2", "", "1", "", "", "1",
      "", "", "", "1", "", "", "", "", "2", "", "1", "1", "Parameter Estimate",
      "", "", "-1.344", "", "", "-3.44", "", "", "15.072", "", "",
      "", "-0.048", "", "", "", "", "", "", "0.059", "0.052", "Standard Error",
      "", "", "0.633", "", "", "3.496", "", "", "7084.929", "", "",
      "", "0.085", "", "", "", "", "", "", "0.094", "194.903", "Odds Ratio",
      "", "", "0.26", "", "", "", "", "0.24", "", "", ">999.99", "",
      "", "", "0.95", "1.01", "1", "", "", "", "", "Wald 99% CI", "",
      "", "(0.05, 1.33)", "", "", "", "", "(0.04, 1.50)", "", "", "(0.00, >999.99)",
      "", "", "", "(0.77, 1.19)", "(0.91, 1.13)", "(<0.01, >999.99)",
      "", "", "", "", "p-value", "", "", "0.0338", "0.6163", "", "0.3251",
      "", "", "0.9983", "", "", "", "0.5710", "", "", "", "", "0.8227",
      "", "0.5321", "0.9998"),
    .Dim = c(22L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("summarize_logistic works as expected for interaction model with categorical variable", {
  adrs <- get_adrs()
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
    c("", "Age", "Age", "Planned Arm Code", "Reference ARM A, n = 126",
      "ARM B, n = 121", "Sex", "F", "M", "ARM C, n = 126", "Sex", "F",
      "M", "Sex", "Reference F, n = 216", "M, n = 157", "Planned Arm Code",
      "ARM A", "ARM B", "ARM C", "Interaction of Planned Arm Code * Sex",
      "Reference ARM A or F, n = 266", "ARM B * M, n = 47", "ARM C * M, n = 60",
      "Degrees of Freedom", "", "1", "2", "", "1", "", "", "", "1",
      "", "", "", "", "", "1", "", "", "", "", "2", "", "1", "1", "Parameter Estimate",
      "", "0", "", "", "-0.028", "", "", "", "16.954", "", "", "",
      "", "", "0.28", "", "", "", "", "", "", "-2.28", "-0.28", "Standard Error",
      "", "0.037", "", "", "1.016", "", "", "", "2182.457", "", "",
      "", "", "", "1.243", "", "", "", "", "", "", "1.483", "3162.68",
      "Odds Ratio", "", "1", "", "", "", "", "0.97", "0.1", "", "",
      ">999.99", ">999.99", "", "", "", "", "1.32", "0.14", "1", "",
      "", "", "", "Wald 99% CI", "", "(0.91, 1.10)", "", "", "", "",
      "(0.07, 13.30)", "(<0.01, 1.63)", "", "", "(0.00, >999.99)",
      "(0.00, >999.99)", "", "", "", "", "(0.05, 32.48)", "(0.02, 1.13)",
      "(0.00, >999.99)", "", "", "", "", "p-value", "", "0.9922", "0.9996",
      "", "0.9780", "", "", "", "0.9938", "", "", "", "", "", "0.8218",
      "", "", "", "", "0.3065", "", "0.1241", "0.9999"),
    .Dim = c(24L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("summarize_logistic works as expected for simple model without interactions", {
  adrs <- get_adrs()
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
    c("", "Planned Arm Code", "Reference ARM A, n = 126",
      "ARM B, n = 121", "ARM C, n = 126", "Age", "Age", "Degrees of Freedom",
      "2", "", "1", "1", "", "1", "Parameter Estimate", "", "", "-1.276",
      "16.883", "", "-0.016", "Standard Error", "", "", "0.676", "1577.857",
      "", "0.036", "Odds Ratio", "", "", "0.28", ">999.99", "", "0.98",
      "Wald 99% CI", "", "", "(0.05, 1.59)", "(0.00, >999.99)", "",
      "(0.90, 1.08)", "p-value", "0.1681", "", "0.0590", "0.9915",
      "", "0.6602"),
    .Dim = c(7L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})
