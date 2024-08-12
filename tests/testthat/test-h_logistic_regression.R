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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
  testthat::expect_type(result_armcd, "list")
  testthat::expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, levels(droplevels(data$RACE)))
  }

  res <- testthat::expect_silent(result_armcd[["ARM B"]][["ASIAN"]])
  testthat::expect_snapshot(res)

  result_race <- h_or_cat_interaction("RACE", "ARMCD", model)
  testthat::expect_type(result_race, "list")
  testthat::expect_named(result_race, levels(droplevels(data$RACE))[-1])
  for (res in result_race) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, levels(data$ARMCD))
  }
})

# h_or_cont_interaction ----

testthat::test_that("h_or_cont_interaction works as expected with median increment", {
  data <- adrs_example
  suppressWarnings(model <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  ))
  result_armcd <- h_or_cont_interaction("ARMCD", "AGE", model)
  testthat::expect_type(result_armcd, "list")
  testthat::expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    testthat::expect_type(res, "list")
    testthat::expect_named(res, as.character(round(stats::median(data$AGE))))
  }

  res <- testthat::expect_silent(result_armcd[["ARM B"]][["34"]])
  testthat::expect_snapshot(res)

  result_age <- h_or_cont_interaction("AGE", "ARMCD", model)
  testthat::expect_type(result_age, "list")
  testthat::expect_named(result_age, levels(data$ARMCD))
  for (res in result_age) {
    testthat::expect_type(res, "list")

    res <- testthat::expect_silent(names(res))
    testthat::expect_snapshot(res)
  }
  res <- testthat::expect_silent(result_age[["ARM B"]])
  testthat::expect_snapshot(res)
})

testthat::test_that("h_or_cont_interaction works as expected with custom increments", {
  data <- adrs_example
  suppressWarnings(model <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  ))
  result_armcd <- h_or_cont_interaction(
    "ARMCD",
    "AGE",
    model,
    at = c(25, 30, 40),
    conf_level = 0.8
  )
  testthat::expect_type(result_armcd, "list")
  testthat::expect_named(result_armcd, levels(data$ARMCD)[-1])
  for (res in result_armcd) {
    testthat::expect_type(res, "list")

    res <- testthat::expect_silent(names(res))
    testthat::expect_snapshot(res)
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
  suppressWarnings(model_cont <- fit_logistic(
    data,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  ))
  result_cont <- h_or_interaction(
    "ARMCD",
    "AGE",
    model_cont,
    at = c(20, 30),
    conf_level = 0.9
  )
  testthat::expect_type(result_cont, "list")
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
  testthat::expect_type(result_cat, "list")
})

# h_simple_term_labels ----

testthat::test_that("h_simple_term_labels works correctly with factor input", {
  x <- c("A", "B", "C", "B", "A")
  terms <- factor(c("B", "C"))
  table <- table(x)
  result <- h_simple_term_labels(terms, table)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# h_interaction_term_labels ----

testthat::test_that("h_interaction_term_labels works correctly with factor input", {
  x <- c("A", "B", "C", "B", "A")
  y <- c("1", "3", "2", "2", "1")
  terms1 <- factor(c("B", "C"))
  terms2 <- factor(c("2", "3"), levels = c("3", "2"))
  table <- table(x, y)
  result <- h_interaction_term_labels(terms1, terms2, table)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_interaction_term_labels works correctly when any term can be fulfilled", {
  x <- c("A", "B", "C", "B", "A")
  y <- c("1", "3", "2", "2", "1")
  terms1 <- "B"
  terms2 <- "2"
  table <- table(x, y)
  result <- h_interaction_term_labels(terms1, terms2, table, any = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# h_glm_simple_term_extract ----

testthat::test_that("h_glm_simple_term_extract works for factor and numeric variables", {
  adrs <- adrs_example
  suppressWarnings(mod1 <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD", covariates = c("AGE", "RACE")
    )
  ))

  result1 <- h_glm_simple_term_extract("RACE", mod1)

  res <- testthat::expect_silent(result1)
  testthat::expect_snapshot(res)

  result2 <- h_glm_simple_term_extract("AGE", mod1)

  res <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res)
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

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    mod <- fit_logistic(
      data,
      variables = list(
        response = "Response",
        arm = "ARMCD",
        covariates = c("AGE", "RACE"),
        strata = "STRATA1"
      )
    )
  )

  result <- testthat::expect_silent(unlist(h_glm_simple_term_extract("AGE", mod)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_interaction_extract works for continuous interaction", {
  adrs <- adrs_example
  suppressWarnings(model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("AGE", "RACE"),
      interaction = "AGE"
    )
  ))
  result <- h_glm_interaction_extract("ARMCD:AGE", model)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result1)
  testthat::expect_snapshot(res)

  result2 <- h_logistic_simple_terms("SEX", mod1, conf_level = 0.99)

  res <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res)
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

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    mod <- fit_logistic(
      data,
      variables = list(
        response = "Response",
        arm = "ARMCD",
        covariates = c("AGE", "RACE", "SEX"),
        strata = "STRATA1"
      )
    )
  )
  result <- testthat::expect_silent(unlist(h_logistic_simple_terms("AGE", mod)))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_glm_inter_term_extract works as expected with continuous interaction", {
  adrs <- adrs_example
  suppressWarnings(model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response", arm = "ARMCD",
      covariates = c("AGE", "RACE"), interaction = "AGE"
    )
  ))
  result1 <- h_glm_inter_term_extract(
    odds_ratio_var = "ARMCD",
    interaction_var = "AGE",
    model,
    conf_level = 0.9
  )
  testthat::expect_s3_class(result1, "data.frame")
  result2 <- h_glm_inter_term_extract(
    odds_ratio_var = "AGE",
    interaction_var = "ARMCD",
    model,
    conf_level = 0.9
  )
  testthat::expect_s3_class(result2, "data.frame")
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
  testthat::expect_s3_class(result_cat, "data.frame")

  res <- testthat::expect_silent(result_cat$variable)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(result_cat$term_label)
  testthat::expect_snapshot(res)

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
  testthat::expect_s3_class(result_cont, "data.frame")

  res <- testthat::expect_silent(result_cont$term)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(result_cont$term_label)
  testthat::expect_snapshot(res)
})
