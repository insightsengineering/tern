# [`get_simple`]: simple fabricated data set for test scenarios.
raw_data <- data.frame(
  time = c(5, 5, 10, 10, 5, 5, 10, 10),
  status = c(0, 0, 1, 0, 0, 1, 1, 1),
  armcd = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B")),
  age = c(15, 68, 65, 17, 12, 33, 45, 20),
  stage = factor(
    c("1", "2", "1", "1", "1", "2", "1", "2"),
    levels = c("1", "2")
  )
)

# Bladder data from survival
dta_bladder_raw <- local({
  # Setting general random for data generation
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- with(
    data = survival::bladder[survival::bladder$enum < 5, ],
    data.frame(
      time = stop,
      status = event,
      arm = paste("ARM:", as.factor(rx)),
      armcd = formatters::with_label(as.factor(rx), "ARM"),
      covar1 = formatters::with_label(as.factor(enum), "A Covariate Label"),
      covar2 = formatters::with_label(
        factor(sample(as.factor(enum)), levels = 1:4, labels = c("F", "F", "M", "M")), "Sex (F/M)"
      ),
      age = sample(20:60, size = 340, replace = TRUE)
    )
  )
  dta_bladder
})

# h_coxreg_univar_formulas ----

testthat::test_that("h_coxreg_univar_formulas creates formulas with covariate", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_univar_formulas creates formulas with strata", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_univar_formulas creates formula for reference when treatment is only considered", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_univar_formulas creates formulas with interactions", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    ),
    interaction = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_univar_formulas creates formula without treatment arm", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", covariates = c("X", "y"),
      strata = "SITE"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_univar_formulas fails when requesting interaction without treatment arm", {
  testthat::expect_error(h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", covariates = c("X", "y"),
      strata = "SITE"
    ),
    interaction = TRUE
  ))
})

testthat::test_that("h_coxreg_univar_formulas fails when requesting interaction without covariates", {
  testthat::expect_error(h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      strata = "SITE"
    ),
    interaction = TRUE
  ))
})

testthat::test_that("h_coxreg_univar_formulas creates formulas with multiple strata", {
  result <- h_coxreg_univar_formulas(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = c("SITE", "COUNTRY")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# h_coxreg_multivar_extract ----

testthat::test_that("h_coxreg_multivar_extract extracts correct coxph results when covariate names overlap", {
  set.seed(1, kind = "Mersenne-Twister")
  dta_simple <- raw_data
  mod <- survival::coxph(survival::Surv(time, status) ~ age + stage, data = dta_simple)
  result <- h_coxreg_multivar_extract(var = "age", mod = mod, data = dta_simple)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_multivar_extract extracts correct coxph results when covariate is a factor", {
  set.seed(1, kind = "Mersenne-Twister")
  dta_simple <- raw_data
  mod <- survival::coxph(survival::Surv(time, status) ~ age + stage, data = dta_simple)
  result <- h_coxreg_multivar_extract(var = "stage", mod = mod, data = dta_simple)
  attributes(result)$heading <- NULL

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# h_coxreg_multivar_formula ----

testthat::test_that("h_coxreg_multivar_formula creates formula without covariate", {
  result <- h_coxreg_multivar_formula(
    variables = list(arm = "ARMCD", event = "EVNT", time = "TIME", covariates = character())
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_multivar_formula creates formulas with a strata", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = "SITE"
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_multivar_formula creates formulas with multiple strata", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
      strata = c("SITE", "COUNTRY")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_multivar_formula creates formula with covariate", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", arm = "armcd", covariates = c("covar1", "covar2")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_multivar_formula creates formula without treatment arm", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", covariates = c("covar1", "covar2")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_multivar_formula creates formulas with multiple strata and without arm", {
  result <- h_coxreg_multivar_formula(
    variables = list(
      time = "time", event = "status", covariates = c("X", "y"),
      strata = c("SITE", "COUNTRY")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# control_coxreg ----

testthat::test_that("control_coxreg returns a standard list of parameters", {
  result <- control_coxreg()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# fit_coxreg_univar ----

testthat::test_that("fit_coxreg_univar returns model results as expected", {
  data <- dta_bladder_raw
  control <- control_coxreg(conf_level = 0.91)
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = "covar1"
  )
  forms <- h_coxreg_univar_formulas(
    variables = variables,
  )

  result <- fit_coxreg_univar(
    variables = variables,
    data = data,
    control = control
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("fit_coxreg_univar runs with non-represented level of a factor", {
  data <- dta_bladder_raw %>%
    dplyr::filter(covar1 %in% 1:3)

  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = "covar1"
  )

  testthat::expect_silent(fit_coxreg_univar(variables = variables, data = data))
})

testthat::test_that("fit_coxreg_univar is stopped when there are not 2 arms", {
  data <- dta_bladder_raw %>%
    dplyr::filter(covar1 %in% 1:3)

  variables <- list(
    time = "time", event = "status", arm = "covar1", covariates = "covar2"
  )

  testthat::expect_error(fit_coxreg_univar(variables = variables, data = data))
})

testthat::test_that("fit_coxreg_univar is stopped when likelihood method is used together with strata", {
  data <- dta_bladder_raw

  variables <- list(
    time = "time", event = "status", arm = "armcd", covariates = "age", strata = "covar1"
  )

  testthat::expect_error(
    fit_coxreg_univar(
      variables = variables, data = data, control = control_coxreg(pval_method = "likelihood")
    )
  )
})

testthat::test_that("fit_coxreg_univar works without treatment arm", {
  data <- dta_bladder_raw

  variables <- list(
    time = "time", event = "status", covariates = "age", strata = "covar1"
  )

  result <- testthat::expect_silent(fit_coxreg_univar(variables = variables, data = data))

  res <- testthat::expect_silent(names(result$mod))
  testthat::expect_snapshot(res)
})

# tidy.summary.coxph ----

testthat::test_that("tidy.summary.coxph method tidies up the Cox regression model", {
  dta_simple <- raw_data
  mod <- summary(survival::coxph(survival::Surv(time, status) ~ armcd, data = dta_simple))
  result <- broom::tidy(mod)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# h_coxreg_univar_extract ----

testthat::test_that("h_coxreg_univar_extract extracts coxph results", {
  dta_simple <- raw_data
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd, data = dta_simple)
  result <- h_coxreg_univar_extract(effect = "armcd", covar = "armcd", mod = mod, data = dta_simple)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# muffled_car_anova ----

testthat::test_that("muffled_car_anova muffles notes about dropped strata term", {
  bladder1 <- survival::bladder[survival::bladder$enum < 5, ]
  mod <- survival::coxph(
    survival::Surv(stop, event) ~ (rx + size + number) * strata(enum) + cluster(id),
    bladder1
  )
  testthat::expect_message(car::Anova(mod, test.statistic = "Wald"))
  testthat::expect_silent(muffled_car_anova(mod, test_statistic = "Wald"))
})

testthat::test_that("muffled_car_anova gives a hint in the error message when an error occurs", {
  bladder2 <- survival::bladder[1:20, ]
  mod <- survival::coxph(
    survival::Surv(stop, event) ~ (rx + size + number) * strata(enum) + cluster(id),
    bladder2
  )
  testthat::expect_error(
    muffled_car_anova(mod, test_statistic = "Wald"),
    "the model seems to have convergence problems"
  )
})

# tidy.coxreg.univar ----

testthat::test_that("tidy.coxreg.univar method tidies up the univariate Cox regression model", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = dta_bladder_raw
  )
  result <- broom::tidy(univar_model)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tidy.coxreg.univar method works with only numeric covariates with strata", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = "age", strata = c("covar1", "covar2")
    ),
    data = dta_bladder_raw
  )
  result <- broom::tidy(univar_model)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tidy.coxreg.univar method works without treatment arm", {
  univar_model <- fit_coxreg_univar(
    variables = list(
      time = "time", event = "status",
      covariates = c("age", "covar1"), strata = "covar2"
    ),
    data = dta_bladder_raw
  )
  result <- testthat::expect_silent(broom::tidy(univar_model))

  res <- testthat::expect_silent(result$term)
  testthat::expect_snapshot(res)
})

# h_coxreg_extract_interaction ----

testthat::test_that("h_coxreg_extract_interaction works with factor as covariate", {
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * covar1, data = dta_bladder_raw)
  testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "covar1", mod = mod, data = dta_bladder_raw,
      control = control_coxreg()
    )
  )
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = dta_bladder_raw[["covar1"]],
      effect = "armcd", covar = "covar1", mod = mod, data = dta_bladder_raw,
      control = control_coxreg()
    )
  )
})

# h_coxreg_inter_effect ----

testthat::test_that("h_coxreg_inter_effect works with numerics as covariate", {
  mod1 <- survival::coxph(survival::Surv(time, status) ~ armcd * age, data = dta_bladder_raw)
  testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "age", mod = mod1, control = control_coxreg(),
      at = list(), data = dta_bladder_raw
    )
  )
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = dta_bladder_raw[["age"]],
      effect = "armcd", covar = "age", mod = mod1, control = control_coxreg(),
      at = list(), data = dta_bladder_raw
    )
  )

  mod2 <- survival::coxph(survival::Surv(time, status) ~ armcd * age + strata(covar1), data = dta_bladder_raw)
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = dta_bladder_raw[["age"]],
      effect = "armcd", covar = "age", mod = mod2, data = dta_bladder_raw,
      at = list(), control = control_coxreg()
    )
  )
})

testthat::test_that("h_coxreg_inter_effect.numerics works with _:_ in effect levels", {
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * age, data = dta_bladder_raw)
  expected <- testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "age", mod = mod, control = control_coxreg(),
      at = list(), data = dta_bladder_raw
    )
  )

  mod <- survival::coxph(survival::Surv(time, status) ~ arm * age, data = dta_bladder_raw)
  result <- testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "arm", covar = "age", mod = mod, control = control_coxreg(),
      at = list(), data = dta_bladder_raw
    )
  )
  # The first column in the effect (arm/armcd) and expected to vary.
  testthat::expect_equal(result[, -1], expected[, -1], ignore_attr = TRUE)
})

testthat::test_that("h_coxreg_inter_effect works with character covariate", {
  dta_bladder_raw$covar2 <- as.character(dta_bladder_raw$covar2)

  mod1 <- survival::coxph(survival::Surv(time, status) ~ armcd * covar2, data = dta_bladder_raw)
  testthat::expect_silent(
    h_coxreg_extract_interaction(
      effect = "armcd", covar = "covar2", mod = mod1, control = control_coxreg(),
      at = list(), data = dta_bladder_raw
    )
  )
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = dta_bladder_raw[["covar2"]],
      effect = "armcd", covar = "covar2", mod = mod1, control = control_coxreg(),
      at = list(), data = dta_bladder_raw
    )
  )

  mod2 <- survival::coxph(survival::Surv(time, status) ~ armcd * covar2 + strata(covar1), data = dta_bladder_raw)
  testthat::expect_silent(
    h_coxreg_inter_effect(
      x = dta_bladder_raw[["covar2"]],
      effect = "armcd", covar = "covar2", mod = mod2, data = dta_bladder_raw,
      at = list(), control = control_coxreg()
    )
  )
})

# h_coxreg_inter_estimations ----

testthat::test_that("h_coxreg_inter_estimations' results identical to soon deprecated estimate_coef", {
  # Testing dataset [survival::bladder].
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- dta_bladder_raw

  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * covar1, data = dta_bladder)

  result <- h_coxreg_inter_estimations(
    variable = "armcd", given = "covar1",
    lvl_var = levels(dta_bladder$armcd),
    lvl_given = levels(dta_bladder$covar1),
    mod = mod, conf_level = .95
  )

  mmat <- stats::model.matrix(mod)[1, ]
  mmat[!mmat == 0] <- 0

  expected <- estimate_coef(
    variable = "armcd", given = "covar1",
    lvl_var = levels(dta_bladder$armcd),
    lvl_given = levels(dta_bladder$covar1),
    coef = stats::coef(mod), mmat = mmat, vcov = stats::vcov(mod),
    conf_level = .95
  )
  testthat::expect_identical(result, expected)
})

# fit_coxreg_multivar ----

testthat::test_that("fit_coxreg_multivar returns model results as expected", {
  data <- dta_bladder_raw
  control <- control_coxreg(conf_level = 0.91)
  variables <- list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  )
  form <- h_coxreg_multivar_formula(variables = variables)

  result <- fit_coxreg_multivar(
    variables = variables,
    data = data,
    control = control
  )

  res <- testthat::expect_silent(result$mod)
  testthat::expect_snapshot(res)
})

testthat::test_that("fit_coxreg_multivar is stopped when likelihood method is used together with strata", {
  data <- dta_bladder_raw

  variables <- list(
    time = "time", event = "status", arm = "armcd", covariates = "age", strata = "covar1"
  )

  testthat::expect_error(
    fit_coxreg_multivar(
      variables = variables, data = data, control = control_coxreg(pval_method = "likelihood")
    )
  )
})

testthat::test_that("fit_coxreg_multivar works correctly also without treatment arm", {
  data <- dta_bladder_raw
  control <- control_coxreg(conf_level = 0.9)
  variables <- list(
    time = "time", event = "status",
    covariates = c("covar1", "covar2")
  )
  result <- testthat::expect_silent(fit_coxreg_multivar(
    variables = variables,
    data = data,
    control = control
  ))
  testthat::expect_s3_class(result$mod, "coxph")
  testthat::expect_equal(stats::formula(result$mod), survival::Surv(time, status) ~ 1 + covar1 + covar2,
    ignore_attr = TRUE
  )
})

# tidy.coxreg.multivar ----

testthat::test_that("tidy.coxreg.multivar method tidies up the multivariate Cox regression model", {
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- dta_bladder_raw

  multivar_model <- fit_coxreg_multivar(
    variables = list(
      time = "time", event = "status", arm = "armcd",
      covariates = c("covar1", "covar2")
    ),
    data = dta_bladder,
    control = control_coxreg(ties = "efron")
  )
  result <- broom::tidy(multivar_model)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
