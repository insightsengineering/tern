library(scda)

get_simple <- function() {
  data.frame(
    time = c(5, 5, 10, 10, 5, 5, 5, 10, 10, 5),
    status = c(0, 0, 1, 0, 1, 0, 1, 1, 1, 0),
    armcd  = factor(LETTERS[c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)], levels = c("A", "B")),
    age = c(15, 68, 65, 17, 52, 12, 33, 45, 20, 17),
    stage = factor(
      c("1", "2", "1", "1", "1", "1", "2", "1", "2", "1"),
      levels = c("1", "2")
    ),
    rsp = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
}

get_adrs <- function() {
  synthetic_cdisc_data("rcd_2021_05_05")$adrs %>%
    dplyr::filter(ARMCD %in% c("ARM A", "ARM B")) %>%
    dplyr::mutate(
      RSP = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
      ARMBIN = droplevels(ARMCD)
    )
}

# h_step_window ----

test_that("h_step_window works as expected for percentiles", {
  x <- -5:10
  control <- control_step(use_percentile = TRUE, num_points = 5L, bandwidth = 0.2)
  result <- expect_silent(h_step_window(x = x, control = control))
  expect_is(result, "list")
  expect_named(result, c("sel", "interval"))
  sel <- result$sel
  expect_is(sel, "matrix")
  expect_identical(dim(sel), c(length(x), control$num_points))
  expect_identical(mode(sel), "logical")
  interval <- result$interval
  expect_is(interval, "matrix")
  expect_identical(
    colnames(interval),
    c("Percentile Center", "Percentile Lower", "Percentile Upper",
      "Interval Center", "Interval Lower", "Interval Upper")
  )
  expect_identical(nrow(interval), control$num_points)
})

test_that("h_step_window works as expected for actual biomarker values", {
  x <- -5:10
  control <- control_step(use_percentile = FALSE, num_points = 5L, bandwidth = 3)
  result <- expect_silent(h_step_window(x = x, control = control))
  expect_is(result, "list")
  expect_named(result, c("sel", "interval"))
  sel <- result$sel
  expect_true(all(colSums(sel) %in% (control$bandwidth * 2 + c(0L, 1L))))
  interval <- result$interval
  expect_is(interval, "matrix")
  expect_identical(
    colnames(interval),
    c("Interval Center", "Interval Lower", "Interval Upper")
  )
})

test_that("h_step_window also works for bandwidth `NULL`", {
  x <- -5:10
  control <- control_step(bandwidth = NULL, num_points = 2L)
  result <- expect_silent(h_step_window(x = x, control = control))
  expect_true(all(result$sel))
  expect_true(all(result$interval[, "Percentile Lower"] == 0))
  expect_true(all(result$interval[, "Percentile Upper"] == 1))
})

# h_step_trt_effect ----

test_that("h_step_trt_effect works for Cox models without interaction", {
  dta_simple <- get_simple()
  # Use a model without biomarker interaction, then we can compare with summary results.
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd + age, data = dta_simple)
  vars <- list(
    arm = "armcd",
    biomarker = "age"
  )
  result <- h_step_trt_effect(
    data = dta_simple,
    model = mod,
    variables = vars,
    x = 100  # Note: this does not have any effect on results, because there is no interaction.
  )
  coef_sum <- summary(mod)$coefficients
  expect_equivalent(result, coef_sum["armcdB", c("coef", "se(coef)")])
})

test_that("h_step_trt_effect works for Cox models with interaction", {
  dta_simple <- get_simple()
  mod <- survival::coxph(survival::Surv(time, status) ~ armcd * age + stage, data = dta_simple)
  vars <- list(
    arm = "armcd",
    biomarker = "age"
  )
  # Try first at age = 0, which reduces to no interaction case.
  result <- h_step_trt_effect(
    data = dta_simple,
    model = mod,
    variables = vars,
    x = 0
  )
  coef_sum <- summary(mod)$coefficients
  expect_equivalent(result, coef_sum["armcdB", c("coef", "se(coef)")])
  # Then at age = 50. Compare point estimate with manual calculation.
  result <- h_step_trt_effect(
    data = dta_simple,
    model = mod,
    variables = vars,
    x = 50
  )
  est_manual <- sum(coef(mod)[c("armcdB", "age", "armcdB:age")] * (c(1, 50, 50) - c(0, 50, 0)))
  expect_equivalent(result["est"], est_manual)
  # Regression test for se.
  expect_equivalent(result["se"], 1.322242, tol = 1e-5)
})

test_that("h_step_trt_effect works for Cox models with strata", {
  dta_simple <- get_simple()
  mod <- survival::coxph(
    survival::Surv(time, status) ~ armcd * age + strata(stage),
    data = dta_simple
  )
  vars <- list(
    arm = "armcd",
    biomarker = "age"
  )
  # Try first at age = 0, which reduces to no interaction case.
  result <- expect_silent(h_step_trt_effect(
    data = dta_simple,
    model = mod,
    variables = vars,
    x = 0
  ))
  coef_sum <- summary(mod)$coefficients
  expect_equivalent(result, coef_sum["armcdB", c("coef", "se(coef)")])
})

test_that("h_step_trt_effect works for logistic regression models without interaction", {
  dta_simple <- get_simple()
  # Use a model without biomarker interaction, then we can compare with summary results.
  mod <- glm(status ~ armcd + age, data = dta_simple, family = binomial())
  vars <- list(
    arm = "armcd",
    biomarker = "age"
  )
  result <- h_step_trt_effect(
    data = dta_simple,
    model = mod,
    variables = vars,
    x = 100  # Note: this does not have any effect on results, because there is no interaction.
  )
  coef_sum <- summary(mod)$coefficients
  expect_equivalent(result, coef_sum["armcdB", c("Estimate", "Std. Error")])
})

test_that("h_step_trt_effect works for logistic regression models with interaction", {
  dta <- get_adrs()
  mod <- glm(formula = RSP ~ ARMBIN * AGE + RACE, data = dta, family = binomial())
  vars <- list(
    arm = "ARMBIN",
    biomarker = "AGE"
  )
  # Try first at age = 0, which reduces to no interaction case.
  result <- h_step_trt_effect(
    data = dta,
    model = mod,
    variables = vars,
    x = 0
  )
  coef_sum <- summary(mod)$coefficients
  expect_equivalent(result, coef_sum["ARMBINARM B", c("Estimate", "Std. Error")])
  # Then at age = 50. Compare point estimate with manual calculation.
  result <- h_step_trt_effect(
    data = dta,
    model = mod,
    variables = vars,
    x = 50
  )
  est_manual <- sum(coef(mod)[c("ARMBINARM B", "AGE", "ARMBINARM B:AGE")] * (c(1, 50, 50) - c(0, 50, 0)))
  expect_equivalent(result["est"], est_manual)
  # Regression test for se.
  expect_equivalent(result["se"], 0.2153840, tol = 1e-5)
})

test_that("h_step_trt_effect works for conditional logistic regression without interaction", {
  dta <- get_adrs()
  mod <- clogit(formula = RSP ~ ARMBIN + strata(STRATA1), data = dta)
  vars <- list(
    arm = "ARMBIN",
    biomarker = "AGE"
  )
  result <- h_step_trt_effect(
    data = dta,
    model = mod,
    variables = vars,
    x = 0
  )
  coef_sum <- summary(mod)$coefficients
  expect_equivalent(result, coef_sum["ARMBINARM B", c("coef", "se(coef)")])
})

test_that("h_step_trt_effect works for conditional logistic regression with interaction", {
  dta <- get_adrs()
  mod <- clogit(formula = RSP ~ ARMBIN * AGE + strata(STRATA1), data = dta)
  vars <- list(
    arm = "ARMBIN",
    biomarker = "AGE"
  )
  result <- h_step_trt_effect(
    data = dta,
    model = mod,
    variables = vars,
    x = 50
  )
  est_manual <- sum(coef(mod)[c("ARMBINARM B", "AGE", "ARMBINARM B:AGE")] * (c(1, 50, 50) - c(0, 50, 0)))
  expect_equivalent(result["est"], est_manual)
  # Regression test for se.
  expect_equivalent(result["se"], 0.2100507, tol = 1e-5)
})

# h_step_survival_formula ----

test_that("h_step_survival_formula works correctly without covariates or strata", {
  expect_equal(
    h_step_survival_formula(
      list(arm = "TRT", biomarker = "BM", event = "EV", time = "TIME"),
      control = control_step(degree = 3)
    ),
    Surv(TIME, EV) ~ TRT * poly(BM, degree = 3, raw = TRUE)
  )
  expect_equal(
    h_step_survival_formula(
      list(arm = "TRT", biomarker = "BM", event = "EV", time = "TIME"),
      control = control_step(degree = 0)
    ),
    Surv(TIME, EV) ~ TRT
  )
})

test_that("h_step_survival_formula works correctly with covariates", {
  expect_equal(
    h_step_survival_formula(
      list(arm = "TRT", biomarker = "BM", event = "EV", time = "TIME", covariates = c("A", "B")),
      control = control_step(degree = 2)
    ),
    Surv(TIME, EV) ~ TRT * poly(BM, degree = 2, raw = TRUE) + A + B
  )
})

test_that("h_step_survival_formula works correctly with strata", {
  expect_equal(
    h_step_survival_formula(
      list(arm = "TRT", biomarker = "BM", event = "EV", time = "TIME", strata = c("A", "B")),
    ),
    Surv(TIME, EV) ~ TRT + strata(A, B)
  )
})

# h_step_survival_est ----

test_that("h_step_survival_est works as expected", {
  dta_simple <- get_simple()
  vars <- list(
    arm = "armcd",
    biomarker = "age",
    time = "time",
    event = "status",
    covariates = "stage"
  )
  age_vals <- c(20, 30, 40)
  form <- h_step_survival_formula(
    variables = vars,
    control = control_step(degree = 1)
  )
  result <- expect_silent(h_step_survival_est(
    formula = form,
    data = dta_simple,
    variables = vars,
    x = age_vals
  ))
  expect_is(result, "matrix")
  expect_identical(dim(result), c(3L, 6L))
  expect_identical(colnames(result), c("n", "events", "loghr", "se", "ci_lower", "ci_upper"))
  expect_equal(result[, "loghr"], c(1.075486, 1.082507, 1.089528), tol = 1e-6)
})

test_that("h_step_survival_est gives a readable warning when fitting warnings occur", {
  dta_simple <- get_simple()
  vars <- list(
    arm = "armcd",
    biomarker = "age",
    time = "time",
    event = "status",
    covariates = "stage"
  )
  age_vals <- c(20, 30, 40)
  # We increase `degree` such that model becomes very complex and hence fitting warnings occur.
  form <- h_step_survival_formula(
    variables = vars,
    control = control_step(degree = 4)
  )
  expect_warning(
    h_step_survival_est(
      formula = form,
      data = dta_simple,
      variables = vars,
      x = age_vals
    ),
    "Fit warnings occurred, please consider using a simpler model"
  )
})

# h_step_rsp_formula ----

test_that("h_step_rsp_formula works correctly without covariates", {
  expect_equal(
    h_step_rsp_formula(
      list(arm = "TRT", biomarker = "BM", response = "RSP"),
      control = c(control_step(degree = 3), control_logistic())
    ),
    RSP ~ TRT * poly(BM, degree = 3, raw = TRUE)
  )
  expect_equal(
    h_step_rsp_formula(
      list(arm = "TRT", biomarker = "BM", response = "RSP")
    ),
    RSP ~ TRT
  )
})

test_that("h_step_rsp_formula works correctly with covariates", {
  expect_equal(
    h_step_rsp_formula(
      list(arm = "TRT", biomarker = "BM", response = "RSP", covariates = c("A", "B")),
      control = c(control_logistic(), control_step(degree = 2))
    ),
    RSP ~ TRT * poly(BM, degree = 2, raw = TRUE) + A + B
  )
})

test_that("h_step_rsp_formula works correctly with different response definition", {
  expect_equal(
    h_step_rsp_formula(
      list(arm = "TRT", biomarker = "BM", response = "AVAL"),
      control = c(
        control_logistic(response_definition = "I(response == 'CR')"),
        control_step(degree = 1)
      )
    ),
    I(AVAL == "CR") ~ TRT * poly(BM, degree = 1, raw = TRUE)
  )
})

test_that("h_step_rsp_formula works correctly with strata", {
  expect_equal(
    h_step_rsp_formula(
      list(arm = "TRT", biomarker = "BM", response = "RSP", strata = c("A", "B")),
      control = c(control_logistic(), control_step(degree = 2))
    ),
    RSP ~ TRT * poly(BM, degree = 2, raw = TRUE) + strata(I(interaction(A, B)))
  )
  expect_equal(
    h_step_rsp_formula(
      list(arm = "TRT", biomarker = "BM", response = "RSP", strata = "A"),
      control = c(control_logistic(), control_step(degree = 2))
    ),
    RSP ~ TRT * poly(BM, degree = 2, raw = TRUE) + strata(A)
  )
})

# h_step_rsp_est ----

test_that("h_step_rsp_est works as expected without strata", {
  dta_simple <- get_simple()
  vars <- list(
    arm = "armcd",
    biomarker = "age",
    response = "rsp",
    covariates = "time"
  )
  age_vals <- c(20, 30, 40)
  form <- h_step_rsp_formula(
    variables = vars,
    control = c(control_logistic(), control_step(degree = 1))
  )
  subset <- dta_simple$age > 15
  result <- expect_silent(h_step_rsp_est(
    formula = form,
    data = dta_simple,
    variables = vars,
    subset = subset,
    x = age_vals
  ))
  expect_is(result, "matrix")
  expect_identical(dim(result), c(3L, 5L))
  expect_identical(colnames(result), c("n", "logor", "se", "ci_lower", "ci_upper"))
  expect_equal(result[, "logor"], c(2.008012, 1.025773, 0.043535), tol = 1e-6)
  expect_equal(result[, "n"], rep(sum(subset), 3L))
})

test_that("h_step_rsp_est works as expected with strata", {
  dta_simple <- get_simple()
  vars <- list(
    arm = "armcd",
    biomarker = "age",
    response = "rsp",
    covariates = "time",
    strata = "stage"
  )
  age_vals <- c(20, 30, 40)
  form <- h_step_rsp_formula(
    variables = vars,
    control = c(control_logistic(), control_step(degree = 1))
  )
  subset <- dta_simple$age > 15
  result <- expect_silent(h_step_rsp_est(
    formula = form,
    data = dta_simple,
    variables = vars,
    subset = subset,
    x = age_vals
  ))
  expect_is(result, "matrix")
  expect_identical(dim(result), c(3L, 5L))
  expect_identical(colnames(result), c("n", "logor", "se", "ci_lower", "ci_upper"))
  expect_equal(result[, "logor"], c(1.134057, 0.511102, -0.111853), tol = 1e-6)
  expect_equal(result[, "n"], rep(sum(subset), 3L))
})

test_that("h_step_rsp_est gives a readable warning when fitting warnings occur", {
  dta_simple <- get_simple()
  vars <- list(
    arm = "armcd",
    biomarker = "age",
    response = "rsp",
    covariates = "time",
    strata = "stage"
  )
  age_vals <- c(20, 30, 40)
  # We increase `degree` such that model becomes very complex and hence fitting warnings occur.
  form <- h_step_rsp_formula(
    variables = vars,
    control = c(control_logistic(), control_step(degree = 4))
  )
  expect_warning(
    h_step_rsp_est(
      formula = form,
      data = dta_simple,
      variables = vars,
      x = age_vals
    ),
    "Fit warnings occurred, please consider using a simpler model"
  )
})
