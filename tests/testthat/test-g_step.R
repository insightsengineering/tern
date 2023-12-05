dat <- survival::lung
dat$sex <- factor(dat$sex)
vars <- list(
  time = "time",
  event = "status",
  arm = "sex",
  biomarker = "age"
)
step_matrix <- fit_survival_step(
  variables = vars,
  data = dat,
  control = c(control_coxph(), control_step(num_points = 10, degree = 2))
)
step_data <- broom::tidy(step_matrix)

testthat::test_that("g_step works with default settings", {
  g_step <- g_step(step_data)
  vdiffr::expect_doppelganger(title = "g_step", fig = g_step)
})

testthat::test_that("g_step works with custom settings", {
  g_step_custom <- g_step(
    step_data,
    use_percentile = FALSE,
    est = list(col = "blue", lty = 1),
    ci_ribbon = NULL
  )
  vdiffr::expect_doppelganger(title = "g_step_custom", fig = g_step_custom)
})

testthat::test_that("tidy.step works as expected for survival STEP results", {
  result <- broom::tidy(step_matrix)
  testthat::expect_true(tibble::is_tibble(result))

  res <- testthat::expect_silent(names(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(sort(names(attributes(result))))
  testthat::expect_snapshot(res)

  testthat::expect_equal(result[["Hazard Ratio"]], exp(step_matrix[, "loghr"]))
  testthat::expect_equal(result$ci_lower, exp(step_matrix[, "ci_lower"]))
})

testthat::test_that("tidy.step works as expected for response STEP results", {
  dat <- survival::lung
  dat$sex <- factor(dat$sex)
  vars <- list(
    response = "status",
    arm = "sex",
    biomarker = "age"
  )
  step_matrix <- fit_rsp_step(
    variables = vars,
    data = dat,
    control = c(
      control_logistic(response_definition = "I(response == 2)"),
      control_step(num_points = 10, bandwidth = 0.5)
    )
  )
  result <- broom::tidy(step_matrix)
  testthat::expect_true(tibble::is_tibble(result))

  res <- testthat::expect_silent(names(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(sort(names(attributes(result))))
  testthat::expect_snapshot(res)

  testthat::expect_equal(result[["Odds Ratio"]], exp(step_matrix[, "logor"]))
  testthat::expect_equal(result$ci_lower, exp(step_matrix[, "ci_lower"]))
})

testthat::test_that("tidy.step gives expected warnings when there are NAs in y variables", {
  step_matrix <- structure(
    cbind(loghr = c(1, 2), ci_lower = c(NA, 1), ci_upper = c(3, 6)),
    class = c("step", "matrix"),
    control = control_analyze_vars(conf_level = 0.9),
    variables = list(biomarker = "bla")
  )
  suppressWarnings(testthat::expect_warning(
    broom::tidy(step_matrix),
    "Missing values in the point estimate or CI columns"
  ))
  suppressWarnings(testthat::expect_warning(
    broom::tidy(step_matrix),
    "Consider using larger `bandwidth`, less `num_points`"
  ))
})

testthat::test_that("tidy.step gives expected warnings when there are very large values in y variables", {
  step_matrix <- structure(
    cbind(loghr = c(1, 2), ci_lower = c(1e100, 1), ci_upper = c(3, 6)),
    class = c("step", "matrix"),
    control = control_analyze_vars(conf_level = 0.9),
    variables = list(biomarker = "bla")
  )
  suppressWarnings(testthat::expect_warning(
    broom::tidy(step_matrix),
    "Very large absolute values in the point estimate or CI columns"
  ))
  suppressWarnings(testthat::expect_warning(
    broom::tidy(step_matrix),
    "Consider using larger `bandwidth`, less `num_points`"
  ))
})
