test_that("tidy.step works as expected for survival STEP results", {
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
  result <- broom::tidy(step_matrix)
  expect_true(tibble::is_tibble(result))
  expect_named(
    result,
    c("Percentile Center", "Percentile Lower", "Percentile Upper",
      "Interval Center", "Interval Lower", "Interval Upper", "n", "events",
      "Hazard Ratio", "se", "ci_lower", "ci_upper")
  )
  expect_named(attributes(result), c("names", "row.names", "class", "estimate", "biomarker", "ci"))
  expect_equal(result[["Hazard Ratio"]], exp(step_matrix[, "loghr"]))
  expect_equal(result$ci_lower, exp(step_matrix[, "ci_lower"]))
})

test_that("tidy.step works as expected for response STEP results", {
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
  expect_true(tibble::is_tibble(result))
  expect_named(
    result,
    c("Percentile Center", "Percentile Lower", "Percentile Upper",
      "Interval Center", "Interval Lower", "Interval Upper", "n",
      "Odds Ratio", "se", "ci_lower", "ci_upper")
  )
  expect_named(attributes(result), c("names", "row.names", "class", "estimate", "biomarker", "ci"))
  expect_equal(result[["Odds Ratio"]], exp(step_matrix[, "logor"]))
  expect_equal(result$ci_lower, exp(step_matrix[, "ci_lower"]))
})
