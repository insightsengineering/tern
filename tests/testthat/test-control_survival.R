test_that("control_coxph works with customized parameters", {
  result <- control_coxph(
    pval_method = "wald", ties = "breslow", conf_level = 0.8
  )
  expected <- list(
    pval_method = "wald",
    ties = "breslow",
    conf_level = 0.8
  )
  expect_identical(result, expected)
})

test_that("control_coxph fails wrong inputs", {
  expect_error(control_coxph(conf_level = 1.1))
  expect_error(control_coxph(pval_method = "bres"))
})


test_that("control_surv_time works with customized parameters", {
  result <- control_surv_time(
    conf_level = 0.8, conf_type = "log-log", quantiles = c(0.3, 0.8)
  )
  expected <- list(
    conf_level = 0.8,
    conf_type = "log-log",
    quantiles = c(0.3, 0.8)
  )
  expect_identical(result, expected)
})

test_that("control_surv_time fails wrong inputs", {
  expect_error(control_surv_time(conf_level = 1.1))
  expect_error(control_surv_time(quantiles = 0.8))
})


test_that("control_surv_timepoint works with customized parameters", {
  result <- control_surv_timepoint(
    conf_level = 0.8, conf_type = "log-log", time_point = 9
  )
  expected <- list(
    conf_level = 0.8,
    conf_type = "log-log",
    time_point = 9
  )
  expect_identical(result, expected)
})

test_that("control_surv_timepoint fails wrong inputs", {
  expect_error(control_surv_timepoint(conf_level = 1.5))
  expect_error(control_surv_timepoint(time_point = c(4, 6)))
})
