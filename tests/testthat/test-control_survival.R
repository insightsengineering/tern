testthat::test_that("control_coxph works with customized parameters", {
  result <- control_coxph(
    pval_method = "wald", ties = "breslow", conf_level = 0.8
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_coxph fails wrong inputs", {
  testthat::expect_error(control_coxph(conf_level = 1.1))
  testthat::expect_error(control_coxph(pval_method = "bres"))
})

testthat::test_that("control_surv_time works with customized parameters", {
  result <- control_surv_time(
    conf_level = 0.8, conf_type = "log-log", quantiles = c(0.3, 0.8)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_surv_time fails wrong inputs", {
  testthat::expect_error(control_surv_time(conf_level = 1.1))
  testthat::expect_error(control_surv_time(quantiles = 0.8))
  testthat::expect_error(control_surv_time(conf_type = "none"))
})

testthat::test_that("control_surv_timepoint works with customized parameters", {
  result <- control_surv_timepoint(
    conf_level = 0.8, conf_type = "log-log"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_surv_timepoint fails wrong inputs", {
  testthat::expect_error(control_surv_timepoint(conf_level = 1.5))
  testthat::expect_error(control_surv_timepoint(time_point = c(4, 6)))
  testthat::expect_error(control_surv_timepoint(conf_type = "none"))
})
