testthat::test_that("control_logistic works with customized parameters", {
  result <- control_logistic(
    conf_level = 0.9,
    response_definition = "response == 'bla'"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_logistic fails wrong inputs", {
  testthat::expect_error(control_logistic(response_definition = "rsp"))
  testthat::expect_error(control_logistic(conf_level = 95))
})
