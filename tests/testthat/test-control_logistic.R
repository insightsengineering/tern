testthat::test_that("control_logistic works with customized parameters", {
  result <- control_logistic(
    conf_level = 0.9,
    response_definition = "response == 'bla'"
  )
  expected <- list(
    response_definition = "response == 'bla'",
    conf_level = 0.9
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("control_logistic fails wrong inputs", {
  testthat::expect_error(control_logistic(response_definition = "rsp"))
  testthat::expect_error(control_logistic(conf_level = 95))
})
