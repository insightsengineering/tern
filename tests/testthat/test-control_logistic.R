test_that("control_logistic works with customized parameters", {
  result <- control_logistic(
    conf_level = 0.9,
    response_definition = "response == 'bla'"
  )
  expected <- list(
    response_definition = "response == 'bla'",
    conf_level = 0.9
  )
  expect_identical(result, expected)
})

test_that("control_logistic fails wrong inputs", {
  expect_error(control_logistic(response_definition = "rsp"))
  expect_error(control_logistic(conf_level = 95))
})
