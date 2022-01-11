test_that("get_free_cores works correctly", {
  result <- get_free_cores()
  expect_is(result, "integer")
  expect_gte(result, 1)
  expect_lt(result, parallel::detectCores())
})
