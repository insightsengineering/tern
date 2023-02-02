testthat::test_that("control_step works with customized parameters", {
  result <- control_step(biomarker = 1:10, use_percentile = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_step fails wrong inputs", {
  testthat::expect_error(control_step(biomarker = "5"))
  testthat::expect_error(control_step(bandwidth = 0))
  testthat::expect_error(control_step(num_points = 1))
})
