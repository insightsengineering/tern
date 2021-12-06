testthat::test_that("control_step works with customized parameters", {
  result <- control_step(biomarker = 1:10, use_percentile = FALSE)
  expected <- list(
    use_percentile = FALSE,
    bandwidth = 2.25,
    degree = 0L,
    num_points = 39L
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("control_step fails wrong inputs", {
  testthat::expect_error(control_step(biomarker = "5"))
  testthat::expect_error(control_step(bandwidth = 0))
  testthat::expect_error(control_step(num_points = 1))
})
