testthat::test_that("h_format_row returns the correct dataframe", {
  format <- c(mean = "xx.x", mean_ci = "(xx.xx, xx.xx)")
  labels <- c(mean = "My Mean")

  # test 1
  mean_ci <- c(48, 51)
  x <- list(mean = 50, mean_ci = mean_ci)
  result <- h_format_row(x, format, labels)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # test 2
  attr(mean_ci, "label") <- "Mean 95% CI"
  x <- list(mean = 50, mean_ci = mean_ci)
  result <- h_format_row(x, format, labels)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
