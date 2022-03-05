testthat::test_that("h_format_row returns the correct dataframe", {
  skip_if_fail_rtables_refactor()

  format <- c(mean = "xx.x", mean_ci = "(xx.xx, xx.xx)")
  labels <- c(mean = "My Mean")

  # test 1
  mean_ci <- c(48, 51)
  x <- list(mean = 50, mean_ci = mean_ci)
  result <- h_format_row(x, format, labels)
  expected <- data.frame("My Mean" = "50", V1 = "(48, 51)", stringsAsFactors = FALSE, check.names = FALSE)
  testthat::expect_identical(result, expected)

  # test 2
  attr(mean_ci, "label") <- "Mean 95% CI"
  x <- list(mean = 50, mean_ci = mean_ci)
  result <- h_format_row(x, format, labels)
  expected <- data.frame("My Mean" = "50", "Mean 95% CI" = "(48, 51)", stringsAsFactors = FALSE, check.names = FALSE)
  testthat::expect_identical(result, expected)
})
