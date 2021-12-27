testthat::test_that("format_fraction works with healthy inputs", {
  result <- format_fraction(c(num = 2, denom = 3))
  expected <- "2/3 (66.7%)"
  testthat::expect_identical(result, expected)
})

testthat::test_that("format_fraction works with 0 numerator input", {
  result <- format_fraction(c(num = 0L, denom = 3L))
  expected <- "0/3"
  testthat::expect_identical(result, expected)
})

testthat::test_that("format_count_fraction works with healthy inputs", {
  result <- format_count_fraction(c(2, 0.6667))
  expected <- "2 (66.7%)"
  testthat::expect_identical(result, expected)
})

testthat::test_that("format_count_fraction works with count of 0", {
  result <- format_count_fraction(c(0, 0))
  expected <- "0"
  testthat::expect_identical(result, expected)
})

testthat::test_that("format_fraction fails with bad inputs", {
  x <- list(
    c(num = c(1L, 2L, 3L), denom = 5L),
    c(num = NA_integer_, denom = 2L)
  )
  purrr::map(
    x,
    ~ testthat::expect_error(format_fraction(.))
  )
})

testthat::test_that("format_xx works with easy inputs", {
  test <- list(c(1.658, 0.5761), c(1e1, 785.6))
  z <- format_xx("xx (xx.x)")
  result <- sapply(test, z)
  expected <- c("2 (0.6)", "10 (785.6)")
  testthat::expect_identical(result, expected)
})

testthat::test_that("format_fraction_threshold works with easy inputs", {
  test <- list(c(100, 0.1), c(10, 0.01), c(0, 0))
  format_fun <- format_fraction_threshold(0.02)
  result <- sapply(test, format_fun)
  expected <- c("10", "<2", "0")
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_get_format_threshold works with easy inputs", {

  # Test default.
  result <- h_get_format_threshold()
  expected <- list(
    threshold = c(low = 0.01, high = 999.99),
    format_string = c(low = "<0.01", high = ">999.99")
  )
  testthat::expect_identical(result, expected)

  # Test non-default value.
  result <- h_get_format_threshold(1L)
  expected <- list(
    threshold = c(low = 0.1, high = 999.9),
    format_string = c(low = "<0.1", high = ">999.9")
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_format_threshold works with easy inputs", {
  test <- c(0.782, 0.127, Inf, 0, 0.009, NA)
  result <- sapply(test, h_format_threshold)
  expected <- c("0.78", "0.13", ">999.99", "0.00", "<0.01", NA)

  testthat::expect_identical(result, expected)
})

testthat::test_that("format_extreme_values works with easy inputs", {
  test <- c(0.127, Inf, 0, 0.009, NA)
  format_fun <- format_extreme_values(2L)
  result <- sapply(test, format_fun)
  expected <- c("0.13", ">999.99", "0.00", "<0.01", NA)
  testthat::expect_identical(result, expected)
})

testthat::test_that("format_extreme_values_ci works with easy inputs", {
  test <- list(c(0.127, Inf), c(0, 0.009), c(NA, NA))
  format_fun <- format_extreme_values_ci(2L)
  result <- sapply(test, format_fun)
  expected <- c("(0.13, >999.99)", "(0.00, <0.01)", "(NA, NA)")
  testthat::expect_identical(result, expected)
})
