testthat::test_that("format_fraction works with healthy inputs", {
  result <- format_fraction(c(num = 2, denom = 3))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_fraction works with 0 numerator input", {
  result <- format_fraction(c(num = 0L, denom = 3L))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_fraction_fixed_dp works with healthy inputs", {
  result <- format_fraction_fixed_dp(c(num = 2, denom = 3))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_fraction_fixed_dp works with whole number percentages", {
  result <- format_fraction_fixed_dp(c(num = 2, denom = 8))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_fraction_fixed_dp works with 0 numerator input", {
  result <- format_fraction_fixed_dp(c(num = 0L, denom = 3L))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction works with healthy inputs", {
  result <- format_count_fraction(c(2, 0.6667))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction works with count of 0", {
  result <- format_count_fraction(c(0, 0))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction works with NA input", {
  result <- format_count_fraction(NA)
  testthat::expect_identical(result, "NA")
})

testthat::test_that("format_count_fraction_fixed_dp works with healthy inputs", {
  result <- format_count_fraction_fixed_dp(c(2, 0.5))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction_fixed_dp works with healthy inputs", {
  result <- format_count_fraction_fixed_dp(c(2, 0.6667))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction_fixed_dp works with count of 0", {
  result <- format_count_fraction_fixed_dp(c(0, 0))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction_lt10 works with healthy inputs", {
  x <- list(c(10, 1), c(19, 0.5183), c(76, 0.996))

  result <- sapply(x, format_count_fraction_lt10)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction_lt10 works with count less than 10", {
  x <- list(c(9, 1), c(1, 0.5), c(7, 0.99))

  result <- sapply(x, format_count_fraction_lt10)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_count_fraction_lt10 works with NA input", {
  result <- format_count_fraction_lt10(NA)
  testthat::expect_identical(result, "NA")
})


testthat::test_that("format_fraction fails with bad inputs", {
  x <- list(
    c(num = c(1L, 2L, 3L), denom = 5L),
    c(num = NA_integer_, denom = 2L)
  )
  for (i in x) {
    testthat::expect_error(format_fraction(i))
  }
})

testthat::test_that("format_xx works with easy inputs", {
  test <- list(c(1.658, 0.5761), c(1e1, 785.6))
  z <- format_xx("xx (xx.x)")
  result <- sapply(test, z)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_sigfig works with easy inputs", {
  test <- list(1.658, 0.5761, 1e-1, 78.6, 1234e-6, 200.00)
  z <- format_sigfig(3)
  result <- sapply(test, z)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_sigfig works with different format types", {
  test <- list(c(1.658, 0.5761), c(1e-1, 78.6), c(1234e-6, 200.00))
  z <- format_sigfig(3, "xx (xx)")
  result <- sapply(test, z)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  test <- list(c(1.658, 0.5761), c(1e-1, 78.6), c(1234e-6, 200.00))
  z <- format_sigfig(3, "xx - xx")
  result <- sapply(test, z)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_fraction_threshold works with easy inputs", {
  test <- list(c(100, 0.1), c(10, 0.01), c(0, 0))
  format_fun <- format_fraction_threshold(0.02)
  result <- sapply(test, format_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_get_format_threshold works with easy inputs", {
  # Test default.
  result <- h_get_format_threshold()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Test non-default value.
  result <- h_get_format_threshold(1L)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_format_threshold works with easy inputs", {
  test <- c(0.782, 0.127, Inf, 0, 0.009, NA)
  result <- sapply(test, h_format_threshold)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_extreme_values works with easy inputs", {
  test <- c(0.127, Inf, 0, 0.009, NA)
  format_fun <- format_extreme_values(2L)
  result <- sapply(test, format_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_extreme_values_ci works with easy inputs", {
  test <- list(c(0.127, Inf), c(0, 0.009), c(NA, NA))
  format_fun <- format_extreme_values_ci(2L)
  result <- sapply(test, format_fun)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("format_extreme_values_ci works with easy inputs", {
  x_todo <- c(0.001, 0.2, 0.0011000, 3, 4)
  res <- c(mean(x_todo[1:3]), sd(x_todo[1:3])) # It can be less
  result <- format_auto(dt_var = x_todo, x_stat = "mean_sd")(x = res)
  testthat::expect_identical(result, "0.06737 (0.11486)")

  result <- format_auto(x_todo, "range")(x = range(x_todo))
  testthat::expect_identical(result, "0.0010 - 4.0000") # Keeps trailing 0s

  # No scientific notation
  no_sc_x <- c(0.0000001, 1)
  testthat::expect_identical(
    format_auto(no_sc_x, "range")(x = no_sc_x),
    "0.0000001 - 1.0000000"
  )

  # More results than formats values and viceversa
  testthat::expect_error(
    format_auto(x_todo, "range")(x = c(1, 2, 3)),
    "Number of inserted values as result \\(3\\)*"
  )
  testthat::expect_error(
    format_auto(x_todo, "range")(x = 1.234),
    "Number of inserted values as result \\(1\\)*"
  )
})
