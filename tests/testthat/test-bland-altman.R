testthat::test_that("unequal length vector gives correct error", {
  testthat::expect_error(s_bland_altman(x = 1:5, y = 1:6, 0.95))
})

testthat::test_that("infeasible input gives correct error", {
  testthat::expect_error(s_bland_altman(x = c("a", "b", "c"), y = 1:3, 0.95))
  testthat::expect_error(s_bland_altman(x = 1:3, y = 4:6, 2))
})


testthat::test_that("s_bland_altman works with two vectors", {
  set.seed(1)
  x <- rnorm(20)
  y <- rnorm(20)
  res <- s_bland_altman(x, y, 0.9)
  average <- (x + y) / 2
  difference <- x - y
  expect <- list(
    df = data.frame(average, difference),
    difference_mean = mean(x) - mean(y),
    ci_mean = c(-0.3414723, 0.7354631),
    difference_sd = 1.392664,
    difference_se = 0.3114091,
    upper_agreement_limit = 2.487724,
    lower_agreement_limit = -2.093733,
    agreement_limit_se = 0.5393764,
    upper_agreement_limit_ci = c(1.555070, 3.420377),
    lower_agreement_limit_ci = c(-3.026386, -1.161079),
    t_value = 1.729133,
    n = 20L
  )
  expect_identical(res, expect, tolerance = 1e-5)
})


testthat::test_that("s_bland_altman works with two vectors with NA element in either vectors", {
  set.seed(1)
  x <- rnorm(20)
  y <- rnorm(20)
  x <- c(NA_real_, 2, x, NA_real_)
  y <- c(1, NA_real_, y, 2)
  res <- s_bland_altman(x, y, 0.9)
  average <- (x + y) / 2
  difference <- x - y
  df <- data.frame(na.omit(data.frame(average, difference)), row.names = NULL)
  expect <- list(
    df = df,
    difference_mean = 0.1969954,
    ci_mean = c(-0.3414723, 0.7354631),
    difference_sd = 1.392664,
    difference_se = 0.3114091,
    upper_agreement_limit = 2.487724,
    lower_agreement_limit = -2.093733,
    agreement_limit_se = 0.5393764,
    upper_agreement_limit_ci = c(1.555070, 3.420377),
    lower_agreement_limit_ci = c(-3.026386, -1.161079),
    t_value = 1.729133,
    n = 20L
  )
  expect_identical(res, expect, tolerance = 1e-5)
})
