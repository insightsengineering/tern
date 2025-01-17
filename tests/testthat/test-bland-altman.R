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


testthat::test_that("s_bland_altman works with default settings", {
  x <- c(
    -0.62645381, 0.18364332, -0.83562861, 1.59528080, 0.32950777, -0.82046838,
    0.48742905, 0.73832471, 0.57578135, -0.30538839, 1.51178117, 0.38984324,
    -0.62124058, -2.21469989, 1.12493092, -0.04493361, -0.01619026, 0.94383621,
    0.82122120, 0.59390132
  )
  y <- c(
    0.91897737, 0.78213630, 0.07456498, -1.98935170, 0.61982575, -0.05612874,
    -0.15579551, -1.47075238, -0.47815006, 0.41794156, 1.35867955, -0.10278773,
    0.38767161, -0.05380504, -1.37705956, -0.41499456, -0.39428995, -0.05931340,
    1.10002537, 0.76317575
  )
  result <- s_bland_altman(x, y, 0.9)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})


testthat::test_that("g_bland_altman works with default settings", {
  x <- c(
    -0.62645381, 0.18364332, -0.83562861, 1.59528080, 0.32950777, -0.82046838,
    0.48742905, 0.73832471, 0.57578135, -0.30538839, 1.51178117, 0.38984324,
    -0.62124058, -2.21469989, 1.12493092, -0.04493361, -0.01619026, 0.94383621,
    0.82122120, 0.59390132
  )
  y <- c(
    0.91897737, 0.78213630, 0.07456498, -1.98935170, 0.61982575, -0.05612874,
    -0.15579551, -1.47075238, -0.47815006, 0.41794156, 1.35867955, -0.10278773,
    0.38767161, -0.05380504, -1.37705956, -0.41499456, -0.39428995, -0.05931340,
    1.10002537, 0.76317575
  )
  conf_level <- 0.9

  testthat::expect_silent(g_bland_altman_res <- withr::with_options(
    opts_partial_match_old,
    g_bland_altman(x, y, conf_level = conf_level)
  ))

  expect_snapshot_ggplot(title = "g_bland_altman", fig = g_bland_altman_res, width = 10, height = 8)
})
