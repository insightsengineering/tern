context("test tabulate_aggregation_functions")

library(rtables)

test_that("`ttest_ci_one_arm` gives same results as `t.test`", {
  # Case 1: No missing values.
  x1 <- c(-12, 14, 20, 5, 8, 7)
  result1 <- ttest_ci_one_arm(x = x1, conf_level = 0.95)
  expected1 <- t.test(x = x1, conf.level = 0.95)$conf.int
  expect_equal(result1, expected1, check.attributes = FALSE)

  # Case 2: Some missing values.
  x2 <- c(-12, 14, NA, 5, NA, 7)
  result2 <- ttest_ci_one_arm(x = x2, conf_level = 0.8)
  expected2 <- t.test(x = x2, conf.level = 0.8)$conf.int
  expect_equal(result2, expected2, check.attributes = FALSE)

  # Case 3: Only 2 values.
  x3 <- c(-12, 14)
  result3 <- ttest_ci_one_arm(x = x3, conf_level = 0.6)
  expected3 <- t.test(x = x3, conf.level = 0.6)$conf.int
  expect_equal(result3, expected3, check.attributes = FALSE)
})

test_that("`ttest_ci_one_arm` returns empty `rcell` if less than 2 values available", {
  # Case 1: All values are missing.
  x1 <- as.numeric(rep(NA, 3))
  result1 <- ttest_ci_one_arm(x1)
  expected1 <- rtables::rcell(" ")
  expect_equal(result1, expected1, check.attributes = FALSE)

  # Case 2: Only one value is available.
  x2 <- as.numeric(c(NA, 2))
  result2 <- ttest_ci_one_arm(x2)
  expected2 <-  rtables::rcell(" ")
  expect_equal(result2, expected2, check.attributes = FALSE)
})

test_that("`ttest_ci_one_arm` returns constant if `x` is constant", {
  x <- c(-12, -12, -12)
  result <- ttest_ci_one_arm(x)
  expected <- c(-12, -12)
  expect_equal(result, expected, check.attributes = FALSE)
})
