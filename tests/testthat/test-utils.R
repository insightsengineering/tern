test_that("as_factor_keep_attributes works correctly for a character vector", {
  foo <- with_label(c("a", "b"), "alphabet")
  result <- expect_warning(
    as_factor_keep_attributes(foo),
    "automatically converting character variable foo to factor"
  )
  expected <- with_label(factor(c("a", "b")), "alphabet")
  expect_identical(result, expected)
})

test_that("as_factor_keep_attributes does not modify a factor at all", {
  foo <- factor(c(1, 2))
  result <- expect_silent(as_factor_keep_attributes(foo))
  expect_identical(result, foo)
})

test_that("f_conf_level works for proportion", {
  result <- f_conf_level(0.95)
  expected <- "95% CI"
  expect_identical(result, expected)
})
test_that("f_conf_level fails for non-proportion input", {
  expect_error(f_conf_level(1.1))
  expect_error(f_conf_level(-1))
})

test_that("make_names works as expected", {
  nams <- c("Any Grade (%)", "Total AE numbers!", "No adverse events ...")
  result <- make_names(nams)
  expected <- c("AnyGrade", "TotalAEnumbers", "Noadverseevents")
  expect_identical(result, expected)
})

test_that("get_covariates works for a character vector", {
  result <- get_covariates(c("a * b", "c"))
  expected <- list(a = "a", b = "b", c = "c")
  expect_identical(result, expected)
})
test_that("get_covariates fails for non-character input", {
  expect_error(get_covariates(c(1, 2)))
  expect_error(get_covariates(factor(c("a", "b", "b"))))
})
