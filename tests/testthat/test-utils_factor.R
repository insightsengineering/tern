test_that("combine_vectors works correctly", {
  x <- c(1:3)
  y <- c(4:6)
  result <- combine_vectors(x, y)

  expected <- list(
    c(1, 4),
    c(2, 5),
    c(3, 6)
  )

  expect_equal(result, expected)
})

test_that("as_factor_keep_attributes works correctly for a character vector", {
  foo <- with_label(c("a", "b"), "alphabet")
  result <- expect_warning(
    as_factor_keep_attributes(foo),
    "automatically converting character variable foo to factor"
  )
  expected <- with_label(factor(c("a", "b")), "alphabet")
  expect_identical(result, expected)
})

test_that("as_factor_keep_attributes converts empty strings for a character vector", {
  foo <- with_label(c("a", "", "b"), "alphabet")
  result <- expect_warning(
    as_factor_keep_attributes(foo, na_level = "missing"),
    "automatically converting character variable foo to factor"
  )
  expected <- with_label(factor(c("a", "missing", "b"), levels = c("a", "b", "missing")), "alphabet")
  expect_identical(result, expected)
})

test_that("as_factor_keep_attributes shows correct name of vector in warning", {
  foo <- with_label(c("a", "b"), "alphabet")
  expect_warning(
    as_factor_keep_attributes(foo, x_name = "FOO"),
    "automatically converting character variable FOO to factor"
  )
})

test_that("as_factor_keep_attributes does not modify a factor at all", {
  foo <- factor(c(1, 2))
  result <- expect_silent(as_factor_keep_attributes(foo))
  expect_identical(result, foo)
})


test_that("fct_discard works as expected", {
  x <- factor(c("a", "b", "c"))
  result <- fct_discard(x, "b")
  expected <- factor(c("a", "c"))
  expect_identical(result, expected)
})

test_that("fct_explicit_na_if works as expected", {
  x <- factor(c("a", "b", NA))
  cond <- c(TRUE, FALSE, FALSE)
  result <- fct_explicit_na_if(x, cond)
  expected <- factor(c("<Missing>", "b", "<Missing>"), levels = c("a", "b", "<Missing>"))
  expect_identical(result, expected)
})

test_that("fct_collapse_only works as expected", {
  x <- factor(c("a", "b", "c", "d"))
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("c", "d"))
  expected <- factor(c("TRT", "CTRL", "CTRL"), levels = c("TRT", "CTRL"))
  expect_identical(result, expected)
})
