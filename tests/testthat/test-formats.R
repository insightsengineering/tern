context("test functions in test-formats.R")

test_that("format_fraction works with healthy inputs", {
  result <- format_fraction(c(num = 2L, denom = 3L))
  expected <- "2/3 (66.7%)"
  expect_identical(result, expected)
})

test_that("format_fraction fails with bad inputs", {
  x <- list(
    c(num = 2, denom = 3),
    c(num = c(1L, 2L, 3L), denom = 5L),
    c(num = NA_integer_, denom = 2L)
  )
  purrr::map(
    x,
    ~expect_error(format_fraction(.))
  )
})
