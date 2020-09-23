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
