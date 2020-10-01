test_that("format_fraction works with healthy inputs", {
  result <- format_fraction(c(num = 2L, denom = 3L))
  expected <- "2/3 (66.7%)"
  expect_identical(result, expected)
})

test_that("format_fraction works with 0 numerator input", {
  result <- format_fraction(c(num = 0L, denom = 3L))
  expected <- "0/3"
  expect_identical(result, expected)
})

test_that("format_count_fraction works with healthy inputs", {
  result <- format_count_fraction(c(2, 0.6667))
  expected <- "2 (66.7%)"
  expect_identical(result, expected)
})

test_that("format_count_fraction works with count of 0", {
  result <- format_count_fraction(c(0, 0))
  expected <- "0"
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

test_that("format_xx works with easy inputs", {
  test <- list(c(1.658, 0.5761), c(1e1, 785.6))
  z <- format_xx("xx (xx.x)")
  result <- sapply(test, z)
  expected <- c("2 (0.6)", "10 (785.6)")
  expect_identical(result, expected)
})
