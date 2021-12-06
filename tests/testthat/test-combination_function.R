testthat::test_that("CombinationFunction class can be used with standard constructor", {
  fun <- function(x) {
    x == 5
  }
  comb_fun <- new("CombinationFunction", fun)
  testthat::expect_is(comb_fun, "CombinationFunction")
  testthat::expect_identical(fun(5), comb_fun(5))
  testthat::expect_identical(fun(3), comb_fun(3))
})

testthat::test_that("CombinationFunction custom constructor works", {
  fun <- function(x) {
    x == 5
  }
  result <- CombinationFunction(fun)
  expected <- new("CombinationFunction", fun)
  testthat::expect_identical(result, expected)
})

testthat::test_that("CombinationFunction `&` method works", {
  a <- CombinationFunction(function(test) {
    test > 3
  })
  b <- CombinationFunction(function(test) {
    test < 10
  })
  c <- a & b
  testthat::expect_is(c, "CombinationFunction")
  testthat::expect_identical(a(5) & b(5), c(5))
  testthat::expect_identical(a(1) & b(1), c(1))
})

testthat::test_that("CombinationFunction `|` method works", {
  a <- CombinationFunction(function(test) {
    test > 3
  })
  b <- CombinationFunction(function(test) {
    test < 10
  })
  c <- a | b
  testthat::expect_is(c, "CombinationFunction")
  testthat::expect_identical(a(1) | b(1), c(1))
  testthat::expect_identical(a(11) | b(11), c(11))
})

testthat::test_that("CombinationFunction `!` method works", {
  a <- CombinationFunction(function(test) {
    test > 3
  })
  b <- !a
  testthat::expect_is(b, "CombinationFunction")
  testthat::expect_identical(!a(1), b(1))
  testthat::expect_identical(!a(10), b(10))
})

testthat::test_that("CombinationFunction chain of logical combinations works", {
  a <- CombinationFunction(function(test) {
    test > 3
  })
  b <- CombinationFunction(function(test) {
    test < 10
  })
  c <- CombinationFunction(function(test) {
    test %% 2 == 0
  })
  d <- a & (b | c)
  testthat::expect_is(d, "CombinationFunction")
  testthat::expect_identical(a(5) & (b(5) | c(5)), d(5))
  testthat::expect_identical(a(12) & (b(12) | c(12)), d(12))
})
