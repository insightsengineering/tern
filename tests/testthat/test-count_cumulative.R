test_that("h_count_cumulative works with healthy input and default arguments", {
  set.seed(1)
  x <- c(sample(1:10, 10), NA)

  result <- h_count_cumulative(
    x = x,
    threshold = 5,
    .N_col = length(x)
  )
  expected <- c(
    count = 5,
    percent = 5 / 11
  )
  expect_identical(result, expected)
})

test_that("h_count_cumulative works with customized arguments", {
  set.seed(1)
  rand <- rnorm(10, 5, 5)
  x <- c(rand[1:5], NA, rand[6:10])

  result <- h_count_cumulative(
    x = x,
    threshold = 5,
    lower_tail = FALSE,
    include_eq = FALSE,
    na.rm = FALSE,
    .N_col = length(x)
  )
  expected <- c(
    count = 7,
    percent = 7 / 11
  )
  expect_identical(result, expected)
})

test_that("s_count_cumulative works with healthy input and default arguments", {
  set.seed(1)
  x <- c(sample(1:10, 10), NA)

  result <- s_count_cumulative(
    x = x,
    thresholds = c(4, 7),
    .N_col = length(x)
  )
  expected <- list(
    count_percent = c(
      list(`4` = c(count = 4, percent = 4 / 11)),
      list(`7` = c(count = 7, percent = 7 / 11))
    )
  )
  attr(expected$count_percent$`4`, "label") <- "<= 4"
  attr(expected$count_percent$`7`, "label") <- "<= 7"
  expect_equal(result, expected, tolerance = .00001)
})

test_that("s_count_cumulative works with customized arguments", {
  set.seed(1)
  rand <- rnorm(10, 5, 5)
  x <- c(rand[1:5], NA, rand[6:10])

  result <- s_count_cumulative(
    x = x,
    thresholds = c(4, 7),
    lower_tail = FALSE,
    include_eq = FALSE,
    na.rm = FALSE,
    .N_col = length(x)
  )
  expected <- list(
    count_percent = c(
      list(`4` = c(count = 7, percent = 7 / 11)),
      list(`7` = c(count = 5, percent = 5 / 11))
    )
  )
  attr(expected$count_percent$`4`, "label") <- "> 4"
  attr(expected$count_percent$`7`, "label") <- "> 7"
  expect_equal(result, expected, tolerance = .00001)
})

test_that("s_count_nonmissing works with numeric input", {
  set.seed(1)
  x <- c(sample(1:10, 10), NA)

  result <- s_count_nonmissing(x = x)
  expected <- list(n = 10)
  attr(expected$n, "label") <- "n"
  expect_equal(result, expected, tolerance = .00001)
})

test_that("s_count_nonmissing also works with character input", {
  x <- c("a", "b", NA, "c", "d")

  result <- s_count_nonmissing(x = x)
  expected <- list(n = 4)
  attr(expected$n, "label") <- "n"
  expect_equal(result, expected, tolerance = .00001)
})
