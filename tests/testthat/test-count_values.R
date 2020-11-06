test_that("s_count_values works for character input without NAs", {
  x <- c("a", "b", "a")

  # Value which is present in vector.
  result <- s_count_values(x, values = "a")
  expected <- list(
    n = 3L,
    count = 2L,
    count_fraction = c(2, 2 / 3)
  )
  expect_identical(result, expected)

  # Value which is not present.
  result <- s_count_values(x, values = "c")
  expected <- list(
    n = 3L,
    count = 0L,
    count_fraction = c(0, 0)
  )
  expect_identical(result, expected)
})

test_that("s_count_values works for character input with NAs", {
  x <- c("a", "b", "a", NA, "c", "b")

  # Default behavior does not count NAs for `n`.
  result <- s_count_values(x, values = "a")
  expected <- list(
    n = 5L,
    count = 2L,
    count_fraction = c(2, 2 / 5)
  )
  expect_identical(result, expected)

  # Count the NAs with `na.rm` argument set.
  result <- s_count_values(x, values = c("bla", "c"), na.rm = FALSE)
  expected <- list(
    n = 6L,
    count = 1L,
    count_fraction = c(1, 1 / 6)
  )
  expect_identical(result, expected)
})

test_that("s_count_values can pass options to s_summary's logical method", {
  x <- c("a", "b", "a")

  result <- s_count_values(x, .N_row = 10, values = "a", denom = "N_row")
  expected <- list(
    n = 3L,
    count = 2L,
    count_fraction = c(2, 2 / 10)
  )
  expect_identical(result, expected)
})

test_that("s_count_values for factor gives same result as for character", {
  x <- c("a", "b", "a")

  result <- s_count_values(factor(x), .N_row = 10, values = "a", denom = "N_row")
  expected <- s_count_values(x, .N_row = 10, values = "a", denom = "N_row")
  expect_identical(result, expected)
})

test_that("s_count_values for factor fails if not all values are levels of the factor", {
  x <- c("a", "b", "a")

  expect_error(s_count_values(factor(x), values = "x"))
  expect_error(s_count_values(factor(x), values = c("a", "x")))
})

test_that("count_values works as expected with a single value", {
  result <- basic_table() %>%
    count_values("Species", values = "setosa") %>%
    build_table(iris)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "setosa", "all obs", "50 (33.33%)"),
    .Dim = c(2L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("count_values works as expected with multiple values and variables", {
  df <- data.frame(
    x = c("a", "b", "a", "c"),
    y = c("b", "a", "a", "f"),
    stringsAsFactors = FALSE
  )
  result <- basic_table() %>%
    count_values(
      c("x", "y"),
      values = c("a", "f")
    ) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "x", "a, f", "y", "a, f", "all obs", "", "2 (50%)",
      "", "3 (75%)"),
    .Dim = c(5L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})
