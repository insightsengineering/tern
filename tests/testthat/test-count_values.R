testthat::test_that("s_count_values works for character input without NAs", {
  x <- c("a", "b", "a")

  # Value which is present in vector.
  result <- s_count_values(x, values = "a")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Value which is not present.
  result <- s_count_values(x, values = "c")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_values works for character input with NAs", {
  x <- c("a", "b", "a", NA, "c", "b")

  # Default behavior does not count NAs for `n`.
  result <- s_count_values(x, values = "a")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Count the NAs with `na.rm` argument set.
  result <- s_count_values(x, values = c("bla", "c"), na.rm = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_values can pass options to s_summary's logical method", {
  x <- c("a", "b", "a")

  result <- s_count_values(x, .N_row = 10, values = "a", denom = "N_row")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_values for factor gives same result as for character", {
  x <- c("a", "b", "a")
  result <- s_count_values(factor(x), .N_row = 10, values = "a", denom = "N_row")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_values for factor gives the same result as for character for values not in factor level", {
  x <- c("a", "b", "a")
  result <- s_count_values(factor(x), values = "x")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_values works as expected with a single value", {
  result <- basic_table() %>%
    count_values("Species", values = "setosa") %>%
    build_table(iris)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_values works as expected with multiple values and variables", {
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_values for logical vector", {
  x <- c(TRUE, FALSE, TRUE)

  # Value which is present in vector.
  result <- s_count_values(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_values for logical vector with NA", {
  x <- c(TRUE, FALSE, TRUE, NA)

  # Value which is present in vector.
  result <- s_count_values(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_values works as expected with multiple values and variables", {
  df <- data.frame(
    x = c(TRUE, FALSE, TRUE, FALSE),
    y = c("b", "a", "a", "f"),
    stringsAsFactors = FALSE
  )
  result <- basic_table() %>%
    count_values(
      "x",
      values = TRUE
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_values works with denom specified", {
  result <- basic_table() %>%
    count_values("Species", values = "setosa", denom = "N_col") %>%
    build_table(iris)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
