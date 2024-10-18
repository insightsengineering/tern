testthat::test_that("h_count_cumulative works with healthy input and default arguments", {
  set.seed(1, kind = "Mersenne-Twister")
  x <- c(sample(1:10, 10), NA)

  result <- h_count_cumulative(
    x = x,
    threshold = 5,
    .N_col = length(x)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_count_cumulative works with customized arguments", {
  set.seed(1, kind = "Mersenne-Twister")
  rand <- stats::rnorm(10, 5, 5)
  x <- c(rand[1:5], NA, rand[6:10])

  result <- h_count_cumulative(
    x = x,
    threshold = 5,
    lower_tail = FALSE,
    include_eq = FALSE,
    na.rm = FALSE,
    .N_col = length(x)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_cumulative works with healthy input and default arguments", {
  set.seed(1, kind = "Mersenne-Twister")
  x <- c(sample(1:10, 10), NA)

  result <- s_count_cumulative(
    x = x,
    thresholds = c(4, 7),
    .N_col = length(x)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_cumulative works with customized arguments", {
  set.seed(1, kind = "Mersenne-Twister")
  rand <- stats::rnorm(10, 5, 5)
  x <- c(rand[1:5], NA, rand[6:10])

  result <- s_count_cumulative(
    x = x,
    thresholds = c(4, 7),
    lower_tail = FALSE,
    include_eq = FALSE,
    na.rm = FALSE,
    .N_col = length(x)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_cumulative works with default arguments", {
  set.seed(1, kind = "Mersenne-Twister")
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- basic_table() %>%
    split_cols_by("grp") %>%
    count_cumulative(
      vars = "a",
      thresholds = c(3, 7)
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_cumulative works with customized arguments", {
  set.seed(1, kind = "Mersenne-Twister")
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- basic_table() %>%
    split_cols_by("grp") %>%
    count_cumulative(
      vars = "a",
      thresholds = c(3, 7),
      lower_tail = FALSE,
      include_eq = FALSE,
      na.rm = FALSE
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_cumulative works with denom argument specified", {
  set.seed(1, kind = "Mersenne-Twister")
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    type = factor(sample(c("x", "y"), 11, replace = TRUE)),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- basic_table() %>%
    split_cols_by("grp") %>%
    split_rows_by("type") %>%
    summarize_row_groups(format = "xx.") %>%
    count_cumulative(
      vars = "a",
      thresholds = c(3, 7),
      lower_tail = FALSE,
      include_eq = FALSE,
      na.rm = FALSE,
      denom = "n"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
