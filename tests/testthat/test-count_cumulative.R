library(random.cdisc.data)
library(dplyr)

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
    fraction = 5 / 11
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
    fraction = 7 / 11
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
    count_fraction = c(
      list(`4` = c(count = 4, fraction = 4 / 11)),
      list(`7` = c(count = 7, fraction = 7 / 11))
    )
  )
  attr(expected$count_fraction$`4`, "label") <- "<= 4"
  attr(expected$count_fraction$`7`, "label") <- "<= 7"
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
    count_fraction = c(
      list(`4` = c(count = 7, fraction = 7 / 11)),
      list(`7` = c(count = 5, fraction = 5 / 11))
    )
  )
  attr(expected$count_fraction$`4`, "label") <- "> 4"
  attr(expected$count_fraction$`7`, "label") <- "> 7"
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

test_that("count_cumulative works with default arguments", {
  set.seed(1)
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- split_cols_by(lyt = NULL, "grp") %>%
    count_cumulative(
      vars = "a",
      thresholds = c(3, 7)
    ) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "a", "  <= 3", "  <= 7",
      "A", "", "2 (40%)", "4 (80%)",
      "B", "", "1 (16.7%)", "3 (50%)"
    ),
    .Dim = 4:3
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("count_cumulative works with customized arguments", {
  set.seed(1)
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- split_cols_by(lyt = NULL, "grp") %>%
    count_cumulative(
      vars = "a",
      thresholds = c(3, 7),
      lower_tail = FALSE,
      include_eq = FALSE,
      na.rm = FALSE
    ) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "a", "  > 3", "  > 7",
      "A", "", "3 (60%)", "1 (20%)",
      "B", "", "4 (66.7%)", "2 (33.3%)"
    ),
    .Dim = 4:3
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("count_missed_doses works with default arguments", {
  set.seed(1)
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- split_cols_by(lyt = NULL, "grp") %>%
    count_missed_doses(
      var = "a",
      thresholds = c(3, 7)
    ) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Missed Doses", "n", "  At least 3 missed doses", "  At least 7 missed doses",
      "A", "", "5", "3 (60%)", "2 (40%)",
      "B", "", "5", "5 (83.3%)", "2 (33.3%)"
    ),
    .Dim = c(5L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})
