test_that("s_count_nonmissing works with numeric input", {
  set.seed(1)
  x <- c(sample(1:10, 10), NA)

  result <- s_count_nonmissing(x = x)
  expected <- list(n = 10)
  expect_equal(result, expected, tolerance = .00001)
})

test_that("s_count_nonmissing also works with character input", {
  x <- c("a", "b", NA, "c", "d")

  result <- s_count_nonmissing(x = x)
  expected <- list(n = 4)
  expect_equal(result, expected, tolerance = .00001)
})

test_that("d_count_missed_doses works as expected", {
  result <- d_count_missed_doses(c(1, 5))
  expected <- c("At least 1 missed dose", "At least 5 missed doses")
  expect_identical(result, expected)
})

test_that("s_count_missed_doses works as expected", {
  result <- s_count_missed_doses(
    x = c(0, 1, 0, 2, 3, 4, 0, 2),
    thresholds = c(2, 5),
    .N_col = 10
  )
  expected <- list(
    n = 8L,
    count_fraction = list(
      "2" = with_label(c(count = 4, fraction = 0.4), label = "At least 2 missed doses"),
      "5" = with_label(c(count = 0, fraction = 0), label = "At least 5 missed doses")
    )
  )
  expect_identical(result, expected)
})

test_that("count_missed_doses works as expected", {
  set.seed(1)
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- basic_table() %>%
    split_cols_by("grp") %>%
    count_missed_doses(
      "a",
      thresholds = c(3, 7),
      var_labels = "Missed Doses"
    ) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Missed Doses", "n", "At least 3 missed doses", "At least 7 missed doses",
      "A", "", "5", "3 (60%)", "2 (40%)",
      "B", "", "5", "5 (83.3%)", "2 (33.3%)"
    ),
    .Dim = c(5L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})
