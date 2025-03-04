testthat::test_that("d_count_missed_doses works as expected", {
  result <- d_count_missed_doses(c(1, 5))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_missed_doses works as expected", {
  result <- s_count_missed_doses(
    x = c(0, 1, 0, 2, 3, 4, 0, 2),
    thresholds = c(2, 5),
    .N_col = 10,
    .N_row = 10
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_missed_doses works as expected", {
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_missed_doses works with denom argument specified", {
  set.seed(1)
  df <- data.frame(
    a = c(sample(1:10, 10), NA),
    type = factor(sample(c("x", "y"), 11, replace = TRUE)),
    grp = factor(c(rep("A", 5), rep("B", 6)), levels = c("A", "B"))
  )

  result <- basic_table() %>%
    split_cols_by("grp") %>%
    split_rows_by("type") %>%
    count_missed_doses(
      "a",
      thresholds = c(3, 7),
      var_labels = "Missed Doses",
      denom = "n"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
