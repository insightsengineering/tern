test_that("s_num_patients works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, NA))
  result <- s_num_patients(x = x, .N_col = 5)
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("s_num_patients_content works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = c(10, 15, 10, 17, 8)
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID")
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("summarize_num_patients works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    AGE = c(10, 15, 10, 17, 8, 11, 11, 19, 17)
  )

  # Check with both output
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_num_patients("USUBJID") %>%
    build_table(df, col_counts = c(5L, 4L))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event", "Number of events",
      "A", "(N=5)", "3 (60%)", "4",
      "B", "(N=4)", "3 (75%)", "4"
    ),
    .Dim = c(4L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)

  # Check with number of unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_num_patients("USUBJID", .stats = c("unique")) %>%
    build_table(df, col_counts = c(5L, 4L))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event",
      "A", "(N=5)", "3 (60%)",
      "B", "(N=4)", "3 (75%)"
    ),
    .Dim = c(3L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)

  # Check with number of non-unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_num_patients("USUBJID", .stats = c("nonunique")) %>%
    build_table(df, col_counts = c(5L, 4L))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of events",
      "A", "(N=5)", "4",
      "B", "(N=4)", "4"
    ),
    .Dim = c(3L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})
