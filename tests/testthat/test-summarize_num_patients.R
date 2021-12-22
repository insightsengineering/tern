testthat::test_that("s_num_patients works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, NA))
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5)
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients works as expected with empty input", {
  x <- as.character()
  result <- s_num_patients(x = x, labelstr = "", .N_col = 0)
  expected <- list(
    unique = c(0, 0),
    nonunique = 0,
    unique_count = with_label(0, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients_content works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = c(10, 15, 10, 17, 8)
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID")
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("summarize_num_patients works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    AGE = c(10, 15, 10, 17, 8, 11, 11, 19, 17)
  )

  # Check with both output
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID") %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event", "Number of events", " (n)",
      "A", "(N=5)", "3 (60%)", "4", "3",
      "B", "(N=4)", "3 (75%)", "4", "3"
    ),
    .Dim = c(5L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Check with number of unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", .stats = c("unique")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event",
      "A", "(N=5)", "3 (60%)",
      "B", "(N=4)", "3 (75%)"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Check with number of non-unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", .stats = c("nonunique")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of events",
      "A", "(N=5)", "4",
      "B", "(N=4)", "4"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Check with number of unique patients count only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", .stats = c("unique_count")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", " (n)",
      "A", "(N=5)", "3",
      "B", "(N=4)", "3"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("s_num_patients count_by works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, 1))
  y <- as.character(c(6, 7, 8, 9, 6))
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, count_by = y)
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients count_by with missing works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, NA))
  y <- as.character(c(6, 7, 8, 9, 6))
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, count_by = y)
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients count_by with missing case 2 works as expected with healthy input", {
  x <- as.character(c(1, 2, 1, 4, 1))
  y <- as.character(c(6, 7, NA, 9, 6))
  result <- s_num_patients(x = x, labelstr = "", .N_col = 5, count_by = y)
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 3,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients_content with count_by works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = as.character(c(10, 15, 10, 17, 8))
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID", count_by = "AGE")
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 3,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients_content with count_by case 2 works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA)),
    AGE = as.character(c(10, 15, 11, 17, 8))
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID", count_by = "AGE")
  expected <- list(
    unique = c(3.0, 0.6),
    nonunique = 4,
    unique_count = with_label(3, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("s_num_patients_content with count_by trivial cases, identical to without count_by", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, 9)),
    AGE = as.character(c(10, 15, 11, 17, 8))
  )
  result <- s_num_patients_content(df = df, .N_col = 5, .var = "USUBJID", count_by = "USUBJID")
  expected <- list(
    unique = c(4.0, 0.8),
    nonunique = 4,
    unique_count = with_label(4, " (n)")
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("summarize_num_patients with count_by works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    BY = as.character(c(10, 15, 10, 17, 8, 11, 11, 19, 17))
  )

  # Check with both output
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY") %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event", "Number of events", " (n)",
      "A", "(N=5)", "3 (60%)", "3", "3",
      "B", "(N=4)", "3 (75%)", "3", "3"
    ),
    .Dim = c(5L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Check with number of unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY", .stats = c("unique")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event",
      "A", "(N=5)", "3 (60%)",
      "B", "(N=4)", "3 (75%)"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Check with number of non-unique patients only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY", .stats = c("nonunique")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of events",
      "A", "(N=5)", "3",
      "B", "(N=4)", "3"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)

  # Check with number of unique patients count only
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY", .stats = c("unique_count")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", " (n)",
      "A", "(N=5)", "3",
      "B", "(N=4)", "3"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_num_patients with count_by different
                    combinations works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
    ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    BY = as.character(c(10, 15, 11, 17, 8, 11, 11, 19, 17))
  )

  # Check with both output
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients("USUBJID", count_by = "BY") %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Number of patients with at least one event", "Number of events", " (n)",
      "A", "(N=5)", "3 (60%)", "4", "3",
      "B", "(N=4)", "3 (75%)", "3", "3"
    ),
    .Dim = c(5L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
