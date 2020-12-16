get_test_data_simple <- function() {

  df <- data.frame(
    USUBJID = as.character(c(1:6, 1)),
    USUBJID2 = as.character(c(1:6, 1) * 10),
    AETOXGR = factor(c(1, 2, 3, 1, 1, 2, 3), levels = c(1:5)),
    AESEV = factor(
      c("MILD", "MODERATE", "SEVERE", "MILD", "MILD", "MODERATE", "SEVERE"),
      levels = c("MILD", "MODERATE", "SEVERE")
    ),
    ARM = factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B")),
    ARM_EMPTY =  factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B", "D")),
    BMRKR = factor(rep(c("HIGH", "LOW"), 4)[-1], levels = c("LOW", "HIGH")),
    stringsAsFactors = FALSE
  )

  df
}

test_that("h_append_grade_groups works with valid input", {
  result <- h_append_grade_groups(
    list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    ),
    list("1" = 10L, "2" = 7L, "3" = 2L, "4" = 2L, "5" = 0L)
  )

  expected <- list(
    "Any Grade" = 21L,
    "Grade 1-2" = 17L,
    "1" = 10L,
    "2" = 7L,
    "Grade 3-4" = 4L,
    "3" = 2L,
    "4" = 2L,
    "5" = 0L
  )
  expect_equal(result, expected)
})

test_that("s_count_occurrences_by_grade works with valid input and default arguments for grade", {
  df <- get_test_data_simple()
  result <- s_count_occurrences_by_grade(df = df, .var = "AETOXGR", .N_col = 10)

  expected <- list(count_fraction = c(
    "1" = list(c(2L, 0.2)),
    "2" = list(c(2L, 0.2)),
    "3" = list(c(2L, 0.2)),
    "4" = list(c(0L, 0)),
    "5" = list(c(0, 0))
  ))

  expect_equal(result, expected)

  # Test with empty input.
  df_empty <- get_test_data_simple() %>%
    dplyr::filter(ARM == "D")

  result <- s_count_occurrences_by_grade(df = df_empty, .var = "AETOXGR", .N_col = 10)

  expected <- list(count_fraction = c(
    "1" = list(c(0, 0)),
    "2" = list(c(0, 0)),
    "3" = list(c(0, 0)),
    "4" = list(c(0, 0)),
    "5" = list(c(0, 0))
  ))

  expect_equal(result, expected)

})

test_that("s_count_occurrences_by_grade works with valid input for grade grouping", {
  df <- get_test_data_simple()
  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AETOXGR",
    .N_col = 10,
    grade_groups = list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    )
  )

  expected <- list(count_fraction = c(
    "Any Grade" = list(c(6L, 0.6)),
    "Grade 1-2" = list(c(4L, 0.4)),
    "1" = list(c(2L, 0.2)),
    "2" = list(c(2L, 0.2)),
    "Grade 3-4" = list(c(2L, 0.2)),
    "3" = list(c(2L, 0.2)),
    "4" = list(c(0, 0)),
    "5" = list(c(0, 0))
  ))
  expect_equal(result, expected)

  # Test with empyt input.
  df_empty <- get_test_data_simple() %>%
    dplyr::filter(ARM == "D")

  result <- s_count_occurrences_by_grade(
    df = df_empty,
    .var = "AETOXGR",
    .N_col = 10,
    grade_groups = list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    )
  )

  expected <- list(count_fraction = c(
    "Any Grade" = list(c(0, 0)),
    "Grade 1-2" = list(c(0, 0)),
    "1" = list(c(0, 0)),
    "2" = list(c(0, 0)),
    "Grade 3-4" = list(c(0, 0)),
    "3" = list(c(0, 0)),
    "4" = list(c(0, 0)),
    "5" = list(c(0, 0))
  ))
  expect_equal(result, expected)

})

test_that("s_count_occurrences_by_grade works with valid input for intensity and custom arguments", {
  df <- get_test_data_simple()

  result <- s_count_occurrences_by_grade(
    df = df,
    .var = "AESEV",
    .N_col = 10L,
    id = "USUBJID2",
    grade_groups = list(
      "Any Intensity" = c("MILD", "MODERATE", "SEVERE")
    )
  )

  expected <- list(count_fraction = c(
    "Any Intensity" = list(c(6L, 0.6)),
    "MILD" = list(c(2L, 0.2)),
    "MODERATE" = list(c(2L, 0.2)),
    "SEVERE" = list(c(2L, 0.2))
  ))
  expect_equal(result, expected)

})

test_that("count_occurrences_by_grade works with default arguments for intensity", {

  df <- get_test_data_simple()
  df_adsl <- unique(df[c("ARM", "ARM_EMPTY", "USUBJID")])

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(var = "AESEV") %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "MILD", "MODERATE", "SEVERE", "A", "(N=3)",
      "0", "1 (33.3%)", "2 (66.7%)", "B", "(N=3)", "2 (66.7%)",
      "1 (33.3%)", "0"
    ),
    .Dim = c(5L, 3L)
  )

  expect_identical(result_matrix, expected_matrix)

  # Test with empty column.
  result <- basic_table() %>%
    split_cols_by("ARM_EMPTY") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(var = "AESEV") %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "MILD", "MODERATE", "SEVERE", "A", "(N=3)",
      "0", "1 (33.3%)", "2 (66.7%)", "B", "(N=3)", "2 (66.7%)",
      "1 (33.3%)", "0", "D", "(N=1)", "0", "0", "0"
    ),
    .Dim = 5:4
  )

})

test_that("count_occurrences_by_grade works with custom arguments for grade", {

  df <- get_test_data_simple()
  df_adsl <- unique(df[c("ARM", "ARM_EMPTY", "USUBJID")])

  # Define additional grade groupings
  grade_groups <-  list(
    "-Any-" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups,
      .formats = "xx.xx (xx.xx%)") %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "-Any-", "Grade 1-2", "1", "2", "Grade 3-5",
      "3", "4", "5", "A", "(N=3)", "3 (100%)", "1 (33.33%)", "0 (0%)",
      "1 (33.33%)", "2 (66.67%)", "2 (66.67%)", "0 (0%)", "0 (0%)",
      "B", "(N=3)", "3 (100%)", "3 (100%)", "2 (66.67%)", "1 (33.33%)",
      "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)"
    ),
    .Dim = c(10L, 3L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("summarize_occurrences_by_grade works with default arguments for intensity", {

  df <- get_test_data_simple()
  df_adsl <- data.frame(
    USUBJID = 1:9,
    ARM = rep(c("A", "B"), c(4, 5))
  )

  result <- basic_table() %>%
    add_colcounts() %>%
    split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AESEV",
      .formats = c("count_fraction" = "xx.xx (xx.xx%)")) %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "A", "MILD", "MODERATE", "SEVERE", "B", "MILD",
      "MODERATE", "SEVERE", "all obs", "(N=9)", "", "0 (0%)", "1 (11.11%)",
      "2 (22.22%)", "", "2 (22.22%)", "1 (11.11%)", "0 (0%)"
    ),
    .Dim = c(10L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)

  # Test with empty input.
  df <- get_test_data_simple()
  df_adsl <- data.frame(
    USUBJID = 1:20,
    ARM_EMPTY = rep(c("A", "B"), each = 10)
  )

  result <- basic_table() %>%
    split_cols_by("ARM_EMPTY") %>%
    add_colcounts() %>%
    split_rows_by("BMRKR", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AESEV",
      .formats = c("count_fraction" = "xx.xx (xx.xx%)")) %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "LOW", "MILD", "MODERATE", "SEVERE", "HIGH",
      "MILD", "MODERATE", "SEVERE", "A", "(N=10)", "", "0", "0",
      "2 (20%)", "", "0", "1 (10%)", "0", "B", "(N=10)",
      "", "1 (10%)", "0", "0", "", "1 (10%)", "1 (10%)",
      "0", "D", "(N=10)", "", "0", "0", "0", "",
      "0", "0", "0"
    ),
    .Dim = c(10L, 4L)
  )

})

test_that("summarize_occurrences_by_grade works with custom arguments for grade", {

  df <- get_test_data_simple()
  df_adsl <- data.frame(
    USUBJID = 1:10,
    ARM_EMPTY = rep(c("A", "B"), each = 5)
  )

  # Define additional grade groupings
  grade_groups <-  list(
    "-Any-" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-5" = c("3", "4", "5")
  )

  result <- basic_table() %>%
    add_colcounts() %>%
    split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
    summarize_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "A", "-Any-", "Grade 1-2", "1", "2", "Grade 3-5",
      "3", "4", "5", "B", "-Any-", "Grade 1-2", "1", "2", "Grade 3-5",
      "3", "4", "5", "all obs", "(N=10)", "", "3 (30%)", "1 (10%)",
      "0", "1 (10%)", "2 (20%)", "2 (20%)", "0", "0",
      "", "3 (30%)", "3 (30%)", "2 (20%)", "1 (10%)", "0", "0",
      "0", "0"
    ),
    .Dim = c(20L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})
