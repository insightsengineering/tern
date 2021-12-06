
raw_data <- local({
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
})

testthat::test_that("h_append_grade_groups works with valid input", {
  result <- h_append_grade_groups(
    list(
      "Any Grade" = as.character(1:5),
      "Grade 1-2" = c("1", "2"),
      "Grade 3-4" = c("3", "4")
    ),
    list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50),
  )

  expected <- list(
    "Any Grade" = 150L,
    "Grade 1-2" = 30L,
    "1" = 10L,
    "2" = 20L,
    "Grade 3-4" = 70L,
    "3" = 30L,
    "4" = 40L,
    "5" = 50L
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("h_append_grade_groups works with valid input with revers order and one-element grade groups", {
  result <- h_append_grade_groups(
    list(
      "Any Grade" = as.character(5:1),
      "Grade A" = "5",
      "Grade B" = c("4", "3")
    ),
    list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50),
  )

  expected <- list(
    "Any Grade" = 150L,
    "Grade A" = 50L,
    "Grade B" = 70L,
    "4" = 40L,
    "3" = 30L,
    "2" = 20L,
    "1" = 10L
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("s_count_occurrences_by_grade works with valid input and default arguments for grade", {
  df <- raw_data
  result <- s_count_occurrences_by_grade(df = df, .var = "AETOXGR", .N_col = 10)

  expected <- list(count_fraction = c(
    "1" = list(c(2L, 0.2)),
    "2" = list(c(2L, 0.2)),
    "3" = list(c(2L, 0.2)),
    "4" = list(c(0L, 0)),
    "5" = list(c(0, 0))
  ))

  testthat::expect_equal(result, expected)

  # Test with empty input.
  df_empty <- raw_data %>%
    dplyr::filter(ARM == "D")

  result <- s_count_occurrences_by_grade(df = df_empty, .var = "AETOXGR", .N_col = 10)

  expected <- list(count_fraction = c(
    "1" = list(c(0, 0)),
    "2" = list(c(0, 0)),
    "3" = list(c(0, 0)),
    "4" = list(c(0, 0)),
    "5" = list(c(0, 0))
  ))

  testthat::expect_equal(result, expected)

})

testthat::test_that("s_count_occurrences_by_grade works with valid input for grade grouping", {
  df <- raw_data
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
  testthat::expect_equal(result, expected)

  # Test with empyt input.
  df_empty <- raw_data %>%
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
  testthat::expect_equal(result, expected)

})

testthat::test_that("s_count_occurrences_by_grade works with valid input for intensity and custom arguments", {
  df <- raw_data

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
  testthat::expect_equal(result, expected)

})

testthat::test_that("count_occurrences_by_grade works with default arguments for intensity", {

  df <- raw_data
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

  testthat::expect_identical(result_matrix, expected_matrix)

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

testthat::test_that("count_occurrences_by_grade label works when more than one variables are analyzed", {

  df <- raw_data
  df_adsl <- unique(df[c("ARM", "ARM_EMPTY", "USUBJID")])

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(var = "AESEV") %>%
    count_occurrences_by_grade(var = "AETOXGR", var_labels = "Toxicity Grade") %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "AESEV", "MILD", "MODERATE", "SEVERE",
      "Toxicity Grade", "1", "2", "3", "4", "5",
      "A", "(N=3)", "", "0", "1 (33.3%)", "2 (66.7%)",
      "", "0", "1 (33.3%)", "2 (66.7%)", "0", "0",
      "B", "(N=3)", "", "2 (66.7%)", "1 (33.3%)", "0",
      "", "2 (66.7%)", "1 (33.3%)", "0", "0", "0"
    ),
    .Dim = c(12L, 3L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)

})


testthat::test_that("count_occurrences_by_grade works with custom arguments for grade", {

  df <- raw_data
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

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_occurrences_by_grade works with default arguments for intensity", {

  df <- raw_data
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
  testthat::expect_identical(result_matrix, expected_matrix)

  # Test with empty input.
  df <- raw_data
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

testthat::test_that("summarize_occurrences_by_grade works with custom arguments for grade", {

  df <- raw_data
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
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("count_occurrences_by_grade works with trim_levels_in_group split function", {

  df <- data.frame(
    USUBJID = as.character(1:30),
    ARM = factor(c(rep("ARM A", 15), rep("ARM B", 15)), levels = c("ARM A", "ARM B")),
    SOC = as.factor(rep("SOC1", 30)),
    AETOXGR = factor(c(rep(1, 10), rep(2, 20)), levels = c(1:5))
  )
  df_adsl <- df %>%
    dplyr::select(USUBJID, ARM) %>%
    unique()

  # Define additional grade groupings
  grade_groups <- list(
    "-Any-" = as.character(1:5),
    "Grade 1-2" = c("1", "2"),
    "Grade 3-4" = c("3", "4")
  )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("SOC", split_fun = trim_levels_in_group("AETOXGR")) %>%
    count_occurrences_by_grade(
      var = "AETOXGR",
      grade_groups = grade_groups
    ) %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "SOC1", "-Any-", "Grade 1-2", "1", "2",
      "ARM A", "(N=15)", "", "15 (100%)", "15 (100%)",
      "10 (66.7%)", "5 (33.3%)", "ARM B", "(N=15)", "",
      "15 (100%)", "15 (100%)", "0", "15 (100%)"
    ),
    .Dim = c(7L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
