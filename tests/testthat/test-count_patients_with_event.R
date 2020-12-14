library(dplyr)

test_that("s_count_patients_with_event handles NA", {

  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002"),
    TRTEMFL = c("Y", "", "", "NA", "", ""),
    stringsAsFactors = FALSE
  )

  result <- s_count_patients_with_event(
    test_data,
    .var = "SUBJID",
    filters = c("TRTEMFL" = "Y")
  )

  expected <- list(n = 2L, count = 1L, count_fraction = c(1.0, 0.5))
  expect_identical(result, expected)
})

test_that("s_count_patients_with_event handles multiple columns", {

  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  result <- s_count_patients_with_event(
    test_data,
    .var = "SUBJID",
    filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
  )

  expected <- list(n = 3L, count = 1L, count_fraction = c(1.0, 0.3333333))

  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("count_patients_with_event works as expected", {

  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = c("A", "A", "A", "A", "A", "A", "B", "B", "B"),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  test_adsl_like <- test_data[!duplicated(test_data["SUBJID"]), ]


  l <- split_cols_by(lyt = NULL, var = "ARM") %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y"),
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      table_names = "total_pts_ae"
    ) %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
      .labels = c(count_fraction = "Total number of patients with fatal AEs"),
      table_names = "total_pts_fatal_ae"
    )

  result <- build_table(l, test_data, col_count = table(test_adsl_like$ARM))

  result <- to_string_matrix(result)


  expected <- structure(
    c("", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=2)",
      "1 (50%)", "0 (0%)", "B", "(N=1)", "1 (100%)", "1 (100%)"
    ),
    .Dim = c(4L, 3L)
  )

  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("count_patients_with_event works as expected for different column count", {

  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  test_adsl_like <- data.frame(
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  col_counts <- table(test_adsl_like$ARM)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y"),
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      denom = "N_col",
      table_names = "total_pts_ae"
    ) %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
      .labels = c(count_fraction = "Total number of patients with fatal AEs"),
      denom = "N_col",
      table_names = "total_pts_fatal_ae"
    )

  result <- build_table(lyt, df = test_data, col_counts = col_counts)

  result <- to_string_matrix(result)
  expected <- structure(
    c("", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=6)",
      "1 (16.67%)", "0 (0%)", "B", "(N=4)", "1 (25%)", "1 (25%)"
    ),
    .Dim = c(4L, 3L)
  )
  expect_identical(result, expected)
})

test_that("count_patients_with_flags works as expected", {

  test_data <- tibble(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL")
  )
  test_data <- test_data %>%
    mutate(
      flag1 = TRTEMFL == "Y",
      flag2 = TRTEMFL == "Y" & AEOUT == "FATAL",
    ) %>%
    var_relabel(
      SUBJID = "A",
      ARM = "B",
      TRTEMFL = "C",
      AEOUT = "D",
      flag1 = "Total number of patients with at least one adverse event",
      flag2 = "Total number of patients with fatal AEs"
    )

  test_adsl_like <- tibble(
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  col_counts <- table(test_adsl_like$ARM)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_patients_with_flags(
      "SUBJID",
      flag_variables = var_labels(test_data[, c("flag1", "flag2")]),
    )

  result <- build_table(lyt, df = test_data, col_counts = col_counts)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=6)",
      "1 (16.67%)", "0 (0%)", "B", "(N=4)", "1 (25%)", "1 (25%)"
    ),
    .Dim = c(4L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("count_patients_with_flags works as expected when specifying table_names", {

  test_data <- tibble(
    USUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL")
  )
  test_data <- test_data %>%
    mutate(
      flag1 = TRTEMFL == "Y",
      flag2 = TRTEMFL == "Y" & AEOUT == "FATAL",
    ) %>%
    var_relabel(
      flag1 = "Total number of patients with at least one adverse event",
      flag2 = "Total number of patients with fatal AEs"
    )

  test_adsl_like <- tibble(
    USUBJID = as.character(1001:1010),
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  col_counts <- table(test_adsl_like$ARM)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_patients_with_flags(
      "SUBJID",
      flag_variables = var_labels(test_data[, c("flag1", "flag2")]),
      table_names = paste0("SUBJID", c("flag1", "flag2"))
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(test_data[, c("flag1", "flag2")]),
      table_names = paste0("USUBJID", c("flag1", "flag2"))
    )

  result <- build_table(lyt, df = test_data, col_counts = col_counts)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=6)", "1 (16.67%)",
      "0 (0%)", "1 (16.67%)", "0 (0%)", "B", "(N=4)", "1 (25%)", "1 (25%)",
      "1 (25%)", "1 (25%)"),
    .Dim = c(6L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("s_count_patients_with_event works with factor filters", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    AEOUT = c(
      "RECOVERING/RESOLVING", "RECOVERED/RESOLVED", "RECOVERING/RESOLVING",
      "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE", "UNKNOWN",
      "FATAL", "RECOVERED/RESOLVED WITH SEQUELAE", "FATAL"
    ),
    stringsAsFactors = TRUE
  )
  result <- s_count_patients_with_event(
    test_data,
    .var = "SUBJID",
    filters = c("AEOUT" = "FATAL")
  )
  expected <- list(n = 3, count = 1, count_fraction = c(1.0000000, 0.3333333))
  expect_equal(result, expected, tolerance = 1e-7)
})
