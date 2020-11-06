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
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
      .labels = c(count_fraction = "Total number of patients with fatal AEs")
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
    ARM = c("A", "A", "A", "A", "A", "A", "B", "B", "B"),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  test_adsl_like <- data.frame(
    SUBJID = as.character(1001:1010),
    ARM = c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"),
    stringsAsFactors = FALSE
  )

  l <- split_cols_by(lyt = NULL, var = "ARM") %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y"),
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      denom = "N_col"
    ) %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
      .labels = c(count_fraction = "Total number of patients with fatal AEs"),
      denom = "N_col"
    )

  result <- build_table(l, test_data, col_count = table(test_adsl_like$ARM))

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
