testthat::test_that("s_count_abnormal_by_baseline works with healthy input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BNRIND = factor(c("LOW", "NORMAL", "NORMAL", "HIGH")),
    stringsAsFactors = FALSE
  )

  # Check with LOW abnormality.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "LOW"
  )
  expected <- list(fraction = list(
    "not_abnormal" = with_label(c(num = 1L, denom = 3L), "Not low baseline status"),
    "abnormal" = with_label(c(num = 0L, denom = 1L), "Low baseline status"),
    "total" = with_label(c(num = 1L, denom = 4L), "Total")
  ))

  testthat::expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "HIGH"
  )
  expected <- list(fraction = list(
    "not_abnormal" = with_label(c(num = 1L, denom = 3L), "Not high baseline status"),
    "abnormal" = with_label(c(num = 1L, denom = 1L), "High baseline status"),
    "total" = with_label(c(num = 2L, denom = 4L), "Total")
  ))
  testthat::expect_identical(result, expected)
})

testthat::test_that("s_count_abnormal_by_baseline also works with tibble and custom arguments", {
  df <- dplyr::as_tibble(
    data.frame(
      myid = as.character(c(1:6)),
      myrange = factor(c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", "NORMAL")),
      mybase = factor(c("NORMAL", "LOW", "NORMAL", "HIGH", "NORMAL", "NORMAL")),
      stringsAsFactors = FALSE
    )
  )

  # Check with LOW abnormality.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "myrange",
    abnormal = "LOW",
    variables = list(id = "myid", baseline = "mybase")
  )
  expected <- list(fraction = list(
    "not_abnormal" = with_label(c(num = 2L, denom = 5L), "Not low baseline status"),
    "abnormal" = with_label(c(num = 0L, denom = 1L), "Low baseline status"),
    "total" = with_label(c(num = 2L, denom = 6L), "Total")
  ))
  testthat::expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "myrange",
    abnormal = "HIGH",
    variables = list(id = "myid", baseline = "mybase")
  )
  expected <- list(fraction = list(
    "not_abnormal" = with_label(c(num = 1L, denom = 5L), "Not high baseline status"),
    "abnormal" = with_label(c(num = 0L, denom = 1L), "High baseline status"),
    "total" = with_label(c(num = 1L, denom = 6L), "Total")
  ))
  testthat::expect_identical(result, expected)
})

testthat::test_that("count_abnormal_by_baseline throws warning with character var", {
  df <- dplyr::as_tibble(
    data.frame(
      myid = as.character(c(1:6)),
      myrange = c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", NA),
      mybase = factor(c("NORMAL", "LOW", "NORMAL", "HIGH", "NORMAL", "NORMAL")),
      stringsAsFactors = FALSE
    )
  )

  # Check with LOW abnormality.
  result <- testthat::expect_warning(
      s_count_abnormal_by_baseline(
        df = df,
        .var = "myrange",
        abnormal = "LOW",
        variables = list(id = "myid", baseline = "mybase")
      )
    )
  expected <- list(fraction = list(
    "not_abnormal" = with_label(c(num = 2L, denom = 4L), "Not low baseline status"),
    "abnormal" = with_label(c(num = 0L, denom = 1L), "Low baseline status"),
    "total" = with_label(c(num = 2L, denom = 5L), "Total")
  ))
  testthat::expect_identical(result, expected)
})

testthat::test_that("count_abnormal_by_baseline works with default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BNRIND = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
  )
  result <- basic_table() %>%
    count_abnormal_by_baseline(var = "ANRIND", abnormal = c(Low = "LOW", High = "HIGH")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(c(
    "", "Low", "Not low baseline status", "Low baseline status",
    "Total", "High", "Not high baseline status", "High baseline status",
    "Total", "all obs", "", "1/3 (33.3%)", "0/1", "1/4 (25%)",
    "", "1/2 (50%)", "1/2 (50%)", "2/4 (50%)"
  ),
  .Dim = c(9L, 2L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("count_abnormal_by_baseline works with custom arguments", {
  df2 <- data.frame(
    ID = as.character(c(1, 2, 3, 4)),
    RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BLRANGE = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
  )
  result <- basic_table() %>%
    count_abnormal_by_baseline(
      var = "RANGE",
      abnormal = c(Low = "LOW", High = "HIGH"),
      variables = list(id = "ID", baseline = "BLRANGE"),
      .formats = c(fraction = "xx / xx")
    ) %>%
    build_table(df2)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "Low", "Not low baseline status", "Low baseline status",
      "Total", "High", "Not high baseline status", "High baseline status",
      "Total", "all obs", "", "1 / 3", "0 / 1", "1 / 4", "", "1 / 2",
      "1 / 2", "2 / 4"),
    .Dim = c(9L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("s_count_abnormal_by_baseline gives error for NA input", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BNRIND = factor(c("LOW", "NORMAL", "NORMAL", NA)),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(s_count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "LOW"
  ))

  df <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", NA)),
    BNRIND = factor(c("LOW", "NORMAL", "NORMAL", "HIGH")),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(s_count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "LOW"
  ))
})
