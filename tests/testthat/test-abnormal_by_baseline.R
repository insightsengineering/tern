test_that("count_abnormal_by_baseline works with healthy input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BNRIND = factor(c("LOW", "NORMAL", "NORMAL", NA)),
    stringsAsFactors = FALSE
  )

  # Check with LOW abnormality.
  result <- count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "LOW"
  )
  expected <- list(
    "not_abnormal" = c(num = 1L, denom = 2L),
    "abnormal" = c(num = 0L, denom = 1L),
    "total" = c(num = 1L, denom = 4L)
  )

  expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "HIGH"
  )
  expected <- list(
    "not_abnormal" = c(num = 1L, denom = 3L),
    "abnormal" = c(num = 0L, denom = 0L),
    "total" = c(num = 2L, denom = 4L)
  )
  expect_identical(result, expected)
})

test_that("count_abnormal_by_baseline also works with tibble and custom arguments", {
  df <- dplyr::as_tibble(
    data.frame(
      myid = as.character(c(1:6)),
      myrange = factor(c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", NA)),
      mybase = factor(c("NORMAL", "LOW", "NORMAL", "HIGH", "NORMAL", "NORMAL")),
      stringsAsFactors = FALSE
    )
  )

  # Check with LOW abnormality.
  result <- count_abnormal_by_baseline(
    df = df,
    .var = "myrange",
    abnormal = "LOW",
    variables = list(id = "myid", baseline = "mybase")
  )

  expected <- list(
    "not_abnormal" = c(num = 2L, denom = 4L),
    "abnormal" = c(num = 0L, denom = 1L),
    "total" = c(num = 2L, denom = 5L)
  )

  expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- count_abnormal_by_baseline(
    df = df,
    .var = "myrange",
    abnormal = "HIGH",
    variables = list(id = "myid", baseline = "mybase")
  )

  expected <- list(
    "not_abnormal" = c(num = 1L, denom = 4L),
    "abnormal" = c(num = 0L, denom = 1L),
    "total" = c(num = 1L, denom = 5L)
  )

  expect_identical(result, expected)
})

test_that("count_abnormal_by_baseline also works with character var", {
  df <- dplyr::as_tibble(
    data.frame(
      myid = as.character(c(1:6)),
      myrange = c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", NA),
      mybase = factor(c("NORMAL", "LOW", "NORMAL", "HIGH", "NORMAL", "NORMAL")),
      stringsAsFactors = FALSE
    )
  )

  # Check with LOW abnormality.
  expect_warning(

    result <- count_abnormal_by_baseline(
      df = df,
      .var = "myrange",
      abnormal = "LOW",
      variables = list(id = "myid", baseline = "mybase")
    )

  )

  expected <- list(
    "not_abnormal" = c(num = 2L, denom = 4L),
    "abnormal" = c(num = 0L, denom = 1L),
    "total" = c(num = 2L, denom = 5L)
  )

  expect_identical(result, expected)

})

test_that("s_count_abnormal_by_baseline works as expected with healthy input", {
  df <- dplyr::as_tibble(
    data.frame(
      myid = as.character(c(1:5)),
      myrange = factor(c("LOW", "NORMAL", "LOW", "HIGH", "HIGH")),
      mybase = factor(c("NORMAL", "LOW", "HIGH", "HIGH", "LOW")),
      stringsAsFactors = FALSE
    )
  )

  # Check with both LOW and HIGH abnormalities.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "myrange",
    abnormal = c(low = "LOW", high = "HIGH"),
    variables = list(id = "myid", baseline = "mybase")
  )

  expected <- list(
    section_label = with_label("", "Low"),
    not_abnormal = with_label(c(num = 2L, denom = 3L), "Not low baseline status"),
    abnormal = with_label(c(num = 0L, denom = 2L), "Low baseline status"),
    total = with_label(c(num = 2L, denom = 5L), "Total"),
    section_label = with_label("", "High"),
    not_abnormal = with_label(c(num = 1L, denom = 3L), "Not high baseline status"),
    abnormal = with_label(c(num = 1L, denom = 2L), "High baseline status"),
    total = with_label(c(num = 2L, denom = 5L), "Total")
  )

  expect_identical(result, expected)
})

test_that("analyze_abnormal_by_baseline works with default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4)),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BNRIND = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
  )
  result <- basic_table() %>%
    analyze_abnormal_by_baseline(vars = "ANRIND", abnormal = c(low = "LOW", high = "HIGH")) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(c(
    "", "Low", " Not low baseline status", " Low baseline status",
    " Total", "High", " Not high baseline status", " High baseline status",
    " Total", "all obs", "", "1/3 (33.3%)", "0/1", "1/4 (25%)",
    "", "1/2 (50%)", "1/2 (50%)", "2/4 (50%)"
  ),
  .Dim = c(9L, 2L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("analyze_abnormal_by_baseline works with custom arguments", {
  df2 <- data.frame(
    ID = as.character(c(1, 2, 3, 4)),
    RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    BLRANGE = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
  )
  result <- basic_table() %>%
    analyze_abnormal_by_baseline(
      vars = "RANGE",
      abnormal = c(low = "LOW", high = "HIGH"),
      variables = list(id = "ID", baseline = "BLRANGE"),
      .formats = c(not_abnormal = "xx / xx", abnormal = "xx / xx", total = "xx / xx")
    ) %>%
    build_table(df2)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "Low", " Not low baseline status", " Low baseline status",
      " Total", "High", " Not high baseline status", " High baseline status",
      " Total", "all obs", "", "1 / 3", "0 / 1", "1 / 4", "", "1 / 2",
      "1 / 2", "2 / 4"),
    .Dim = c(9L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})
