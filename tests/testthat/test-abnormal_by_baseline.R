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

  expected <- list(fraction = list(
    low = list(
      "not_abnormal" = c(num = 2L, denom = 3L),
      "abnormal" = c(num = 0L, denom = 2L),
      "total" = c(num = 2L, denom = 5L)
    ),
    high = list(
      "not_abnormal" = c(num = 1L, denom = 3L),
      "abnormal" = c(num = 1L, denom = 2L),
      "total" = c(num = 2L, denom = 5L)
    )
  ))

  expect_identical(result, expected)
})
