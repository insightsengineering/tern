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
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with HIGH abnormality.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "ANRIND",
    abnormal = "HIGH"
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_abnormal_by_baseline also works with tibble and custom arguments", {
  df <- tibble::as_tibble(
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
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Check with HIGH abnormality.
  result <- s_count_abnormal_by_baseline(
    df = df,
    .var = "myrange",
    abnormal = "HIGH",
    variables = list(id = "myid", baseline = "mybase")
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_abnormal_by_baseline throws warning with character var", {
  df <- tibble::as_tibble(
    data.frame(
      myid = as.character(c(1:6)),
      myrange = c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", NA),
      mybase = factor(c("NORMAL", "LOW", "NORMAL", "HIGH", "NORMAL", "NORMAL")),
      stringsAsFactors = FALSE
    )
  )

  # Check with LOW abnormality.
  testthat::expect_warning(
    result <- s_count_abnormal_by_baseline(
      df = df,
      .var = "myrange",
      abnormal = "LOW",
      variables = list(id = "myid", baseline = "mybase")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
