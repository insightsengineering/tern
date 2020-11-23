test_that("s_count_abnormal works with healthy input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
    stringsAsFactors = FALSE
  )

  # Check with LOW abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = c(low = "LOW")
  )
  expected <- list(fraction = with_label(
    label = "low",
    c(
      num = 1L,  # Patient 1 had LOW during treatment.
      denom = 2L  # Both patients 1 and 2 have post-baseline assessments.
    )
  ))
  expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = c(High = "HIGH")
  )
  expected <- list(fraction = with_label(
    label = "High",
    c(
      num = 1L,  # One patient had HIGH during treatment.
      denom = 2L  # Since by default we don't exclude patients with abnormality.
    )
  ))
  expect_identical(result, expected)
})

test_that("s_count_abnormal works when excluding patients with abnormality at baseline", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2, 3, 3)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH", "LOW", "HIGH")),
    stringsAsFactors = FALSE
  )

  # Check with LOW abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = c(low = "LOW"),
    exclude_base_abn = TRUE
  )
  expected <- list(fraction = with_label(
    label = "low",
    c(
      num = 1L,  # Patient 1 had a new LOW during treatment.
      denom = 2L  # Only patients 1 and 2 had non-LOW at baseline.
    )
  ))
  expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "ANRIND",
    abnormal = c(High = "HIGH"),
    exclude_base_abn = TRUE
  )
  expected <- list(fraction = with_label(
    label = "High",
    c(
      num = 1L,  # Only patient 3 had a new HIGH during treatment.
      denom = 2L  # Patients 1 and 3 did not have HIGH at baseline.
    )
  ))
  expect_identical(result, expected)
})

test_that("s_count_abnormal also works with tibble and custom arguments", {
  df <- dplyr::as_tibble(
    data.frame(
      myid = as.character(c(1, 1, 1, 2, 2, 2)),
      myvisit = factor(c("SCREENING", "BASELINE", "WEEK 1", "SCREENING", "BASELINE", "WEEK 1")),
      myrange = factor(c("LOW", "NORMAL", "LOW", "NORMAL", "HIGH", "HIGH")),
      stringsAsFactors = FALSE
    )
  )

  # Check with LOW abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "myrange",
    abnormal = c(low = "LOW"),
    variables = list(id = "myid", visit = "myvisit"),
    baseline = c("SCREENING", "BASELINE"),
    exclude_base_abn = TRUE
  )
  expected <- list(fraction = with_label(
    label = "low",
    c(
      num = 0L,  # No patient had a new LOW after baseline.
      denom = 1L  # Only patient 2 did not have low during baseline.
    )
  ))
  expect_identical(result, expected)

  # Check with HIGH abnormality.
  result <- s_count_abnormal(
    df = df,
    .var = "myrange",
    abnormal = c(high = "HIGH"),
    variables = list(id = "myid", visit = "myvisit"),
    baseline = c("SCREENING", "BASELINE"),
    exclude_base_abn = TRUE
  )
  expected <- list(fraction = with_label(
    label = "high",
    c(
      num = 0L,  # Note that patient 2 does not count, as there was a HIGH during baseline already.
      denom = 1L  # Only patient 1 is in the denominator.
    )
  ))
  expect_identical(result, expected)
})

test_that("count_abnormal works with default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
  )
  result <- basic_table() %>%
    count_abnormal(var = "ANRIND", abnormal = c(low = "LOW", high = "HIGH"), exclude_base_abn = TRUE) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "low", "high", "all obs", "1/2 (50%)", "0/1"
    ),
    .Dim = 3:2
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("count_abnormal works with custom arguments", {
  df2 <- data.frame(
    ID = as.character(c(1, 1, 2, 2)),
    VISIT = factor(c("SCREENING", "WEEK 1", "SCREENING", "WEEK 1")),
    RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
  )
  result <- basic_table() %>%
    count_abnormal(
      var = "RANGE",
      abnormal = c("< LLN" = "LOW", "> ULN" = "HIGH"),
      variables = list(id = "ID", visit = "VISIT"),
      baseline = "SCREENING",
      .indent_mods = c(fraction = 1L),
      .formats = c(fraction = "xx / xx"),
      exclude_base_abn = TRUE,
      table_names = c("below", "above")
    ) %>%
    build_table(df2)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "< LLN", "> ULN", "all obs", "1 / 2", "0 / 1"),
    .Dim = 3:2
  )
  expect_identical(result_matrix, expected_matrix)
})
