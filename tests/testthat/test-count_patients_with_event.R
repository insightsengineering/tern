testthat::test_that("s_count_patients_with_event handles NA", {
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_patients_with_event handles multiple columns", {
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_event works as expected", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = c("A", "A", "A", "A", "A", "A", "B", "B", "B"),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  test_adsl_like <- test_data[!duplicated(test_data["SUBJID"]), ]

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
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
  result <- build_table(l, test_data, alt_counts_df = test_adsl_like)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_event works as expected for different column count", {
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

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
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
  result <- build_table(lyt, df = test_data, alt_counts_df = test_adsl_like)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_patients_with_event works with factor filters", {
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_patients_with_flags works as expected with risk difference column", {
  # One statistic
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("TRTEMFL" = "Y"),
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae, alt_counts_df = tern_ex_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Multiple statistics
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = add_riskdiff("A: Drug X", "B: Placebo")) %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("TRTEMFL" = "Y"),
      .stats = c("count", "count_fraction"),
      riskdiff = TRUE
    ) %>%
    build_table(tern_ex_adae)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
