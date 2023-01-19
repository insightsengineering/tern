# Tests all ENT variants

adsl <- adsl_raw
adsl$REGION1 <- droplevels(adsl$REGION1) # nolint
adsl$COUNTRY <- droplevels(adsl$COUNTRY) # nolint
adsl <- adsl[order(adsl$REGION1, adsl$COUNTRY, adsl$INVID), ]

testthat::test_that("ENT01_IT is produced correctly", {
  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    split_rows_by("REGION1") %>%
    summarize_row_groups() %>%
    split_rows_by("COUNTRY") %>%
    summarize_row_groups() %>%
    split_rows_by("INVID") %>%
    summarize_row_groups()

  tbl <- build_table(l, adsl)
  result <- prune_table(tbl, all_zero_or_na, stop_depth = 4)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("ENT01a_IT is produced correctly", {
  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    split_rows_by("COUNTRY") %>%
    summarize_row_groups() %>%
    split_rows_by("INVID") %>%
    summarize_row_groups()

  result <- build_table(l, adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("ENT02_IT is produced correctly", {
  adsl$INVID_INVNAM <- paste(adsl$INVID, adsl$INVNAM, sep = " / ") # nolint

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    split_rows_by("REGION1") %>%
    summarize_row_groups() %>%
    split_rows_by("COUNTRY") %>%
    summarize_row_groups() %>%
    split_rows_by("INVID_INVNAM") %>%
    summarize_row_groups()

  tbl <- build_table(l, adsl)
  result <- prune_table(tbl, all_zero_or_na, stop_depth = 4)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("ENT02a_IT is produced correctly", {
  adsl$INVID_INVNAM <- paste(adsl$INVID, adsl$INVNAM, sep = " / ") # nolint

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    split_rows_by("COUNTRY") %>%
    summarize_row_groups() %>%
    split_rows_by("INVID_INVNAM") %>%
    summarize_row_groups()

  result <- build_table(l, adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
