# Tests all variants of LBT06

adsl <- adsl_raw
adlb <- adlb_raw

testthat::test_that("LBT06 default variant is produced correctly", {
  adlb <- adlb %>%
    dplyr::filter(PARAMCD == "ALT") %>%
    dplyr::filter(!(AVISIT %in% c("SCREENING", "BASELINE"))) %>%
    dplyr::mutate(AVISIT = droplevels(AVISIT))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_fun = drop_split_levels) %>%
    split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
    count_abnormal_by_baseline(
      "ANRIND",
      abnormal = c(Low = "LOW", High = "HIGH")
    ) %>%
    build_table(adlb, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
