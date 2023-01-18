# Tests all variants of LBT04

adsl <- adsl_raw
adlb <- adlb_raw

testthat::test_that("LBT04 default variant is produced correctly", {
  # Note: We exclude "SCREENING" visit here since otherwise it would be used as post-baseline below.
  adlb <- adlb %>%
    dplyr::filter(AVISIT != "SCREENING") %>%
    dplyr::mutate(AVISIT = droplevels(AVISIT))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("LBCAT") %>%
    split_rows_by("PARAM", split_fun = drop_split_levels) %>%
    count_abnormal(
      var = "ANRIND",
      abnormal = list(Low = "LOW", High = "HIGH"),
      exclude_base_abn = TRUE
    ) %>%
    build_table(adlb, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
