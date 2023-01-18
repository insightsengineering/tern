# Tests the variants for EGT02

adsl <- adsl_raw
adeg <- adeg_raw

testthat::test_that("(EGT02) 1. Regardless of Abnormality at Baseline", {
  # Note: We exclude "SCREENING" and "BASELINE" visits here
  # so to keep only post-baseline for analysis.
  adeg <- adeg %>%
    dplyr::filter(AVISIT != "SCREENING") %>%
    dplyr::filter(AVISIT != "BASELINE") %>%
    dplyr::mutate(AVISIT = droplevels(AVISIT))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_overall_col("All Patients") %>%
    add_colcounts() %>%
    split_rows_by(
      "PARAM",
      split_label = c("Parameter / Analysis Reference Range Indicator"),
      split_fun = keep_split_levels(c("Heart Rate", "QT Duration", "RR Duration")),
      label_pos = "visible"
    ) %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = FALSE) %>%
    build_table(df = adeg, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("(EGT02) 2. Among Subjects Without Abnormality at Baseline", {
  adeg <- adeg %>%
    dplyr::filter(AVISIT != "SCREENING") %>%
    dplyr::filter(AVISIT != "BASELINE") %>%
    dplyr::mutate(AVISIT = droplevels(AVISIT))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    add_overall_col("All Patients") %>%
    split_rows_by(
      "PARAM",
      split_label = c("Parameter / Analysis Reference Range Indicator"),
      label_pos = "visible",
      split_fun = keep_split_levels(c("Heart Rate", "QT Duration", "RR Duration"))
    ) %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = TRUE) %>%
    build_table(df = adeg, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
