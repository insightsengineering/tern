# Test the single variant for VST02

adsl <- adsl_raw
advs <- advs_raw

testthat::test_that("1. Vital Sign Abnormalities (Regardless of Abnormality at Baseline, VST02_1)", {
  # Note: We keep only post-baseline for analysis.
  advs_f <- advs %>% dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y")

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_label = c("Parameter / Abnormality Direction"), label_pos = "visible") %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH")) %>%
    build_table(df = advs_f, alt_counts_df = adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("2. Vital Sign Abnormalities (Among Subject Without Abnormality at Baseline, VST02_2)", {
  # Note: We keep only post-baseline for analysis.
  advs_f <- advs %>% dplyr::filter(AVISITN > 0)

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_label = c("Parameter / Abnormality Direction"), label_pos = "visible") %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = TRUE) %>%
    build_table(df = advs_f, alt_counts_df = adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})
