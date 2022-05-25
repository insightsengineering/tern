# Tests the variants for EGT02.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adeg <- synthetic_cdisc_data("rcd_2022_02_28")$adeg

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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Parameter / Analysis Reference Range Indicator",
      "Heart Rate", "Low", "High", "QT Duration", "Low", "High", "RR Duration",
      "Low", "High", "A: Drug X", "(N=134)", "", "", "40/134 (29.9%)",
      "39/134 (29.1%)", "", "33/134 (24.6%)", "30/134 (22.4%)", "",
      "45/134 (33.6%)", "29/134 (21.6%)", "B: Placebo", "(N=134)",
      "", "", "43/134 (32.1%)", "45/134 (33.6%)", "", "44/134 (32.8%)",
      "42/134 (31.3%)", "", "26/134 (19.4%)", "49/134 (36.6%)", "C: Combination",
      "(N=132)", "", "", "37/132 (28%)", "36/132 (27.3%)", "", "47/132 (35.6%)",
      "34/132 (25.8%)", "", "38/132 (28.8%)", "27/132 (20.5%)", "All Patients",
      "(N=400)", "", "", "120/400 (30%)", "120/400 (30%)", "", "124/400 (31%)",
      "106/400 (26.5%)", "", "109/400 (27.3%)", "105/400 (26.2%)"
    ),
    .Dim = c(12L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Parameter / Analysis Reference Range Indicator",
      "Heart Rate", "Low", "High", "QT Duration", "Low", "High", "RR Duration",
      "Low", "High", "A: Drug X", "(N=134)", "", "", "39/128 (30.5%)",
      "37/127 (29.1%)", "", "29/113 (25.7%)", "30/125 (24%)", "", "45/129 (34.9%)",
      "27/127 (21.3%)", "B: Placebo", "(N=134)", "", "", "42/124 (33.9%)",
      "43/129 (33.3%)", "", "42/126 (33.3%)", "40/130 (30.8%)", "",
      "26/123 (21.1%)", "43/122 (35.2%)", "C: Combination", "(N=132)",
      "", "", "33/117 (28.2%)", "31/122 (25.4%)", "", "43/124 (34.7%)",
      "30/119 (25.2%)", "", "37/126 (29.4%)", "25/125 (20%)", "All Patients",
      "(N=400)", "", "", "114/369 (30.9%)", "111/378 (29.4%)", "",
      "114/363 (31.4%)", "100/374 (26.7%)", "", "108/378 (28.6%)",
      "95/374 (25.4%)"
    ),
    .Dim = c(12L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
