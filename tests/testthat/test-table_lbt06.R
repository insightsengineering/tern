# Tests all variants of LBT06.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Alanine Aminotransferase Measurement", "WEEK 1 DAY 8",
      "Low", "Not low baseline status", "Low baseline status", "Total",
      "High", "Not high baseline status", "High baseline status", "Total",
      "WEEK 2 DAY 15", "Low", "Not low baseline status", "Low baseline status",
      "Total", "High", "Not high baseline status", "High baseline status",
      "Total", "WEEK 3 DAY 22", "Low", "Not low baseline status", "Low baseline status",
      "Total", "High", "Not high baseline status", "High baseline status",
      "Total", "WEEK 4 DAY 29", "Low", "Not low baseline status", "Low baseline status",
      "Total", "High", "Not high baseline status", "High baseline status",
      "Total", "WEEK 5 DAY 36", "Low", "Not low baseline status", "Low baseline status",
      "Total", "High", "Not high baseline status", "High baseline status",
      "Total", "A: Drug X", "(N=134)", "", "", "", "10/124 (8.1%)",
      "2/10 (20%)", "12/134 (9%)", "", "10/121 (8.3%)", "1/13 (7.7%)",
      "11/134 (8.2%)", "", "", "13/124 (10.5%)", "2/10 (20%)", "15/134 (11.2%)",
      "", "8/121 (6.6%)", "1/13 (7.7%)", "9/134 (6.7%)", "", "", "12/124 (9.7%)",
      "1/10 (10%)", "13/134 (9.7%)", "", "12/121 (9.9%)", "1/13 (7.7%)",
      "13/134 (9.7%)", "", "", "10/124 (8.1%)", "0/10", "10/134 (7.5%)",
      "", "12/121 (9.9%)", "3/13 (23.1%)", "15/134 (11.2%)", "", "",
      "14/124 (11.3%)", "2/10 (20%)", "16/134 (11.9%)", "", "16/121 (13.2%)",
      "1/13 (7.7%)", "17/134 (12.7%)", "B: Placebo", "(N=134)", "",
      "", "", "8/122 (6.6%)", "0/12", "8/134 (6%)", "", "11/118 (9.3%)",
      "2/16 (12.5%)", "13/134 (9.7%)", "", "", "8/122 (6.6%)", "0/12",
      "8/134 (6%)", "", "13/118 (11%)", "2/16 (12.5%)", "15/134 (11.2%)",
      "", "", "13/122 (10.7%)", "2/12 (16.7%)", "15/134 (11.2%)", "",
      "16/118 (13.6%)", "2/16 (12.5%)", "18/134 (13.4%)", "", "", "13/122 (10.7%)",
      "2/12 (16.7%)", "15/134 (11.2%)", "", "8/118 (6.8%)", "2/16 (12.5%)",
      "10/134 (7.5%)", "", "", "15/122 (12.3%)", "3/12 (25%)", "18/134 (13.4%)",
      "", "9/118 (7.6%)", "2/16 (12.5%)", "11/134 (8.2%)", "C: Combination",
      "(N=132)", "", "", "", "6/117 (5.1%)", "0/15", "6/132 (4.5%)",
      "", "11/118 (9.3%)", "1/14 (7.1%)", "12/132 (9.1%)", "", "",
      "13/117 (11.1%)", "0/15", "13/132 (9.8%)", "", "15/118 (12.7%)",
      "1/14 (7.1%)", "16/132 (12.1%)", "", "", "6/117 (5.1%)", "3/15 (20%)",
      "9/132 (6.8%)", "", "14/118 (11.9%)", "0/14", "14/132 (10.6%)",
      "", "", "10/117 (8.5%)", "0/15", "10/132 (7.6%)", "", "18/118 (15.3%)",
      "1/14 (7.1%)", "19/132 (14.4%)", "", "", "10/117 (8.5%)", "1/15 (6.7%)",
      "11/132 (8.3%)", "", "10/118 (8.5%)", "0/14", "10/132 (7.6%)"
    ),
    .Dim = c(48L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
