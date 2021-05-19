# Tests LBT07.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
adlb <- synthetic_cdisc_data("rcd_2021_05_05")$adlb

test_that("LBT07 is produced correctly", {
  adlb_f <- adlb %>%
    dplyr::filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
    dplyr::mutate(
      ATOXGR = as.numeric(as.character(ATOXGR)),
      WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
      WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE)
    ) %>%
    dplyr::filter(PARAMCD %in% c("ALT", "CRP")) %>%
    droplevels()

  l <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    split_rows_by("PARAMCD") %>%
    summarize_num_patients(
      var = "USUBJID",
      required = "ATOXGR",
      .stats = "unique_count"
    ) %>%
    count_abnormal_by_worst_grade(
      "ATOXGR",
      abnormal = c(Low = "low", High = "high"),
      variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
    )
  result <- build_table(l, adlb_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "ALT (n)", "Low", "1", "2", "3", "4", "Any",
      "High", "1", "2", "3", "4", "Any", "CRP (n)", "Low", "1",
      "2", "3", "4", "Any", "High", "1", "2", "3", "4", "Any",
      "ARM A", "134", "", "14 (10.4%)", "13 (9.7%)", "20 (14.9%)",
      "7 (5.2%)", "54 (40.3%)", "", "18 (13.4%)", "15 (11.2%)",
      "10 (7.5%)", "12 (9%)", "55 (41%)", "134", "", "16 (11.9%)",
      "21 (15.7%)", "12 (9%)", "11 (8.2%)", "60 (44.8%)", "",
      "17 (12.7%)", "15 (11.2%)", "16 (11.9%)", "12 (9%)", "60 (44.8%)",
      "ARM B", "134", "", "15 (11.2%)", "18 (13.4%)", "12 (9%)", "8 (6%)",
      "53 (39.6%)", "", "16 (11.9%)", "13 (9.7%)", "16 (11.9%)",
      "11 (8.2%)", "56 (41.8%)", "134", "", "19 (14.2%)", "13 (9.7%)",
      "9 (6.7%)", "7 (5.2%)", "48 (35.8%)", "", "15 (11.2%)",
      "16 (11.9%)", "12 (9%)", "12 (9%)", "55 (41%)", "ARM C",
      "132", "", "10 (7.6%)", "11 (8.3%)", "10 (7.6%)", "10 (7.6%)",
      "41 (31.1%)", "", "18 (13.6%)", "18 (13.6%)", "13 (9.8%)",
      "13 (9.8%)", "62 (47%)", "132", "", "15 (11.4%)", "16 (12.1%)",
      "18 (13.6%)", "5 (3.8%)", "54 (40.9%)", "", "9 (6.8%)",
      "13 (9.8%)", "14 (10.6%)", "10 (7.6%)", "46 (34.8%)"
    ),
    .Dim = c(27L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
