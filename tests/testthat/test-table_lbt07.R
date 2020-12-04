# Tests LBT07.

library(random.cdisc.data)
library(dplyr)

test_that("LBT07 is produced correctly", {
  adlb <- radlb(cached = TRUE)
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
    c("", "ALT (n)", "Low", "1", "2", "3", "4", "5", "Any",
      "High", "1", "2", "3", "4", "5", "Any", "CRP (n)", "Low", "1",
      "2", "3", "4", "5", "Any", "High", "1", "2", "3", "4", "5", "Any",
      "ARM A", "134", "", "14 (10.4%)", "9 (6.7%)", "14 (10.4%)", "13 (9.7%)",
      "4 (3%)", "54 (40.3%)", "", "17 (12.7%)", "14 (10.4%)", "10 (7.5%)",
      "8 (6%)", "6 (4.5%)", "55 (41%)", "134", "", "13 (9.7%)", "21 (15.7%)",
      "12 (9%)", "6 (4.5%)", "8 (6%)", "60 (44.8%)", "", "16 (11.9%)",
      "12 (9%)", "13 (9.7%)", "14 (10.4%)", "5 (3.7%)", "60 (44.8%)",
      "ARM B", "134", "", "13 (9.7%)", "14 (10.4%)", "15 (11.2%)",
      "6 (4.5%)", "5 (3.7%)", "53 (39.6%)", "", "15 (11.2%)", "6 (4.5%)",
      "15 (11.2%)", "16 (11.9%)", "4 (3%)", "56 (41.8%)", "134", "",
      "16 (11.9%)", "15 (11.2%)", "8 (6%)", "5 (3.7%)", "4 (3%)", "48 (35.8%)",
      "", "14 (10.4%)", "14 (10.4%)", "6 (4.5%)", "15 (11.2%)", "6 (4.5%)",
      "55 (41%)", "ARM C", "132", "", "9 (6.8%)", "9 (6.8%)", "10 (7.6%)",
      "9 (6.8%)", "4 (3%)", "41 (31.1%)", "", "18 (13.6%)", "16 (12.1%)",
      "9 (6.8%)", "12 (9.1%)", "7 (5.3%)", "62 (47%)", "132", "", "15 (11.4%)",
      "11 (8.3%)", "16 (12.1%)", "10 (7.6%)", "2 (1.5%)", "54 (40.9%)",
      "", "8 (6.1%)", "12 (9.1%)", "11 (8.3%)", "8 (6.1%)", "7 (5.3%)",
      "46 (34.8%)"),
    .Dim = c(31L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
