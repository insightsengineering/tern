library(scda)
library(rtables)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
adlb <- synthetic_cdisc_data("rcd_2021_05_05")$adlb

test_that("LBT14 variant 1: HIGH works as expected", {

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not High" = c(0, -1, -2, -3, -4),
        "1" = 1,
        "2" = 2,
        "3" = 3,
        "4" = 4,
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        by_var = "BTOXGR"
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not High", "Total", "Not High", "1",
      "2", "3", "4", "Missing", "1", "Total", "Not High", "1", "2",
      "3", "4", "Missing", "2", "Total", "Not High", "1", "2", "3",
      "4", "Missing", "3", "Total", "Not High", "1", "2", "3", "4",
      "Missing", "4", "Total", "Not High", "1", "2", "3", "4", "Missing",
      "Missing", "Total", "Not High", "1", "2", "3", "4", "Missing",
      "ARM A", "(N=134)", "", "", "121", "72 (59.5%)", "16 (13.2%)",
      "13 (10.7%)", "9 (7.4%)", "11 (9.1%)", "0", "", "4", "1 (25%)",
      "1 (25%)", "1 (25%)", "0", "1 (25%)", "0", "", "4", "3 (75%)",
      "0", "1 (25%)", "0", "0", "0", "", "2", "2 (100%)", "0", "0",
      "0", "0", "0", "", "3", "1 (33.3%)", "1 (33.3%)", "0", "1 (33.3%)",
      "0", "0", "", "0", "0", "0", "0", "0", "0", "0", "ARM B", "(N=134)",
      "", "", "118", "70 (59.3%)", "13 (11%)", "12 (10.2%)", "14 (11.9%)",
      "9 (7.6%)", "0", "", "4", "1 (25%)", "0", "0", "1 (25%)", "2 (50%)",
      "0", "", "4", "3 (75%)", "1 (25%)", "0", "0", "0", "0", "", "5",
      "2 (40%)", "2 (40%)", "1 (20%)", "0", "0", "0", "", "3", "2 (66.7%)",
      "0", "0", "1 (33.3%)", "0", "0", "", "0", "0", "0", "0", "0",
      "0", "0", "ARM C", "(N=132)", "", "", "118", "59 (50%)", "17 (14.4%)",
      "18 (15.3%)", "12 (10.2%)", "12 (10.2%)", "0", "", "3", "2 (66.7%)",
      "0", "0", "1 (33.3%)", "0", "0", "", "4", "3 (75%)", "1 (25%)",
      "0", "0", "0", "0", "", "5", "4 (80%)", "0", "0", "0", "1 (20%)",
      "0", "", "2", "2 (100%)", "0", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "0", "0", "0"
    ),
    .Dim = c(51L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LBT14 variant 2: LOW works as expected", {

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not Low" = c(0, 1, 2, 3, 4),
        "1" = -1,
        "2" = -2,
        "3" = -3,
        "4" = -4,
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        by_var = "BTOXGR"
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low", "Total", "Not Low", "1",
      "2", "3", "4", "Missing", "1", "Total", "Not Low", "1", "2",
      "3", "4", "Missing", "2", "Total", "Not Low", "1", "2", "3",
      "4", "Missing", "3", "Total", "Not Low", "1", "2", "3", "4",
      "Missing", "4", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "Missing", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "ARM A", "(N=134)", "", "", "124", "74 (59.7%)", "13 (10.5%)",
      "13 (10.5%)", "17 (13.7%)", "7 (5.6%)", "0", "", "3", "0", "1 (33.3%)",
      "0", "2 (66.7%)", "0", "0", "", "2", "2 (100%)", "0", "0", "0",
      "0", "0", "", "2", "2 (100%)", "0", "0", "0", "0", "0", "", "3",
      "2 (66.7%)", "0", "0", "1 (33.3%)", "0", "0", "", "0", "0", "0",
      "0", "0", "0", "0", "ARM B", "(N=134)", "", "", "122", "76 (62.3%)",
      "12 (9.8%)", "16 (13.1%)", "11 (9%)", "7 (5.7%)", "0", "", "6",
      "2 (33.3%)", "2 (33.3%)", "1 (16.7%)", "1 (16.7%)", "0", "0",
      "", "1", "0", "0", "1 (100%)", "0", "0", "0", "", "3", "2 (66.7%)",
      "1 (33.3%)", "0", "0", "0", "0", "", "2", "1 (50%)", "0", "0",
      "0", "1 (50%)", "0", "", "0", "0", "0", "0", "0", "0", "0", "ARM C",
      "(N=132)", "", "", "117", "80 (68.4%)", "9 (7.7%)", "11 (9.4%)",
      "9 (7.7%)", "8 (6.8%)", "0", "", "7", "5 (71.4%)", "0", "0",
      "0", "2 (28.6%)", "0", "", "4", "3 (75%)", "0", "0", "1 (25%)",
      "0", "0", "", "4", "3 (75%)", "1 (25%)", "0", "0", "0", "0",
      "", "0", "0", "0", "0", "0", "0", "0", "", "0", "0", "0", "0",
      "0", "0", "0"
    ),
    .Dim = c(51L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LBT14 variant 3: LOW without baseline missing works as expected", {

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not Low" = c(0, 1, 2, 3, 4),
        "1" = -1,
        "2" = -2,
        "3" = -3,
        "4" = -4,
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        by_var = "BTOXGR",
        by_grade_list = list(
          "Not Low" = c(0, 1, 2, 3, 4),
          "1" = -1,
          "2" = -2,
          "3" = -3,
          "4" = -4
        )
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low", "Total", "Not Low", "1",
      "2", "3", "4", "Missing", "1", "Total", "Not Low", "1", "2",
      "3", "4", "Missing", "2", "Total", "Not Low", "1", "2", "3",
      "4", "Missing", "3", "Total", "Not Low", "1", "2", "3", "4",
      "Missing", "4", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "ARM A", "(N=134)", "", "", "124", "74 (59.7%)", "13 (10.5%)",
      "13 (10.5%)", "17 (13.7%)", "7 (5.6%)", "0", "", "3", "0", "1 (33.3%)",
      "0", "2 (66.7%)", "0", "0", "", "2", "2 (100%)", "0", "0", "0",
      "0", "0", "", "2", "2 (100%)", "0", "0", "0", "0", "0", "", "3",
      "2 (66.7%)", "0", "0", "1 (33.3%)", "0", "0", "ARM B", "(N=134)",
      "", "", "122", "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)",
      "7 (5.7%)", "0", "", "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)",
      "1 (16.7%)", "0", "0", "", "1", "0", "0", "1 (100%)", "0", "0",
      "0", "", "3", "2 (66.7%)", "1 (33.3%)", "0", "0", "0", "0", "",
      "2", "1 (50%)", "0", "0", "0", "1 (50%)", "0", "ARM C", "(N=132)",
      "", "", "117", "80 (68.4%)", "9 (7.7%)", "11 (9.4%)", "9 (7.7%)",
      "8 (6.8%)", "0", "", "7", "5 (71.4%)", "0", "0", "0", "2 (28.6%)",
      "0", "", "4", "3 (75%)", "0", "0", "1 (25%)", "0", "0", "", "4",
      "3 (75%)", "1 (25%)", "0", "0", "0", "0", "", "0", "0", "0",
      "0", "0", "0", "0"
    ),
    .Dim = c(43L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LBT14 variant 4: LOW and force 1 missing both baseline and post-baseline as expected", {

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    filter(!USUBJID %in% c("AB12345-CHN-3-id-128")) %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not Low" = c(0, 1, 2, 3, 4),
        "1" = -1,
        "2" = -2,
        "3" = -3,
        "4" = -4,
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        by_var = "BTOXGR"
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low", "Total", "Not Low", "1",
      "2", "3", "4", "Missing", "1", "Total", "Not Low", "1", "2",
      "3", "4", "Missing", "2", "Total", "Not Low", "1", "2", "3",
      "4", "Missing", "3", "Total", "Not Low", "1", "2", "3", "4",
      "Missing", "4", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "Missing", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "ARM A", "(N=134)", "", "", "123", "74 (60.2%)", "13 (10.6%)",
      "13 (10.6%)", "16 (13%)", "7 (5.7%)", "0", "", "3", "0", "1 (33.3%)",
      "0", "2 (66.7%)", "0", "0", "", "2", "2 (100%)", "0", "0", "0",
      "0", "0", "", "2", "2 (100%)", "0", "0", "0", "0", "0", "", "3",
      "2 (66.7%)", "0", "0", "1 (33.3%)", "0", "0", "", "1", "0", "0",
      "0", "0", "0", "1 (100%)", "ARM B", "(N=134)", "", "", "122",
      "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)", "7 (5.7%)",
      "0", "", "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)", "1 (16.7%)",
      "0", "0", "", "1", "0", "0", "1 (100%)", "0", "0", "0", "", "3",
      "2 (66.7%)", "1 (33.3%)", "0", "0", "0", "0", "", "2", "1 (50%)",
      "0", "0", "0", "1 (50%)", "0", "", "0", "0", "0", "0", "0", "0",
      "0", "ARM C", "(N=132)", "", "", "117", "80 (68.4%)", "9 (7.7%)",
      "11 (9.4%)", "9 (7.7%)", "8 (6.8%)", "0", "", "7", "5 (71.4%)",
      "0", "0", "0", "2 (28.6%)", "0", "", "4", "3 (75%)", "0", "0",
      "1 (25%)", "0", "0", "", "4", "3 (75%)", "1 (25%)", "0", "0",
      "0", "0", "", "0", "0", "0", "0", "0", "0", "0", "", "0", "0",
      "0", "0", "0", "0", "0"
    ),
    .Dim = c(51L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("LBT14 variant 4: LOW and force 1 missing both baseline and post-baseline, then force the missing baseline as 0 as expected", { #nolint

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    filter(!USUBJID %in% c("AB12345-CHN-3-id-128")) %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))

  adlb_out$BTOXGR[adlb_out$BTOXGR == "<Missing>"] <- 0 #nolint

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not Low" = c(0, 1, 2, 3, 4),
        "1" = -1,
        "2" = -2,
        "3" = -3,
        "4" = -4,
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        by_var = "BTOXGR"
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low", "Total", "Not Low", "1",
      "2", "3", "4", "Missing", "1", "Total", "Not Low", "1", "2",
      "3", "4", "Missing", "2", "Total", "Not Low", "1", "2", "3",
      "4", "Missing", "3", "Total", "Not Low", "1", "2", "3", "4",
      "Missing", "4", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "Missing", "Total", "Not Low", "1", "2", "3", "4", "Missing",
      "ARM A", "(N=134)", "", "", "124", "74 (59.7%)", "13 (10.5%)",
      "13 (10.5%)", "16 (12.9%)", "7 (5.6%)", "1 (0.8%)", "", "3",
      "0", "1 (33.3%)", "0", "2 (66.7%)", "0", "0", "", "2", "2 (100%)",
      "0", "0", "0", "0", "0", "", "2", "2 (100%)", "0", "0", "0",
      "0", "0", "", "3", "2 (66.7%)", "0", "0", "1 (33.3%)", "0", "0",
      "", "0", "0", "0", "0", "0", "0", "0", "ARM B", "(N=134)", "",
      "", "122", "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)",
      "7 (5.7%)", "0", "", "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)",
      "1 (16.7%)", "0", "0", "", "1", "0", "0", "1 (100%)", "0", "0",
      "0", "", "3", "2 (66.7%)", "1 (33.3%)", "0", "0", "0", "0", "",
      "2", "1 (50%)", "0", "0", "0", "1 (50%)", "0", "", "0", "0",
      "0", "0", "0", "0", "0", "ARM C", "(N=132)", "", "", "117", "80 (68.4%)",
      "9 (7.7%)", "11 (9.4%)", "9 (7.7%)", "8 (6.8%)", "0", "", "7",
      "5 (71.4%)", "0", "0", "0", "2 (28.6%)", "0", "", "4", "3 (75%)",
      "0", "0", "1 (25%)", "0", "0", "", "4", "3 (75%)", "1 (25%)",
      "0", "0", "0", "0", "", "0", "0", "0", "0", "0", "0", "0", "",
      "0", "0", "0", "0", "0", "0", "0"
    ),
    .Dim = c(51L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
