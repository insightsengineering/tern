library(scda)
library(rtables)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

testthat::test_that("LBT14 variant 1: HIGH works as expected", {
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = dplyr::case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    dplyr::mutate(
      BTOXGR_GP = dplyr::case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      )
    )
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing")),
      BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing"))
    )


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
    split_rows_by("BTOXGR_GP", split_fun = drop_split_levels) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("ATOXGR_GP", denom = "n", drop = TRUE) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not High (n)", "Not High", "1", "2", "3", "4",
      "1 (n)", "Not High", "1", "2", "3", "4",
      "2 (n)", "Not High", "1", "2",
      "3 (n)", "Not High", "1", "2", "4",
      "4 (n)", "Not High", "1", "3",
      "ARM A", "(N=134)", "", "121", "72 (59.5%)", "16 (13.2%)", "13 (10.7%)", "9 (7.4%)", "11 (9.1%)",
      "4", "1 (25%)", "1 (25%)", "1 (25%)", "0", "1 (25%)",
      "4", "3 (75%)", "0", "1 (25%)",
      "2", "2 (100%)", "0", "0", "0",
      "3", "1 (33.3%)", "1 (33.3%)", "1 (33.3%)",
      "ARM B", "(N=134)", "", "118", "70 (59.3%)", "13 (11%)", "12 (10.2%)", "14 (11.9%)", "9 (7.6%)",
      "4", "1 (25%)", "0", "0", "1 (25%)", "2 (50%)",
      "4", "3 (75%)", "1 (25%)", "0",
      "5", "2 (40%)", "2 (40%)", "1 (20%)", "0",
      "3", "2 (66.7%)", "0", "1 (33.3%)",
      "ARM C", "(N=132)", "", "118", "59 (50%)", "17 (14.4%)", "18 (15.3%)", "12 (10.2%)", "12 (10.2%)",
      "3", "2 (66.7%)", "0", "0", "1 (33.3%)", "0",
      "4", "3 (75%)", "1 (25%)", "0",
      "5", "4 (80%)", "0", "0", "1 (20%)",
      "2", "2 (100%)", "0", "0"
    ),
    .Dim = c(28L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LBT14 variant 2: LOW works as expected", {
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = dplyr::case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    dplyr::mutate(
      BTOXGR_GP = dplyr::case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      )
    )
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing")),
      BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing"))
    )

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
    split_rows_by("BTOXGR_GP", split_fun = drop_split_levels) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("ATOXGR_GP", denom = "n", drop = TRUE) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low (n)", "Not Low", "1", "2", "3", "4",
      "1 (n)", "Not Low", "1", "2", "3", "4",
      "2 (n)", "Not Low", "2", "3",
      "3 (n)", "Not Low", "1",
      "4 (n)", "Not Low", "3", "4",
      "ARM A", "(N=134)", "", "124", "74 (59.7%)", "13 (10.5%)", "13 (10.5%)", "17 (13.7%)", "7 (5.6%)",
      "3", "0", "1 (33.3%)", "0", "2 (66.7%)", "0",
      "2", "2 (100%)", "0", "0",
      "2", "2 (100%)", "0",
      "3", "2 (66.7%)", "1 (33.3%)", "0",
      "ARM B", "(N=134)", "", "122", "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)", "7 (5.7%)",
      "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)", "1 (16.7%)", "0",
      "1", "0", "1 (100%)", "0",
      "3", "2 (66.7%)", "1 (33.3%)",
      "2", "1 (50%)", "0", "1 (50%)",
      "ARM C", "(N=132)", "", "117", "80 (68.4%)", "9 (7.7%)", "11 (9.4%)", "9 (7.7%)", "8 (6.8%)",
      "7", "5 (71.4%)", "0", "0", "0", "2 (28.6%)",
      "4", "3 (75%)", "0", "1 (25%)",
      "4", "3 (75%)", "1 (25%)",
      "0", "0", "0", "0"
    ),
    .Dim = c(26L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LBT14 variant 3: LOW without baseline missing works as expected", {
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))
  adlb_out <- adlb_out %>%
    dplyr::filter(BTOXGR != "<Missing>") %>%
    dplyr::mutate(
      ATOXGR_GP = dplyr::case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    dplyr::mutate(
      BTOXGR_GP = dplyr::case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4"
      )
    )
  adlb_out <- adlb_out %>% dplyr::mutate(
    ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing")),
    BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not Low", "1", "2", "3", "4"))
  )


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
    split_rows_by("BTOXGR_GP", split_fun = drop_split_levels) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("ATOXGR_GP", denom = "n", drop = TRUE) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low (n)", "Not Low", "1", "2", "3", "4",
      "1 (n)", "Not Low", "1", "2", "3", "4",
      "2 (n)", "Not Low", "2", "3",
      "3 (n)", "Not Low", "1",
      "4 (n)", "Not Low", "3", "4",
      "ARM A", "(N=134)", "", "124", "74 (59.7%)", "13 (10.5%)", "13 (10.5%)", "17 (13.7%)", "7 (5.6%)",
      "3", "0", "1 (33.3%)", "0", "2 (66.7%)", "0",
      "2", "2 (100%)", "0", "0",
      "2", "2 (100%)", "0", "3",
      "2 (66.7%)", "1 (33.3%)", "0",
      "ARM B", "(N=134)", "", "122", "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)", "7 (5.7%)",
      "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)", "1 (16.7%)", "0",
      "1", "0", "1 (100%)", "0",
      "3", "2 (66.7%)", "1 (33.3%)",
      "2", "1 (50%)", "0", "1 (50%)",
      "ARM C", "(N=132)", "", "117", "80 (68.4%)", "9 (7.7%)", "11 (9.4%)", "9 (7.7%)", "8 (6.8%)",
      "7", "5 (71.4%)", "0", "0", "0", "2 (28.6%)",
      "4", "3 (75%)", "0", "1 (25%)",
      "4", "3 (75%)", "1 (25%)",
      "0", "0", "0", "0"
    ),
    .Dim = c(26L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LBT14 variant 4: LOW and force 1 missing both baseline and post-baseline, then force the missing baseline as 0 as expected", { # nolint
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD %in% "ALT") %>%
    dplyr::filter(!USUBJID %in% c("AB12345-CHN-3-id-128")) %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))

  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = dplyr::case_when(
        ATOXGR %in% c(0, 1, 2, 3, 4) ~ "Not Low",
        ATOXGR == -1 ~ "1",
        ATOXGR == -2 ~ "2",
        ATOXGR == -3 ~ "3",
        ATOXGR == -4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    dplyr::mutate(
      BTOXGR_GP = dplyr::case_when(
        BTOXGR %in% c(0, 1, 2, 3, 4, "<Missing>") ~ "Not Low",
        BTOXGR == -1 ~ "1",
        BTOXGR == -2 ~ "2",
        BTOXGR == -3 ~ "3",
        BTOXGR == -4 ~ "4"
      )
    )
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not Low", "1", "2", "3", "4", "Missing")),
      BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not Low", "1", "2", "3", "4"))
    )


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
    split_rows_by("BTOXGR_GP", split_fun = drop_split_levels) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("ATOXGR_GP", denom = "n", drop = TRUE) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low (n)", "Not Low", "1", "2", "3", "4", "Missing",
      "1 (n)", "Not Low", "1", "2", "3", "4",
      "2 (n)", "Not Low", "2", "3",
      "3 (n)", "Not Low", "1",
      "4 (n)", "Not Low", "3", "4",
      "ARM A", "(N=134)", "", "124", "74 (59.7%)", "13 (10.5%)", "13 (10.5%)", "16 (12.9%)", "7 (5.6%)", "1 (0.8%)",
      "3", "0", "1 (33.3%)", "0", "2 (66.7%)", "0",
      "2", "2 (100%)", "0", "0",
      "2", "2 (100%)", "0",
      "3", "2 (66.7%)", "1 (33.3%)", "0",
      "ARM B", "(N=134)", "", "122", "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)", "7 (5.7%)", "0",
      "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)", "1 (16.7%)", "0",
      "1", "0", "1 (100%)", "0",
      "3", "2 (66.7%)", "1 (33.3%)",
      "2", "1 (50%)", "0", "1 (50%)",
      "ARM C", "(N=132)", "", "117", "80 (68.4%)", "9 (7.7%)", "11 (9.4%)", "9 (7.7%)", "8 (6.8%)", "0",
      "7", "5 (71.4%)", "0", "0", "0", "2 (28.6%)",
      "4", "3 (75%)", "0", "1 (25%)",
      "4", "3 (75%)", "1 (25%)",
      "0", "0", "0", "0"
    ),
    .Dim = c(27L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("LBT14 variant 5: HIGH with fillings works as expected", {
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = dplyr::case_when(
        ATOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        ATOXGR == 1 ~ "1",
        ATOXGR == 2 ~ "2",
        ATOXGR == 3 ~ "3",
        ATOXGR == 4 ~ "4",
        ATOXGR == "<Missing>" ~ "Missing"
      )
    ) %>%
    dplyr::mutate(
      BTOXGR_GP = dplyr::case_when(
        BTOXGR %in% c(0, -1, -2, -3, -4) ~ "Not High",
        BTOXGR == 1 ~ "1",
        BTOXGR == 2 ~ "2",
        BTOXGR == 3 ~ "3",
        BTOXGR == 4 ~ "4",
        BTOXGR == "<Missing>" ~ "Missing"
      )
    )
  adlb_out <- adlb_out %>%
    dplyr::mutate(
      ATOXGR_GP = factor(ATOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing")),
      BTOXGR_GP = factor(BTOXGR_GP, levels = c("Not High", "1", "2", "3", "4", "Missing"))
    )


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", split_fun = drop_split_levels) %>%
    split_rows_by("BTOXGR_GP", split_fun = keep_split_levels(c("Not High", "1", "2", "3", "4", "Missing"))) %>%
    summarize_num_patients(var = "USUBJID", .stats = c("unique_count")) %>%
    count_occurrences("ATOXGR_GP", denom = "n", drop = FALSE) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not High (n)", "Not High", "1", "2", "3", "4", "Missing",
      "1 (n)", "Not High", "1", "2", "3", "4", "Missing",
      "2 (n)", "Not High", "1", "2", "3", "4", "Missing",
      "3 (n)", "Not High", "1", "2", "3", "4", "Missing",
      "4 (n)", "Not High", "1", "2", "3", "4", "Missing",
      "Missing (n)", "Not High", "1", "2", "3", "4", "Missing",
      "ARM A", "(N=134)", "", "121", "72 (59.5%)", "16 (13.2%)", "13 (10.7%)", "9 (7.4%)", "11 (9.1%)", "0",
      "4", "1 (25%)", "1 (25%)", "1 (25%)", "0", "1 (25%)", "0",
      "4", "3 (75%)", "0", "1 (25%)", "0", "0", "0",
      "2", "2 (100%)", "0", "0", "0", "0", "0",
      "3", "1 (33.3%)", "1 (33.3%)", "0", "1 (33.3%)", "0", "0",
      "0", "0", "0", "0", "0", "0", "0",
      "ARM B", "(N=134)", "", "118", "70 (59.3%)", "13 (11%)", "12 (10.2%)", "14 (11.9%)", "9 (7.6%)", "0",
      "4", "1 (25%)", "0", "0", "1 (25%)", "2 (50%)", "0",
      "4", "3 (75%)", "1 (25%)", "0", "0", "0", "0",
      "5", "2 (40%)", "2 (40%)", "1 (20%)", "0", "0", "0",
      "3", "2 (66.7%)", "0", "0", "1 (33.3%)", "0", "0",
      "0", "0", "0", "0", "0", "0", "0",
      "ARM C", "(N=132)", "", "118", "59 (50%)", "17 (14.4%)", "18 (15.3%)", "12 (10.2%)", "12 (10.2%)", "0",
      "3", "2 (66.7%)", "0", "0", "1 (33.3%)", "0", "0",
      "4", "3 (75%)", "1 (25%)", "0", "0", "0", "0",
      "5", "4 (80%)", "0", "0", "0", "1 (20%)", "0",
      "2", "2 (100%)", "0", "0", "0", "0", "0",
      "0", "0", "0", "0", "0", "0", "0"
    ),
    .Dim = c(45L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
