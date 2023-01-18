adsl <- adsl_raw
adlb <- adlb_raw

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
