adsl <- adsl_raw
adlb <- adlb_raw

adlb <- adlb %>%
  dplyr::mutate(
    GRADDR = dplyr::case_when(
      PARAMCD == "ALT" ~ "B",
      PARAMCD == "CRP" ~ "L",
      PARAMCD == "IGA" ~ "H"
    )
  ) %>%
  dplyr::filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

testthat::test_that("LBT08 produce correctly", {
  df <- h_adlb_worsen(
    adlb,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD", label_pos = "topleft") %>%
    split_rows_by("GRADDR", label_pos = "topleft") %>%
    count_abnormal_lab_worsen_by_baseline(
      var = "ATOXGR",
      variables = list(
        id = "USUBJID",
        baseline_var = "BTOXGR",
        direction_var = "GRADDR"
      )
    ) %>%
    build_table(df = df, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
