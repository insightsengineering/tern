library(scda)
library(rtables)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "PARAMCD", "  GRADDR", "ALT", "High", "1",
      "2", "3", "4", "Any", "Low", "1", "2", "3", "4", "Any", "CRP",
      "Low", "1", "2", "3", "4", "Any", "IGA", "High", "1", "2", "3",
      "4", "Any", "ARM A", "(N=134)", "", "", "16/119 (13.4%)", "14/123 (11.4%)",
      "9/127 (7.1%)", "12/129 (9.3%)", "51/129 (39.5%)", "", "13/124 (10.5%)",
      "13/127 (10.2%)", "19/128 (14.8%)", "7/130 (5.4%)", "52/130 (40%)",
      "", "", "14/122 (11.5%)", "21/124 (16.9%)", "12/129 (9.3%)",
      "10/131 (7.6%)", "57/131 (43.5%)", "", "", "24/118 (20.3%)",
      "13/120 (10.8%)", "11/124 (8.9%)", "11/129 (8.5%)", "59/129 (45.7%)",
      "ARM B", "(N=134)", "", "", "13/117 (11.1%)", "12/121 (9.9%)",
      "15/125 (12%)", "11/130 (8.5%)", "51/130 (39.2%)", "", "12/121 (9.9%)",
      "17/127 (13.4%)", "12/128 (9.4%)", "7/131 (5.3%)", "48/131 (36.6%)",
      "", "", "17/125 (13.6%)", "12/130 (9.2%)", "9/131 (6.9%)", "7/133 (5.3%)",
      "45/133 (33.8%)", "", "", "12/120 (10%)", "19/124 (15.3%)", "10/128 (7.8%)",
      "13/130 (10%)", "54/130 (41.5%)", "ARM C", "(N=132)", "", "",
      "17/116 (14.7%)", "17/119 (14.3%)", "13/123 (10.6%)", "13/128 (10.2%)",
      "60/128 (46.9%)", "", "9/116 (7.8%)", "11/123 (8.9%)", "10/127 (7.9%)",
      "10/131 (7.6%)", "40/131 (30.5%)", "", "", "13/117 (11.1%)",
      "16/122 (13.1%)", "16/123 (13%)", "4/124 (3.2%)", "49/124 (39.5%)",
      "", "", "13/119 (10.9%)", "13/125 (10.4%)", "17/128 (13.3%)",
      "4/130 (3.1%)", "47/130 (36.2%)"
    ),
    .Dim = c(29L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
