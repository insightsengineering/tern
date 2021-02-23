library(random.cdisc.data)
library(rtables)
library(dplyr)

adlb <- radlb(cached = TRUE)
adsl <- radsl(cached = TRUE)
adlb <- adlb %>% mutate(
  GRADDR = case_when(
    PARAMCD == "ALT" ~ "B",
    PARAMCD == "CRP" ~ "L",
    PARAMCD == "IGA" ~ "H"
  )
) %>% filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

test_that("LBT08 produce correctly", {
  df <- h_adlb_worsen(
    adlb,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    split_rows_by("GRADDR") %>%
    count_abnormal_lab_worsen_by_baseline(
      var = "ATOXGR",
      variables = list(
        id = "USUBJID",
        baseline_var = "BTOXGR",
        direction_var = "GRADDR"
      )
    ) %>%
    append_topleft("Direction of Abnormality") %>%
    build_table(df = df, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("Direction of Abnormality", "",                         "ALT",
      "High",                     "1",                        "2",
      "3",                        "4",                        "Any",
      "Low",                      "1",                        "2",
      "3",                        "4",                        "Any",
      "CRP",                      "Low",                      "1",
      "2",                        "3",                        "4",
      "Any",                      "IGA",                      "High",
      "1",                        "2",                        "3",
      "4",                        "Any",                      "ARM A",
      "(N=134)",                  "",                         "",
      "15/116 (12.9%)",           "13/120 (10.8%)",           "9/123 (7.3%)",
      "8/125 (6.4%)",             "45/130 (34.6%)",           "",
      "13/122 (10.7%)",           "8/125 (6.4%)",             "13/126 (10.3%)",
      "13/127 (10.2%)",           "48/132 (36.4%)",           "",
      "",                         "11/120 (9.2%)",            "20/122 (16.4%)",
      "12/124 (9.7%)",            "6/129 (4.7%)",             "50/131 (38.2%)",
      "",                         "",                         "23/117 (19.7%)",
      "11/119 (9.2%)",            "11/122 (9%)",              "3/125 (2.4%)",
      "48/128 (37.5%)",           "ARM B",                    "(N=134)",
      "",                         "",                         "12/116 (10.3%)",
      "5/120 (4.2%)",             "14/123 (11.4%)",           "15/127 (11.8%)",
      "47/131 (35.9%)",           "",                         "11/118 (9.3%)",
      "11/124 (8.9%)",            "14/124 (11.3%)",           "6/128 (4.7%)",
      "44/132 (33.3%)",           "",                         "",
      "14/122 (11.5%)",           "14/126 (11.1%)",           "8/128 (6.2%)",
      "5/129 (3.9%)",             "41/133 (30.8%)",           "",
      "",                         "8/119 (6.7%)",             "17/121 (14%)",
      "13/126 (10.3%)",           "7/128 (5.5%)",             "45/129 (34.9%)",
      "ARM C",                    "(N=132)",                  "",
      "",                         "17/116 (14.7%)",           "15/118 (12.7%)",
      "9/123 (7.3%)",             "12/127 (9.4%)",            "53/129 (41.1%)",
      "",                         "9/114 (7.9%)",             "7/120 (5.8%)",
      "10/125 (8%)",              "9/127 (7.1%)",             "36/129 (27.9%)",
      "",                         "",                         "13/115 (11.3%)",
      "11/119 (9.2%)",            "14/120 (11.7%)",           "9/121 (7.4%)",
      "48/126 (38.1%)",           "",                         "",
      "13/119 (10.9%)",           "11/125 (8.8%)",            "13/127 (10.2%)",
      "7/129 (5.4%)",             "44/130 (33.8%)"),
    .Dim = c(29L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
