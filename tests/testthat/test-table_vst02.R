# Test the single variant for VST02

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
advs <- synthetic_cdisc_data("rcd_2022_02_28")$advs

testthat::test_that("1. Vital Sign Abnormalities (Regardless of Abnormality at Baseline, VST02_1)", {

  # Note: We keep only post-baseline for analysis.
  advs_f <- advs %>% dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y")

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_label = c("Parameter / Abnormality Direction"), label_pos = "visible") %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH")) %>%
    build_table(df = advs_f, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(c(
    "", "", "Parameter / Abnormality Direction", "Diastolic Blood Pressure",
    "Low", "High", "Pulse Rate", "Low", "High", "Respiratory Rate",
    "Low", "High", "Systolic Blood Pressure", "Low", "High", "Temperature",
    "Low", "High", "Weight", "Low", "High", "A: Drug X", "(N=134)",
    "", "", "53/134 (39.6%)", "53/134 (39.6%)", "", "63/134 (47%)",
    "52/134 (38.8%)", "", "59/134 (44%)", "62/134 (46.3%)", "", "54/134 (40.3%)",
    "58/134 (43.3%)", "", "59/134 (44%)", "45/134 (33.6%)", "", "62/134 (46.3%)",
    "54/134 (40.3%)", "B: Placebo", "(N=134)", "", "", "57/134 (42.5%)",
    "51/134 (38.1%)", "", "53/134 (39.6%)", "57/134 (42.5%)", "",
    "49/134 (36.6%)", "54/134 (40.3%)", "", "55/134 (41%)", "49/134 (36.6%)",
    "", "50/134 (37.3%)", "54/134 (40.3%)", "", "57/134 (42.5%)",
    "58/134 (43.3%)", "C: Combination", "(N=132)", "", "", "54/132 (40.9%)",
    "52/132 (39.4%)", "", "50/132 (37.9%)", "39/132 (29.5%)", "",
    "45/132 (34.1%)", "62/132 (47%)", "", "57/132 (43.2%)", "52/132 (39.4%)",
    "", "61/132 (46.2%)", "52/132 (39.4%)", "", "63/132 (47.7%)",
    "55/132 (41.7%)", "All Patients", "(N=400)", "", "", "164/400 (41%)",
    "156/400 (39%)", "", "166/400 (41.5%)", "148/400 (37%)", "",
    "153/400 (38.2%)", "178/400 (44.5%)", "", "166/400 (41.5%)",
    "159/400 (39.8%)", "", "170/400 (42.5%)", "151/400 (37.8%)",
    "", "182/400 (45.5%)", "167/400 (41.8%)"
  ),
  .Dim = c(21L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("2. Vital Sign Abnormalities (Among Subject Without Abnormality at Baseline, VST02_2)", {

  # Note: We keep only post-baseline for analysis.
  advs_f <- advs %>% dplyr::filter(AVISITN > 0)

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_label = c("Parameter / Abnormality Direction"), label_pos = "visible") %>%
    count_abnormal("ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = TRUE) %>%
    build_table(df = advs_f, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Parameter / Abnormality Direction", "Diastolic Blood Pressure",
      "Low", "High", "Pulse Rate", "Low", "High", "Respiratory Rate",
      "Low", "High", "Systolic Blood Pressure", "Low", "High", "Temperature",
      "Low", "High", "Weight", "Low", "High", "A: Drug X", "(N=134)",
      "", "", "45/120 (37.5%)", "47/113 (41.6%)", "", "60/125 (48%)",
      "47/119 (39.5%)", "", "53/123 (43.1%)", "57/124 (46%)", "", "47/116 (40.5%)",
      "55/121 (45.5%)", "", "47/119 (39.5%)", "43/128 (33.6%)", "",
      "54/120 (45%)", "49/123 (39.8%)", "B: Placebo", "(N=134)", "",
      "", "53/121 (43.8%)", "48/123 (39%)", "", "49/121 (40.5%)", "52/119 (43.7%)",
      "", "45/118 (38.1%)", "52/127 (40.9%)", "", "51/120 (42.5%)",
      "43/125 (34.4%)", "", "44/119 (37%)", "47/121 (38.8%)", "", "50/117 (42.7%)",
      "54/121 (44.6%)", "C: Combination", "(N=132)", "", "", "48/118 (40.7%)",
      "48/119 (40.3%)", "", "47/118 (39.8%)", "34/117 (29.1%)", "",
      "39/118 (33.1%)", "53/118 (44.9%)", "", "51/120 (42.5%)", "49/124 (39.5%)",
      "", "55/115 (47.8%)", "50/122 (41%)", "", "57/119 (47.9%)", "47/113 (41.6%)",
      "All Patients", "(N=400)", "", "", "146/359 (40.7%)", "143/355 (40.3%)",
      "", "156/364 (42.9%)", "133/355 (37.5%)", "", "137/359 (38.2%)",
      "162/369 (43.9%)", "", "149/356 (41.9%)", "147/370 (39.7%)",
      "", "146/353 (41.4%)", "140/371 (37.7%)", "", "161/356 (45.2%)",
      "150/357 (42%)"
    ),
    .Dim = c(21L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
