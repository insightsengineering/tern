# Tests all variants of LBT04.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

testthat::test_that("LBT04 default variant is produced correctly", {
  # Note: We exclude "SCREENING" visit here since otherwise it would be used as post-baseline below.
  adlb <- adlb %>%
    dplyr::filter(AVISIT != "SCREENING") %>%
    dplyr::mutate(AVISIT = droplevels(AVISIT))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("LBCAT") %>%
    split_rows_by("PARAM", split_fun = drop_split_levels) %>%
    count_abnormal(
      var = "ANRIND",
      abnormal = list(Low = "LOW", High = "HIGH"),
      exclude_base_abn = TRUE
    ) %>%
    build_table(adlb, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "CHEMISTRY", "Alanine Aminotransferase Measurement",
      "Low", "High", "C-Reactive Protein Measurement", "Low", "High",
      "IMMUNOLOGY", "Immunoglobulin A Measurement", "Low", "High",
      "A: Drug X", "(N=134)", "", "", "50/124 (40.3%)", "49/121 (40.5%)",
      "", "55/122 (45.1%)", "55/115 (47.8%)", "", "", "51/120 (42.5%)",
      "58/119 (48.7%)", "B: Placebo", "(N=134)", "", "", "46/122 (37.7%)",
      "48/118 (40.7%)", "", "44/125 (35.2%)", "47/115 (40.9%)", "",
      "", "61/119 (51.3%)", "54/123 (43.9%)", "C: Combination", "(N=132)",
      "", "", "37/117 (31.6%)", "59/118 (50%)", "", "50/120 (41.7%)",
      "44/114 (38.6%)", "", "", "50/120 (41.7%)", "45/119 (37.8%)"
    ),
    .Dim = c(13L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
