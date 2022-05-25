# Test single variant for CMT02_PT

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adcm <- synthetic_cdisc_data("rcd_2022_02_28")$adcm

testthat::test_that("CMT02_PT default variant is produced correctly", {
  adcm <- adcm %>%
    dplyr::mutate(
      ASEQ = as.factor(ASEQ)
    )

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      count_by = "ASEQ",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one event",
      "Total number of events", "medname A_1/3", "medname C_2/2", "medname B_1/4",
      "medname B_2/4", "medname A_2/3", "medname C_1/2", "medname B_4/4",
      "medname A_3/3", "medname B_3/4", "A: Drug X", "(N=134)", "122 (91%)",
      "609", "54 (40.3%)", "52 (38.8%)", "52 (38.8%)", "52 (38.8%)",
      "53 (39.6%)", "51 (38.1%)", "50 (37.3%)", "45 (33.6%)", "47 (35.1%)",
      "B: Placebo", "(N=134)", "123 (91.8%)", "622", "49 (36.6%)",
      "58 (43.3%)", "57 (42.5%)", "55 (41%)", "50 (37.3%)", "50 (37.3%)",
      "45 (33.6%)", "54 (40.3%)", "47 (35.1%)", "C: Combination", "(N=132)",
      "120 (90.9%)", "703", "69 (52.3%)", "60 (45.5%)", "59 (44.7%)",
      "56 (42.4%)", "56 (42.4%)", "56 (42.4%)", "55 (41.7%)", "48 (36.4%)",
      "52 (39.4%)", "All Patients", "(N=400)", "365 (91.2%)", "1934",
      "172 (43%)", "170 (42.5%)", "168 (42%)", "163 (40.8%)", "159 (39.8%)",
      "157 (39.2%)", "150 (37.5%)", "147 (36.8%)", "146 (36.5%)"
    ),
    .Dim = c(13L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
