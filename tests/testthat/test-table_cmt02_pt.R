# Test single variant for CMT02_PT

adsl <- adsl_raw
adcm <- adcm_raw

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

  res <- expect_silent(result)
  expect_snapshot(res)
})
