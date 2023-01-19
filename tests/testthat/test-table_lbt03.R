# Tests the single variant for LBT03

adsl <- adsl_raw
adlb <- adlb_raw

testthat::test_that("LBT03 default variant is produced correctly", {
  adlb_f <- adlb %>%
    dplyr::filter(AVISIT != "SCREENING", PARAMCD == "ALT") %>%
    dplyr::mutate(
      AVISIT = droplevels(AVISIT),
      ABLFLL = ABLFL == "Y"
    )

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    summarize_change(
      "CHG",
      variables = list(value = "AVAL", baseline_flag = "ABLFLL"),
      na.rm = TRUE
    ) %>%
    build_table(
      df = adlb_f,
      alt_counts_df = adsl
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
