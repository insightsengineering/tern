# Test the single variant for LBT01

adsl <- adsl_raw
adlb <- adlb_raw

testthat::test_that("LBT01 default variant is produced correctly", {
  adlb_f <- adlb %>%
    dplyr::filter(
      PARAM == "Alanine Aminotransferase Measurement" &
        !(ARM == "B: Placebo" & AVISIT == "WEEK 1 DAY 8") &
        AVISIT != "SCREENING"
    )

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Analysis Value", "Absolute Change from Baseline")
    ) %>%
    summarize_colvars()

  result <- build_table(l, adlb_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})
