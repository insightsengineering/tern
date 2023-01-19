# Test the single variant for EGT01

adsl <- adsl_raw
adeg <- adeg_raw

testthat::test_that("EGT01 default variant is produced correctly", {
  adeg_f <- adeg %>%
    dplyr::filter(PARAM == "Heart Rate" & AVISIT != "SCREENING")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT", split_fun = drop_split_levels) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Analysis Value", "Absolute Change from Baseline")
    ) %>%
    summarize_colvars()

  result <- build_table(l, adeg_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
