# Tests the single variant for LBT02

adsl <- adsl_raw
adlb <- adlb_raw

testthat::test_that("LBT02 default variant is produced correctly", {
  adlb <- subset(adlb, AVISIT != "SCREENING" & PARAMCD == "ALT")
  adlb$AVISIT <- droplevels(adlb$AVISIT) # nolint snake_case

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    split_rows_by(var = "AVISIT") %>%
    add_colcounts() %>%
    summarize_vars(vars = "AVAL")

  result <- build_table(l, df = adlb, alt_counts_df = adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})
