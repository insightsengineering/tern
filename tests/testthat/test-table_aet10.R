# Tests the single variant of AET10

adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("AET10 default variant is produced correctly", {
  result1 <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_occurrences(vars = "AEDECOD") %>%
    build_table(adae, alt_counts_df = adsl)

  result2 <- prune_table(
    result1,
    keep_rows(
      has_fraction_in_any_col(atleast = 0.40, col_names = levels(adsl$ARM))
    )
  )

  result <- sort_at_path(result2, path = c("AEDECOD"), scorefun = score_occurrences)

  res <- expect_silent(result)
  expect_snapshot(res)
})
