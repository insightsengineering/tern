adsl <- adsl_raw %>% df_explicit_na()
addv <- addv_raw %>% df_explicit_na()
addv_pan <- addv %>% filter(AEPRELFL == "Y" & DVCAT == "MAJOR")

testthat::test_that("PDT02 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one major protocol deviation related to epidemic/pandemic",
        nonunique = "Total number of major protocol deviations related to epidemic/pandemic"
      )
    ) %>%
    split_rows_by(
      "DVREAS",
      split_fun = drop_split_levels,
      nested = FALSE,
      label_pos = "topleft",
      split_label = obj_label(addv_pan$DVREAS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique_count")
    ) %>%
    count_occurrences(vars = "DVTERM") %>%
    append_varlabels(addv_pan, "DVTERM", indent = 1L)

  result <- build_table(lyt, addv_pan, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
