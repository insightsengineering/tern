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

  result_matrix <- to_string_matrix(result)
  expected_matrix <- matrix(
    c(
      "Reason for Deviation", "A: Drug X", "B: Placebo", "C: Combination",
      "  Protocol Deviation Term", "(N=134)", "(N=134)", "(N=132)",
      "Total number of patients with at least one major protocol deviation related to epidemic/pandemic",
      "9 (6.7%)", "2 (1.5%)", "5 (3.8%)",
      "Total number of major protocol deviations related to epidemic/pandemic", "9", "2", "6",
      "Site action due to epidemic/pandemic (n)", "9", "2", "5",
      "Dose missed or significantly out of window", "2 (1.5%)", "0", "1 (0.8%)",
      "Failure to sign updated ICF within two visits", "2 (1.5%)", "1 (0.7%)", "1 (0.8%)",
      "Missed 2 or more efficacy assessments", "2 (1.5%)", "0", "1 (0.8%)",
      "Significant deviation from planned dose", "3 (2.2%)", "1 (0.7%)", "2 (1.5%)"
    ),
    nrow = 9, ncol = 4,
    byrow = TRUE
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
