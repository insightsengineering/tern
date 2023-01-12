adsl <- adsl_raw %>% df_explicit_na()
addv <- addv_raw %>% df_explicit_na()

split_fun <- drop_split_levels

testthat::test_that("PDT01 is produced correctly", {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one major protocol deviation",
        nonunique = "Total number of major protocol deviations"
      )
    ) %>%
    split_rows_by(
      "DVDECOD",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(addv$DVDECOD)
    ) %>%
    count_occurrences(vars = "DVTERM") %>%
    append_varlabels(addv, "DVTERM", indent = 1L)

  result <- build_table(
    lyt = lyt,
    df = addv,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    sort_at_path(path = c("DVDECOD", "*", "DVTERM"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- matrix(
    c(
      "Protocol Deviation Coded Term", "A: Drug X", "B: Placebo", "C: Combination",
      "  Protocol Deviation Term", "(N=134)", "(N=134)", "(N=132)",
      "Total number of patients with at least one major protocol deviation", "22 (16.4%)", "23 (17.2%)", "13 (9.8%)",
      "Total number of major protocol deviations", "40", "42", "21",
      "EXCLUSION CRITERIA", "", "", "",
      "Active or untreated or other excluded cns metastases", "5 (3.7%)", "3 (2.2%)", "0",
      "Pregnancy criteria", "2 (1.5%)", "4 (3.0%)", "0",
      "History of other malignancies within the last 5 years", "3 (2.2%)", "2 (1.5%)", "0",
      "Uncontrolled concurrent condition", "3 (2.2%)", "1 (0.7%)", "0",
      "Other exclusion criteria", "0", "0", "3 (2.3%)",
      "Received prior prohibited therapy or medication", "0", "2 (1.5%)", "1 (0.8%)",
      "INCLUSION CRITERIA", "", "", "",
      "No signed ICF at study entry", "6 (4.5%)", "4 (3.0%)", "0",
      "Ineligible cancer type or current cancer stage", "6 (4.5%)", "1 (0.7%)", "1 (0.8%)",
      "Inclusion lab values outside allowed limits", "0", "3 (2.2%)", "0",
      "Does not meet prior therapy requirements", "1 (0.7%)", "0", "0",
      "Inclusion-related test not done/out of window", "0", "0", "1 (0.8%)",
      "MEDICATION", "", "", "",
      "Significant deviation from planned dose", "3 (2.2%)", "1 (0.7%)", "2 (1.5%)",
      "Received incorrect study medication", "1 (0.7%)", "2 (1.5%)", "1 (0.8%)",
      "Discontinued study drug for unspecified reason", "1 (0.7%)", "1 (0.7%)", "1 (0.8%)",
      "Dose missed or significantly out of window", "2 (1.5%)", "0", "1 (0.8%)",
      "Received prohibited concomitant medication", "0", "2 (1.5%)", "0",
      "PROCEDURAL", "", "", "",
      "Eligibility-related test not done/out of window", "1 (0.7%)", "6 (4.5%)", "1 (0.8%)",
      "Omission of screening tumor assessment", "0", "4 (3.0%)", "3 (2.3%)",
      "Missed assessment affecting safety/study outcomes", "1 (0.7%)", "2 (1.5%)", "2 (1.5%)",
      "Failure to sign updated ICF within two visits", "2 (1.5%)", "1 (0.7%)", "1 (0.8%)",
      "Missed 2 or more efficacy assessments", "2 (1.5%)", "0", "1 (0.8%)",
      "Omission of complete lab panel required by protocol", "0", "1 (0.7%)", "1 (0.8%)"
    ),
    nrow = 30, ncol = 4,
    byrow = TRUE
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
