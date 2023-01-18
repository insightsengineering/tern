# Test variants for AET06_SMQ.
adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("AET06_SMQ variant 1 is produced correctly", {
  adsl_labels <- formatters::var_labels(adsl)
  adae_labels <- formatters::var_labels(adae)

  adae <- adae %>%
    dplyr::mutate(
      SMQ1 = dplyr::case_when(
        AEBODSYS %in% c("cl A.1", "cl B.1", "cl C.1", "cl D.1") ~ "SMQ 1 (broad)",
        TRUE ~ NA_character_
      ),
      SMQ2 = dplyr::case_when(
        AEBODSYS %in% c("cl A.1", "cl D.1") ~ "SMQ 1 (narrow)",
        TRUE ~ NA_character_
      ),
      SMQ3 = dplyr::case_when(
        AEDECOD %in% c("dcd B.2.1.2.1", "dcd A.1.1.1.2", "dcd C.2.1.2.1", "dcd B.2.2.3.1") ~ "AESI",
        TRUE ~ NA_character_
      )
    )


  adae_smq1 <- adae %>%
    dplyr::filter(!is.na(SMQ1)) %>%
    dplyr::rename(SMQ = SMQ1) %>%
    dplyr::select(-SMQ2, -SMQ3)

  adae_smq2 <- adae %>%
    dplyr::filter(!is.na(SMQ2)) %>%
    dplyr::rename(SMQ = SMQ2) %>%
    dplyr::select(-SMQ1, -SMQ3)

  adae_smq3 <- adae %>%
    dplyr::filter(!is.na(SMQ3)) %>%
    dplyr::rename(SMQ = SMQ3) %>%
    dplyr::select(-SMQ1, -SMQ2)

  adae_f <- rbind(adae_smq1, adae_smq2, adae_smq3)

  formatters::var_labels(adae_f) <- c(adae_labels, "SMQ" = "Standardised MedDRA Queries")

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD")

  result <- build_table(lyt, adae_f, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET06_SMQ variant 2 is produced correctly", {
  adsl_labels <- formatters::var_labels(adsl)
  adae_labels <- formatters::var_labels(adae)

  adsl <- adsl %>%
    dplyr::mutate(
      AGE65 = dplyr::case_when(
        AGE >= 65 ~ ">= 65",
        TRUE ~ "< 65"
      ),
      AGE65 = factor(AGE65, levels = c(">= 65", "< 65"))
    )

  formatters::var_labels(adsl) <- c(adsl_labels, "AGE65" = "AGE65 GROUP")

  adae <- adae %>%
    dplyr::mutate(
      SMQ1 = dplyr::case_when(
        AEBODSYS %in% c("cl A.1", "cl B.1", "cl C.1", "cl D.1") ~ "SMQ 1 (broad)",
        TRUE ~ NA_character_
      ),
      SMQ2 = dplyr::case_when(
        AEBODSYS %in% c("cl A.1", "cl D.1") ~ "SMQ 1 (narrow)",
        TRUE ~ NA_character_
      ),
      SMQ3 = dplyr::case_when(
        AEDECOD %in% c("dcd B.2.1.2.1", "dcd A.1.1.1.2", "dcd C.2.1.2.1", "dcd B.2.2.3.1") ~ "AESI",
        TRUE ~ NA_character_
      ),
      AGE65 = dplyr::case_when(
        AGE >= 65 ~ ">= 65",
        TRUE ~ "< 65"
      ),
      AGE65 = factor(AGE65, levels = c(">= 65", "< 65"))
    )

  adae_smq1 <- adae %>%
    dplyr::filter(!is.na(SMQ1)) %>%
    dplyr::rename(SMQ = SMQ1) %>%
    dplyr::select(-SMQ2, -SMQ3)

  adae_smq2 <- adae %>%
    dplyr::filter(!is.na(SMQ2)) %>%
    dplyr::rename(SMQ = SMQ2) %>%
    dplyr::select(-SMQ1, -SMQ3)

  adae_smq3 <- adae %>%
    dplyr::filter(!is.na(SMQ3)) %>%
    dplyr::rename(SMQ = SMQ3) %>%
    dplyr::select(-SMQ1, -SMQ2)

  adae_f <- rbind(adae_smq1, adae_smq2, adae_smq3)

  formatters::var_labels(adae_f) <- c(adae_labels, "SMQ" = "Standardised MedDRA Queries", "AGE65" = "AGE65 GROUP")

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("AGE65") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD")

  result <- build_table(lyt, adae_f, alt_counts_df = adsl) %>%
    sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- expect_silent(result)
  expect_snapshot(res)
})
