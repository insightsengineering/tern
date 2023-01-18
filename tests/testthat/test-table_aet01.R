adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("Safety Summary Variant 1 works as expected", {
  # Add flags (TRUE/FALSE) for select AEs of interest.
  adae <- adae %>%
    dplyr::mutate(
      FATAL = AESDTH == "Y",
      SER = AESER == "Y",
      SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
      SERDSM = AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      RELSER = AESER == "Y" & AEREL == "Y",
      WD = AEACN == "DRUG WITHDRAWN",
      DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL = AEREL == "Y",
      RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
      RELDSM = AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      CTC35 = AETOXGR %in% c("3", "4", "5")
    )

  aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "CTC35")
  labels <- c(
    "AE with fatal outcome",
    "Serious AE",
    "Serious AE leading to withdrawal from treatment",
    "Serious AE leading to dose modification/interruption",
    "Related Serious AE",
    "AE leading to withdrawal from treatment",
    "AE leading to dose modification/interruption",
    "Related AE",
    "Related AE leading to withdrawal from treatment",
    "Related AE leading to dose modification/interruption",
    "Grade 3-5 AE"
  )
  for (i in seq_along(aesi_vars)) {
    attr(adae[[aesi_vars[i]]], "label") <- labels[i]
  }

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      denom = "N_col"
    ) %>%
    count_values(
      "DCSREAS",
      values = "ADVERSE EVENT",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      denom = "N_col"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = "AB12345"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) %>%
    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total AEs"),
      table_names = "total_aes"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("Safety Summary Variant 2 (with Medical Concepts Section) works as expected", {
  # Add flags (TRUE/FALSE) for select AEs of interest.
  adae <- adae %>%
    dplyr::mutate(
      FATAL = AESDTH == "Y",
      SER = AESER == "Y",
      SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
      SERDSM = AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      RELSER = AESER == "Y" & AEREL == "Y",
      WD = AEACN == "DRUG WITHDRAWN",
      DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL = AEREL == "Y",
      RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
      RELDSM = AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      CTC35 = AETOXGR %in% c("3", "4", "5")
    )
  aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "CTC35")
  labels <- c(
    "AE with fatal outcome",
    "Serious AE",
    "Serious AE leading to withdrawal from treatment",
    "Serious AE leading to dose modification/interruption",
    "Related Serious AE",
    "AE leading to withdrawal from treatment",
    "AE leading to dose modification/interruption",
    "Related AE",
    "Related AE leading to withdrawal from treatment",
    "Related AE leading to dose modification/interruption",
    "Grade 3-5 AE"
  )
  for (i in seq_along(aesi_vars)) {
    attr(adae[[aesi_vars[i]]], "label") <- labels[i]
  }

  # Add flags (TRUE/FALSE) for select AE basket variables.
  adae <- adae %>%
    dplyr::mutate(
      SMQ01 = SMQ01NAM != "",
      SMQ02 = SMQ02NAM != "",
      CQ01 = CQ01NAM != ""
    )
  aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "CTC35")
  basket_vars <- c("SMQ01", "SMQ02", "CQ01")
  labels <- c(
    aesi_label(adae$SMQ01NAM, adae$SMQ01SC),
    aesi_label(adae$SMQ02NAM, adae$SMQ02SC),
    aesi_label(adae$CQ01NAM)
  )
  for (i in seq_along(basket_vars)) {
    attr(adae[[basket_vars[i]]], "label") <- labels[i]
  }

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      denom = "N_col"
    ) %>%
    count_values(
      "DCSREAS",
      values = "ADVERSE EVENT",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      denom = "N_col"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = "AB12345"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) %>%
    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total AEs"),
      denom = "N_col",
      table_names = "total_aes"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible",
      table_names = "table_ae"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae[, basket_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible",
      table_names = "table_aesi"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("Safety Summary Variant 3 (with Modified Rows) works as expected", {
  # Add flags (TRUE/FALSE) for select AEs of interest -- custom groups.
  adae <- adae %>%
    dplyr::mutate(
      FATAL = AESDTH == "Y",
      SER = AESER == "Y",
      WD = AEACN == "DRUG WITHDRAWN",
      REL = AEREL == "Y",
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    )
  aesi_vars <- c("FATAL", "SER", "WD", "REL", "CTC35", "CTC45")
  labels <- c(
    "AE with fatal outcome",
    "Serious AE",
    "AE leading to withdrawal from treatment",
    "Related AE",
    "Grade 3-5 AE",
    "Grade 4/5 AE"
  )
  for (i in seq_along(aesi_vars)) {
    attr(adae[[aesi_vars[i]]], "label") <- labels[i]
  }

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      denom = "N_col"
    ) %>%
    count_values(
      "DCSREAS",
      values = "ADVERSE EVENT",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      denom = "N_col",
      table_names = "tbl_dscsreas_ae"
    ) %>%
    count_values(
      "DCSREAS",
      values = "WITHDRAWAL BY SUBJECT",
      .labels = c(count_fraction = "Total number of patients withdrawn informed consent"),
      denom = "N_col",
      table_names = "tbl_dscsreas_wd"
    )
  result_adsl <- build_table(lyt_adsl, df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = "AB12345"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) %>%
    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total AEs"),
      denom = "N_col",
      table_names = "total_aes"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  result_matrix <- to_string_matrix(result)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("Safety Summary Variant 4 (with Rows Counting Events and Additional Sections) works as expected", {
  adae <- adae %>%
    dplyr::mutate(USUBJID_AESEQ = paste(USUBJID, AESEQ, sep = "@@")) # nolint # Create unique ID per AE in dataset.

  # Add flags (TRUE/FALSE) for select AEs of interest.
  adae <- adae %>%
    dplyr::mutate(
      FATAL = AESDTH == "Y",
      SER = AESER == "Y",
      WD = AEACN == "DRUG WITHDRAWN",
      DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL = AEREL == "Y",
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    )
  aesi_vars <- c("FATAL", "SER", "WD", "DSM", "REL", "CTC35", "CTC45")
  labels <- c(
    "AE with fatal outcome",
    "Serious AE",
    "AE leading to withdrawal from treatment",
    "AE leading to dose modification/interruption",
    "Related AE",
    "Grade 3-5 AE",
    "Grade 4/5"
  )
  for (i in seq_along(aesi_vars)) {
    attr(adae[[aesi_vars[i]]], "label") <- labels[i]
  }

  count_subj_vars <- c("FATAL", "SER", "WD", "DSM", "REL", "CTC35")
  count_term_vars <- c("SER", "DSM", "REL", "CTC35", "CTC45")
  count_ae_vars <- c("SER", "DSM", "REL", "CTC35", "CTC45")

  # Layout for variables from adsl dataset.
  lyt_adsl <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      denom = "N_col"
    ) %>%
    count_values(
      "DCSREAS",
      values = "ADVERSE EVENT",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      denom = "N_col"
    )

  result_adsl <- build_table(lyt_adsl, df = adsl)

  # Layout for variables from adae dataset.
  lyt_adae <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = "AB12345"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      table_names = "total_subj"
    ) %>%
    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total AEs"),
      denom = "N_col",
      table_names = "total_aes"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = formatters::var_labels(adae[, count_subj_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    ) %>%
    count_patients_with_flags(
      "AEDECOD",
      flag_variables = formatters::var_labels(adae[, count_term_vars]),
      denom = "N_col",
      var_labels = "Total number of unique preferred terms which are",
      show_labels = "visible",
      .stats = "count",
      .formats = c(count = "xx"),
      table_names = "table_term"
    ) %>%
    count_patients_with_flags(
      "USUBJID_AESEQ",
      flag_variables = formatters::var_labels(adae[, count_ae_vars]),
      denom = "N_col",
      var_labels = "Total number of adverse events which are",
      show_labels = "visible",
      .stats = "count",
      .formats = c(count = "xx"),
      table_names = "table_ae"
    )

  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  # Combine tables.
  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  res <- expect_silent(result)
  expect_snapshot(res)
})
