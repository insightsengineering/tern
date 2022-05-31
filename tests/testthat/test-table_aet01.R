library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total AEs", "Total number of deaths", "Total number of patients withdrawn from study due to an AE",
      "Total number of patients with at least one", "AE with fatal outcome",
      "Serious AE", "Serious AE leading to withdrawal from treatment",
      "Serious AE leading to dose modification/interruption", "Related Serious AE",
      "AE leading to withdrawal from treatment", "AE leading to dose modification/interruption",
      "Related AE", "Related AE leading to withdrawal from treatment",
      "Related AE leading to dose modification/interruption", "Grade 3-5 AE",
      "A: Drug X", "(N=134)", "122 (91%)", "609", "22 (16.42%)",
      "6 (4.48%)", "", "76 (56.7%)", "104 (77.6%)", "8 (6%)",
      "36 (26.9%)", "76 (56.7%)", "23 (17.2%)", "72 (53.7%)", "105 (78.4%)",
      "6 (4.5%)", "31 (23.1%)", "109 (81.3%)", "B: Placebo", "(N=134)",
      "123 (91.8%)", "622", "26 (19.40%)", "1 (0.75%)", "", "70 (52.2%)",
      "101 (75.4%)", "8 (6%)", "31 (23.1%)", "70 (52.2%)", "24 (17.9%)",
      "65 (48.5%)", "108 (80.6%)", "9 (6.7%)", "31 (23.1%)", "104 (77.6%)",
      "C: Combination", "(N=132)", "120 (90.9%)", "703", "19 (14.39%)",
      "2 (1.52%)", "", "75 (56.8%)", "99 (75%)", "7 (5.3%)", "26 (19.7%)",
      "75 (56.8%)", "27 (20.5%)", "76 (57.6%)", "109 (82.6%)",
      "13 (9.8%)", "32 (24.2%)", "109 (82.6%)"
    ),
    .Dim = c(18L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
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
    aesi_label(adae$SMQ01NAM, adae$SMQ01SC), aesi_label(adae$SMQ02NAM, adae$SMQ02SC), aesi_label(adae$CQ01NAM)
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total AEs", "Total number of deaths", "Total number of patients withdrawn from study due to an AE",
      "Total number of patients with at least one", "AE with fatal outcome",
      "Serious AE", "Serious AE leading to withdrawal from treatment",
      "Serious AE leading to dose modification/interruption", "Related Serious AE",
      "AE leading to withdrawal from treatment", "AE leading to dose modification/interruption",
      "Related AE", "Related AE leading to withdrawal from treatment",
      "Related AE leading to dose modification/interruption", "Grade 3-5 AE",
      "Total number of patients with at least one", "C.1.1.1.3/B.2.2.3.1 AESI (BROAD)",
      "SMQ 02 Reference Name", "D.2.1.5.3/A.1.1.1.1 AESI", "A: Drug X",
      "(N=134)", "122 (91%)", "609", "22 (16.42%)", "6 (4.48%)",
      "", "76 (56.7%)", "104 (77.6%)", "8 (6%)", "36 (26.9%)",
      "76 (56.7%)", "23 (17.2%)", "72 (53.7%)", "105 (78.4%)",
      "6 (4.5%)", "31 (23.1%)", "109 (81.3%)", "", "72 (53.7%)",
      "0", "74 (55.2%)", "B: Placebo", "(N=134)", "123 (91.8%)",
      "622", "26 (19.40%)", "1 (0.75%)", "", "70 (52.2%)", "101 (75.4%)",
      "8 (6%)", "31 (23.1%)", "70 (52.2%)", "24 (17.9%)", "65 (48.5%)",
      "108 (80.6%)", "9 (6.7%)", "31 (23.1%)", "104 (77.6%)", "",
      "79 (59%)", "0", "80 (59.7%)", "C: Combination", "(N=132)",
      "120 (90.9%)", "703", "19 (14.39%)", "2 (1.52%)", "", "75 (56.8%)",
      "99 (75%)", "7 (5.3%)", "26 (19.7%)", "75 (56.8%)", "27 (20.5%)",
      "76 (57.6%)", "109 (82.6%)", "13 (9.8%)", "32 (24.2%)", "109 (82.6%)",
      "", "75 (56.8%)", "0", "87 (65.9%)"
    ),
    .Dim = c(22L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
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

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total AEs", "Total number of deaths", "Total number of patients withdrawn from study due to an AE",
      "Total number of patients withdrawn informed consent", "Total number of patients with at least one",
      "AE with fatal outcome", "Serious AE", "AE leading to withdrawal from treatment",
      "Related AE", "Grade 3-5 AE", "Grade 4/5 AE", "A: Drug X", "(N=134)",
      "122 (91%)", "609", "22 (16.42%)", "6 (4.48%)", "2 (1.49%)",
      "", "76 (56.7%)", "104 (77.6%)", "23 (17.2%)", "105 (78.4%)",
      "109 (81.3%)", "91 (67.9%)", "B: Placebo", "(N=134)", "123 (91.8%)",
      "622", "26 (19.40%)", "1 (0.75%)", "2 (1.49%)", "", "70 (52.2%)",
      "101 (75.4%)", "24 (17.9%)", "108 (80.6%)", "104 (77.6%)",
      "90 (67.2%)", "C: Combination", "(N=132)", "120 (90.9%)", "703",
      "19 (14.39%)", "2 (1.52%)", "0 (0.00%)", "", "75 (56.8%)", "99 (75%)",
      "27 (20.5%)", "109 (82.6%)", "109 (82.6%)", "93 (70.5%)"
    ),
    .Dim = c(14L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total AEs", "Total number of deaths", "Total number of patients withdrawn from study due to an AE",
      "Total number of patients with at least one", "AE with fatal outcome",
      "Serious AE", "AE leading to withdrawal from treatment", "AE leading to dose modification/interruption",
      "Related AE", "Grade 3-5 AE", "Total number of unique preferred terms which are",
      "Serious AE", "AE leading to dose modification/interruption",
      "Related AE", "Grade 3-5 AE", "Grade 4/5", "Total number of adverse events which are",
      "Serious AE", "AE leading to dose modification/interruption",
      "Related AE", "Grade 3-5 AE", "Grade 4/5", "A: Drug X", "(N=134)",
      "122 (91%)", "609", "22 (16.42%)", "6 (4.48%)", "", "76 (56.7%)",
      "104 (77.6%)", "23 (17.2%)", "72 (53.7%)", "105 (78.4%)",
      "109 (81.3%)", "", "4", "8", "5", "5", "3", "", "249", "109",
      "282", "303", "172", "B: Placebo", "(N=134)", "123 (91.8%)",
      "622", "26 (19.40%)", "1 (0.75%)", "", "70 (52.2%)", "101 (75.4%)",
      "24 (17.9%)", "65 (48.5%)", "108 (80.6%)", "104 (77.6%)",
      "", "4", "8", "5", "5", "3", "", "255", "100", "299", "291",
      "174", "C: Combination", "(N=132)", "120 (90.9%)", "703", "19 (14.39%)",
      "2 (1.52%)", "", "75 (56.8%)", "99 (75%)", "27 (20.5%)", "76 (57.6%)",
      "109 (82.6%)", "109 (82.6%)", "", "4", "8", "5", "5", "3",
      "", "282", "138", "336", "327", "197"
    ),
    .Dim = c(25L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
