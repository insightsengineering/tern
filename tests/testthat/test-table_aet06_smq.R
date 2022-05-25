# Test variants for AET06_SMQ.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

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
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "", "Total number of patients with at least one adverse event",
      "SMQ 1 (broad)", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd A.1.1.1.1", "dcd A.1.1.1.2", "dcd D.1.1.1.1",
      "dcd D.1.1.4.2", "dcd B.1.1.1.1", "dcd C.1.1.1.3", "SMQ 1 (narrow)",
      "Total number of patients with at least one adverse event", "Total number of events",
      "dcd A.1.1.1.1", "dcd A.1.1.1.2", "dcd D.1.1.1.1", "dcd D.1.1.4.2",
      "AESI", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd B.2.2.3.1", "dcd A.1.1.1.2", "dcd B.2.1.2.1",
      "dcd C.2.1.2.1", "A: Drug X", "F", "(N=79)", "72 (91.1%)", "",
      "69 (87.3%)", "229", "34 (43%)", "32 (40.5%)", "25 (31.6%)",
      "30 (38%)", "28 (35.4%)", "30 (38%)", "", "64 (81%)", "157",
      "34 (43%)", "32 (40.5%)", "25 (31.6%)", "30 (38%)", "", "59 (74.7%)",
      "155", "30 (38%)", "32 (40.5%)", "29 (36.7%)", "23 (29.1%)",
      "A: Drug X", "M", "(N=55)", "50 (90.9%)", "", "46 (83.6%)", "141",
      "16 (29.1%)", "16 (29.1%)", "25 (45.5%)", "18 (32.7%)", "19 (34.5%)",
      "13 (23.6%)", "", "36 (65.5%)", "102", "16 (29.1%)", "16 (29.1%)",
      "25 (45.5%)", "18 (32.7%)", "", "38 (69.1%)", "90", "18 (32.7%)",
      "16 (29.1%)", "20 (36.4%)", "12 (21.8%)", "B: Placebo", "F",
      "(N=82)", "74 (90.2%)", "", "70 (85.4%)", "245", "31 (37.8%)",
      "33 (40.2%)", "29 (35.4%)", "22 (26.8%)", "33 (40.2%)", "36 (43.9%)",
      "", "63 (76.8%)", "157", "31 (37.8%)", "33 (40.2%)", "29 (35.4%)",
      "22 (26.8%)", "", "66 (80.5%)", "176", "32 (39%)", "33 (40.2%)",
      "30 (36.6%)", "36 (43.9%)", "B: Placebo", "M", "(N=52)", "45 (86.5%)",
      "", "42 (80.8%)", "114", "14 (26.9%)", "15 (28.8%)", "13 (25%)",
      "20 (38.5%)", "16 (30.8%)", "10 (19.2%)", "", "36 (69.2%)", "79",
      "14 (26.9%)", "15 (28.8%)", "13 (25%)", "20 (38.5%)", "", "37 (71.2%)",
      "83", "22 (42.3%)", "15 (28.8%)", "14 (26.9%)", "12 (23.1%)",
      "C: Combination", "F", "(N=70)", "63 (90%)", "", "62 (88.6%)",
      "238", "33 (47.1%)", "24 (34.3%)", "27 (38.6%)", "27 (38.6%)",
      "24 (34.3%)", "27 (38.6%)", "", "53 (75.7%)", "159", "33 (47.1%)",
      "24 (34.3%)", "27 (38.6%)", "27 (38.6%)", "", "54 (77.1%)", "138",
      "26 (37.1%)", "24 (34.3%)", "22 (31.4%)", "30 (42.9%)", "C: Combination",
      "M", "(N=62)", "55 (88.7%)", "", "54 (87.1%)", "183", "30 (48.4%)",
      "26 (41.9%)", "24 (38.7%)", "23 (37.1%)", "19 (30.6%)", "16 (25.8%)",
      "", "54 (87.1%)", "136", "30 (48.4%)", "26 (41.9%)", "24 (38.7%)",
      "23 (37.1%)", "", "53 (85.5%)", "142", "25 (40.3%)", "26 (41.9%)",
      "30 (48.4%)", "25 (40.3%)"
    ),
    .Dim = c(27L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
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
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(unique = "Total number of patients with at least one adverse event")
    ) %>%
    split_rows_by(
      "SMQ",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = -1L,
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

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "", "Total number of patients with at least one adverse event",
      "SMQ 1 (broad)", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd A.1.1.1.1", "dcd A.1.1.1.2", "dcd D.1.1.1.1",
      "dcd D.1.1.4.2", "dcd B.1.1.1.1", "dcd C.1.1.1.3", "SMQ 1 (narrow)",
      "Total number of patients with at least one adverse event", "Total number of events",
      "dcd A.1.1.1.1", "dcd A.1.1.1.2", "dcd D.1.1.1.1", "dcd D.1.1.4.2",
      "AESI", "Total number of patients with at least one adverse event",
      "Total number of events", "dcd B.2.2.3.1", "dcd A.1.1.1.2", "dcd B.2.1.2.1",
      "dcd C.2.1.2.1", "A: Drug X", ">= 65", "(N=0)", "0", "", "0",
      "0", "0", "0", "0", "0", "0", "0", "", "0", "0", "0",
      "0", "0", "0", "", "0", "0", "0", "0", "0", "0", "A: Drug X",
      "< 65", "(N=134)", "122 (91%)", "", "115 (85.8%)", "370", "50 (37.3%)",
      "48 (35.8%)", "50 (37.3%)", "48 (35.8%)", "47 (35.1%)", "43 (32.1%)",
      "", "100 (74.6%)", "259", "50 (37.3%)", "48 (35.8%)", "50 (37.3%)",
      "48 (35.8%)", "", "97 (72.4%)", "245", "48 (35.8%)", "48 (35.8%)",
      "49 (36.6%)", "35 (26.1%)", "B: Placebo", ">= 65", "(N=0)", "0",
      "", "0", "0", "0", "0", "0", "0", "0", "0", "", "0",
      "0", "0", "0", "0", "0", "", "0", "0", "0", "0", "0",
      "0", "B: Placebo", "< 65", "(N=134)", "119 (88.8%)", "", "112 (83.6%)",
      "359", "45 (33.6%)", "48 (35.8%)", "42 (31.3%)", "42 (31.3%)",
      "49 (36.6%)", "46 (34.3%)", "", "99 (73.9%)", "236", "45 (33.6%)",
      "48 (35.8%)", "42 (31.3%)", "42 (31.3%)", "", "103 (76.9%)",
      "259", "54 (40.3%)", "48 (35.8%)", "44 (32.8%)", "48 (35.8%)",
      "C: Combination", ">= 65", "(N=1)", "1 (100%)", "", "1 (100%)",
      "2", "0", "0", "0", "1 (100%)", "1 (100%)", "0", "", "1 (100%)",
      "1", "0", "0", "0", "1 (100%)", "", "1 (100%)", "1", "0", "0",
      "1 (100%)", "0", "C: Combination", "< 65", "(N=131)", "117 (89.3%)",
      "", "115 (87.8%)", "419", "63 (48.1%)", "50 (38.2%)", "51 (38.9%)",
      "49 (37.4%)", "42 (32.1%)", "43 (32.8%)", "", "106 (80.9%)",
      "294", "63 (48.1%)", "50 (38.2%)", "51 (38.9%)", "49 (37.4%)",
      "", "106 (80.9%)", "279", "51 (38.9%)", "50 (38.2%)", "51 (38.9%)",
      "55 (42%)"
    ),
    .Dim = c(27L, 7L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
