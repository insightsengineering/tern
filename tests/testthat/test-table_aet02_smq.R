# Test all variants of AET02 SMQ

library(scda)

stack_adae_by_smq <- function(adae, smq) {
  l_df <- lapply(smq, function(ae_grp) {
    keep <- !(is.na(adae[[ae_grp]]))
    df <- adae[keep, ]
    df[["AE_GRP"]] <- ae_grp
    df
  })

  do.call(rbind, l_df)
}

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

testthat::test_that("AET02SMQ variant 1 is produced correctly", {
  adae <- stack_adae_by_smq(adae, c("SMQ01NAM"))

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )
    ) %>%
    split_rows_by(
      "AE_GRP",
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
        nonunique = "Overall total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AE_GRP"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AE_GRP", "*", "AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "",
      "Total number of patients with at least one adverse event", "SMQ01NAM",
      "Total number of patients with at least one adverse event", "Overall total number of events",
      "dcd B.2.2.3.1", "dcd C.1.1.1.3",
      "A: Drug X", "(N=134)",
      "72 (53.7%)", "",
      "72 (53.7%)", "119",
      "48 (35.8%)", "43 (32.1%)",
      "B: Placebo", "(N=134)",
      "79 (59%)", "",
      "79 (59%)", "139",
      "54 (40.3%)", "46 (34.3%)",
      "C: Combination", "(N=132)",
      "75 (56.8%)", "",
      "75 (56.8%)", "141",
      "51 (38.6%)", "43 (32.6%)"
    ),
    .Dim = c(8L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET02SMQ variant 2 is produced correctly", {
  adae <- stack_adae_by_smq(adae, c("SMQ01NAM", "CQ01NAM"))

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )
    ) %>%
    split_rows_by(
      "AE_GRP",
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
        nonunique = "Overall total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  result <- result %>%
    sort_at_path(path = c("AE_GRP"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AE_GRP", "*", "AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "",
      "",
      "Total number of patients with at least one adverse event",
      "CQ01NAM",
      "Total number of patients with at least one adverse event",
      "Overall total number of events",
      "dcd D.2.1.5.3",
      "dcd A.1.1.1.1",
      "SMQ01NAM",
      "Total number of patients with at least one adverse event",
      "Overall total number of events",
      "dcd B.2.2.3.1",
      "dcd C.1.1.1.3",
      "A: Drug X",
      "(N=134)",
      "95 (70.9%)",
      "",
      "74 (55.2%)",
      "126",
      "47 (35.1%)",
      "50 (37.3%)",
      "",
      "72 (53.7%)",
      "119",
      "48 (35.8%)",
      "43 (32.1%)",
      "B: Placebo",
      "(N=134)",
      "103 (76.9%)",
      "",
      "80 (59.7%)",
      "134",
      "58 (43.3%)",
      "45 (33.6%)",
      "",
      "79 (59%)",
      "139",
      "54 (40.3%)",
      "46 (34.3%)",
      "C: Combination",
      "(N=132)",
      "108 (81.8%)",
      "",
      "87 (65.9%)",
      "162",
      "57 (43.2%)",
      "63 (47.7%)",
      "",
      "75 (56.8%)",
      "141",
      "51 (38.6%)",
      "43 (32.6%)"
    ),
    .Dim = c(13L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
