# Test all variants of AET02 SMQ
stack_adae_by_smq <- function(adae, smq) {
  l_df <- lapply(smq, function(ae_grp) {
    keep <- !(is.na(adae[[ae_grp]]))
    df <- adae[keep, ]
    df[["AE_GRP"]] <- ae_grp
    df
  })

  do.call(rbind, l_df)
}

adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("AET02SMQ variant 1 is produced correctly", {
  adae <- stack_adae_by_smq(adae, c("SMQ01NAM"))

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )
    ) %>%
    split_rows_by(
      "AE_GRP",
      child_labels = "visible",
      nested = FALSE,
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

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET02SMQ variant 2 is produced correctly", {
  adae <- stack_adae_by_smq(adae, c("SMQ01NAM", "CQ01NAM"))

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )
    ) %>%
    split_rows_by(
      "AE_GRP",
      child_labels = "visible",
      nested = FALSE,
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

  res <- expect_silent(result)
  expect_snapshot(res)
})
