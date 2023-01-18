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

  res <- expect_silent(result)
  expect_snapshot(res)
})
