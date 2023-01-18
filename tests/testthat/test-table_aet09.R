adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("AET09 variant 1 is produced correctly, AE related to study drug", {
  adae_r <- adae[adae$AEREL == "Y", ]

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae_r, alt_counts_df = adsl)

  result <- result %>%
    prune_table() %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_onecol(4)) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET09 variant 2 is produced correctly, AE related to study drug (including high-level terms)", {
  adae_r <- adae[adae$AEREL == "Y", ]

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))

  result <- build_table(lyt, adae_r, alt_counts_df = adsl) %>%
    prune_table()

  result <- result %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"), scorefun = score_occurrences)

  res <- expect_silent(result)
  expect_snapshot(res)
})
