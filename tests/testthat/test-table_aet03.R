adsl <- adsl_raw
adae <- adae_raw

testthat::test_that("AET03 variant 1 is produced correctly", {
  adae$AEDECOD <- as.character(adae$AEDECOD) # nolint
  adae$AEBODSYS <- as.character(adae$AEBODSYS) # nolint
  adae$ASEV <- as.character(adae$AESEV) # nolint
  adae$ASEV[1:15] <- "LIFE THREATENING"
  adae$ASEV <- factor(adae$ASEV, levels = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING")) # nolint

  n_per_arm <- table(adsl$ACTARM)

  gr_grp <- list(
    "- Any Intensity -" = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING")
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_occurrences_by_grade(
      var = "ASEV",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("ASEV"),
      child_labels = "visible", nested = TRUE
    ) %>%
    summarize_occurrences_by_grade(
      var = "ASEV",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("ASEV"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c("- Any Intensity -")
    ) %>%
    count_occurrences_by_grade(var = "ASEV", .indent_mods = -1L)

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_matrix <- to_string_matrix(result)

  # Pagination also works (and sorting)
  pag_result <- paginate_table(result, lpp = 15)

  testthat::expect_identical(
    to_string_matrix(pag_result[[3]])[3, 1],
    "cl B.2"
  )
  testthat::expect_identical(
    to_string_matrix(pag_result[[1]])[3:4, 1],
    c("- Any Intensity -", "MILD")
  )
})
