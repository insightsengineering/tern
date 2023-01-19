# Test variants 1 and 5 for AET06. Variants 2-4 are essentially identical
# to variant 1, needing only the data frame to be pre-processed
# and the baseline variable to be changed from SEX.

adsl <- adsl_raw
adae <- adae_raw
adsub <- adsub_raw
adsub_bmi <- adsub %>%
  dplyr::filter(PARAMCD == "BBMISI") %>%
  dplyr::select(STUDYID, USUBJID, AVALCAT1) %>%
  dplyr::mutate(
    AVALCAT1 = factor(AVALCAT1, levels = c("<18.5", "18.5 - 24.9", "25 - 29.9", ">30"))
  )
adsl <- adsl %>%
  dplyr::mutate(
    RACE1 = dplyr::case_when(
      RACE == "WHITE" ~ "WHITE",
      TRUE ~ "NON-WHITE"
    ),
    RACE1 = factor(
      RACE1,
      levels = c("WHITE", "NON-WHITE")
    )
  ) %>%
  dplyr::left_join(
    y = adsub_bmi,
    by = c("STUDYID", "USUBJID")
  )

adae_labels <- var_labels(adae)

adae <- adae %>%
  dplyr::mutate(
    RACE1 = dplyr::case_when(
      RACE == "WHITE" ~ "WHITE",
      TRUE ~ "NON-WHITE"
    ),
    RACE1 = factor(
      RACE1,
      levels = c("WHITE", "NON-WHITE")
    )
  ) %>%
  dplyr::left_join(
    y = adsub_bmi,
    by = c("STUDYID", "USUBJID")
  )

testthat::test_that("AET06 variant 1 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      indent_mod = 1L
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

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET06 variant 3 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("AVALCAT1") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(
    lyt = lyt,
    df = adae,
    alt_counts_df = adsl
  ) %>%
    prune_table() %>%
    # Sorted by decreasing frequency across all groups by System Organ Class and Preferred Term.
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = cont_n_allcols
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("AET06 variant 5 is produced correctly", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
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
    count_occurrences("AEDECOD")

  result <- build_table(lyt, adae, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
