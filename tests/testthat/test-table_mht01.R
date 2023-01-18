# Tests the single variant for MHT01

adsl <- adsl_raw
admh <- admh_raw

testthat::test_that("MHT01 variant 1 is produced accurately", {
  adsl_f <- adsl %>%
    dplyr::filter(SAFFL == "Y")

  admh_f <- admh %>%
    dplyr::filter(
      SAFFL == "Y",
      MHBODSYS != "",
      MHDECOD != ""
    )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    split_rows_by(
      var = "MHBODSYS",
      split_fun = drop_split_levels,
      child_labels = "visible",
      nested = FALSE,
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    count_occurrences(vars = "MHDECOD", .indent_mods = -1L)

  result <- build_table(lyt, admh_f, alt_counts_df = adsl_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("MHT01 variant 2 is produced accurately", {
  adsl_f <- adsl %>%
    dplyr::filter(SAFFL == "Y")

  admh_f <- admh %>%
    dplyr::filter(
      SAFFL == "Y",
      MHBODSYS != "",
      MHDECOD != ""
    )

  admh_f_prior <- admh_f %>%
    dplyr::filter(ASTDY <= 0)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    split_rows_by(
      var = "MHBODSYS",
      split_fun = drop_split_levels,
      child_labels = "visible",
      nested = FALSE,
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one event", "Total number of events")
    ) %>%
    count_occurrences(vars = "MHDECOD", .indent_mods = -1L)

  result <- build_table(lyt, admh_f_prior, alt_counts_df = adsl_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("MHT01 variant 3 is produced accurately", {
  adsl_f <- adsl %>%
    dplyr::filter(SAFFL == "Y")

  admh_f <- admh %>%
    dplyr::filter(
      SAFFL == "Y",
      MHBODSYS != "",
      MHDECOD != ""
    )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Total number of patients with at least one event")
    ) %>%
    split_rows_by(
      var = "MHBODSYS",
      split_fun = drop_split_levels,
      child_labels = "visible",
      nested = FALSE,
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Total number of patients with at least one event")
    ) %>%
    count_occurrences(vars = "MHDECOD", .indent_mods = -1L)

  result <- build_table(lyt, admh_f, alt_counts_df = adsl_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})

# MHT01 variant 4 can not be produced by current rtables
# Medical History with total number of conditions per body system after the summary of patients
# Not a blocker given it's just a cosmetic variant

testthat::test_that("MHT01 variant 5 is produced accurately", {
  adsl_f <- adsl %>%
    dplyr::filter(SAFFL == "Y")

  admh_f <- admh %>%
    dplyr::filter(
      SAFFL == "Y",
      MHBODSYS != "",
      MHDECOD != ""
    )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_overall_col("All Patients") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(unique = "Total number of patients with at least one event")
    ) %>%
    split_rows_by(
      var = "MHBODSYS",
      split_fun = drop_split_levels,
      child_labels = "visible",
      nested = FALSE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(unique = "Total number of patients with at least one event")
    ) %>%
    count_occurrences(vars = "MHDECOD", .indent_mods = -1L)

  result <- build_table(lyt, admh_f, alt_counts_df = adsl_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})
