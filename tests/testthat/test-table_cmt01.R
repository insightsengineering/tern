adsl <- adsl_raw
adcm <- adcm_raw

testthat::test_that("CMT01 default variant (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% dplyr::filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels, child_labels = "visible",
      nested = FALSE,
      indent_mod = 1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01 variant 1 (prior medications) is produced correctly", {
  adcm_p <- adcm %>% dplyr::filter(ATIREL == "PRIOR")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels, child_labels = "visible",
      nested = FALSE,
      indent_mod = 1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_p, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01 variant 3 (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% dplyr::filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels,
      child_labels = "visible",
      nested = FALSE,
      indent_mod = 1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c(unique = "Total number of patients with at least one treatment"),
      .stats = "unique"
    ) %>%
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("CMT01 variant 4 (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% dplyr::filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one treatment (%)", "Total number of treatments")
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels,
      child_labels = "visible",
      indent_mod = 1L,
      nested = FALSE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one treatment (%)", "Total number of treatments")
    ) %>%
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences, decreasing = TRUE) %>%
    sort_at_path(path = c("CMCLAS"), scorefun = cont_n_onecol(4), decreasing = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
