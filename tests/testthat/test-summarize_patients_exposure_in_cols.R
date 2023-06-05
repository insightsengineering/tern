set.seed(1)
anl_local <- data.frame(
  USUBJID = c(paste("id", seq(1, 12), sep = "")),
  ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
  SEX = c(rep("Female", 6), rep("Male", 6)),
  AVAL = as.numeric(sample(seq(1, 5), 12, replace = TRUE)),
  stringsAsFactors = TRUE
)

adsl_local <- data.frame(
  USUBJID = c(paste("id", seq(1, 12), sep = "")),
  ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
  SEX = c(rep("Female", 6), rep("Male", 6)),
  stringsAsFactors = TRUE
)

testthat::test_that("s_count_patients_sum_exposure works as expected", {
  df <- anl_local
  adsl <- adsl_local
  result <- s_count_patients_sum_exposure(df = df, .N_col = nrow(adsl), .stats = c("n_patients", "sum_exposure"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_count_patients_sum_exposure works as expected", {
  result <- a_count_patients_sum_exposure(
    df = anl_local,
    var = "SEX",
    .N_col = nrow(adsl_local),
    .stats = "n_patients"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_patients_exposure_in_cols works well with default arguments", {
  df <- anl_local
  adsl <- adsl_local

  result <- basic_table() %>%
    split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
    summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE) %>%
    analyze_patients_exposure_in_cols(var = "SEX", col_split = FALSE) %>%
    build_table(df = df, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_patients_exposure_in_cols works well with custom arguments", {
  df <- anl_local
  adsl <- adsl_local

  result <- basic_table() %>%
    split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL",
      col_split = TRUE,
      custom_label = "xyz",
      .stats = "sum_exposure"
    ) %>%
    analyze_patients_exposure_in_cols(
      var = "SEX",
      col_split = FALSE,
      .stats = "sum_exposure"
    ) %>%
    build_table(df = df, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "summarize_patients_exposure_in_cols returns correct column label when no variable split and only one statistic",
  code = {
    df <- anl_local
    adsl <- adsl_local

    table <- basic_table() %>%
      summarize_patients_exposure_in_cols(
        var = "AVAL",
        col_split = TRUE,
        custom_label = "xyz",
        .stats = "n_patients"
      ) %>%
      build_table(df = df, alt_counts_df = adsl)

    invisible(capture.output({
      result <- col_paths_summary(table)$label
    }))

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("analyze_patients_exposure_in_cols works well with default arguments", {
  df <- anl_local
  adsl <- adsl_local

  result <- basic_table() %>%
    split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
    analyze_patients_exposure_in_cols(var = "SEX", col_split = TRUE) %>%
    build_table(df = df, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("analyze_patients_exposure_in_cols works well with custom arguments", {
  df <- anl_local
  adsl <- adsl_local

  result <- basic_table() %>%
    split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
    analyze_patients_exposure_in_cols(
      var = "SEX",
      col_split = TRUE,
      .stats = "sum_exposure"
    ) %>%
    build_table(df = df, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "analyze_patients_exposure_in_cols works with no variable split and only one statistic",
  code = {
    df <- anl_local
    adsl <- adsl_local

    table <- basic_table() %>%
      analyze_patients_exposure_in_cols(
        ex_var = "AVAL",
        col_split = TRUE,
        .stats = "n_patients"
      ) %>%
      build_table(df = df, alt_counts_df = adsl)

    res <- testthat::expect_silent(table)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("patients_exposure_in_cols works with totals after the row split", {
  # Fixes adding total as last analyze level, issue #950
  lyt <- basic_table(
    title = "Extent of Exposure",
    main_footer = "* Patient Time is the sum of patients and times",
    show_colcounts = TRUE
  ) %>%
    analyze_patients_exposure_in_cols(
      var = "SEX",
      col_split = TRUE,
      add_total_level = TRUE,
      custom_label = "REAL TOTAL"
    ) %>%
    append_topleft(c("", "Sex"))

  tbl <- build_table(lyt, anl_local)

  res <- testthat::expect_silent(tbl)
  testthat::expect_snapshot(res)
})
