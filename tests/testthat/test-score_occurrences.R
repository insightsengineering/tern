# Local data pre-processing
dfae_local <- local({
  set.seed(1)
  dfsl <- data.frame(
    USUBJID = as.character(c(1, 2, 3, 4, 5)),
    ARM = sample(c("A", "B", "C"), 5, replace = TRUE),
    stringsAsFactors = TRUE
  )
  dfae <- data.frame(
    USUBJID = factor(as.character(c(1, 2, 3, 4)), levels = as.character(c(1, 2, 3, 4, 5))),
    AEBODSYS = sample(c("AEBS1", "AEBS2"), 20, replace = TRUE),
    AEDECOD = sample(c("AEPT1", "AEPT2", "AEPT3"), 20, replace = TRUE),
    AESUPSYS = sample(c("AESS1", "AESS2"), 20, replace = TRUE)
  )
  dfae <- dfae %>% dplyr::arrange(USUBJID, AEBODSYS, AEDECOD) # nolint
  dfae <- dplyr::left_join(dfae, dfsl, by = "USUBJID")
  structure(
    dfae,
    dfsl = dfsl
  )
})

full_table <- local({
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )
    ) %>%
    split_rows_by(
      var = "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      indent_mod = 1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD")

  dfae <- dfae_local # nolint
  build_table(lyt, dfae, alt_counts_df = attr(dfae, "dfsl")) %>%
    prune_table()
})

full_table_with_empty <- local({
  dfae <- dfae_local %>%
    df_explicit_na()
  # add empty level for class
  levels(dfae$AEBODSYS) <- c(levels(dfae$AEBODSYS), "EMPTY_LEVEL")

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by(
      var = "AEBODSYS", child_labels = "visible", nested = FALSE,
      split_fun = trim_levels_in_group("AEDECOD", drop_outlevs = FALSE)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", drop = FALSE)

  build_table(lyt, dfae, alt_counts_df = attr(dfae, "dfsl"))
})

testthat::test_that("score_occurrences functions as expected", {
  sorted_table <- full_table %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences
    )

  res <- testthat::expect_silent(sorted_table)
  testthat::expect_snapshot(res)
})

testthat::test_that("score_occurrences functions as expected with empty analysis rows", {
  sorted_table <- full_table_with_empty %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences,
      na.pos = "omit"
    )

  res <- testthat::expect_silent(sorted_table)
  testthat::expect_snapshot(res)
})

testthat::test_that("score_occurrences_cols functions as expected", {
  score_col_c <- score_occurrences_cols(col_names = "C")
  testthat::expect_type(score_col_c, "closure")

  sorted_table <- full_table %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_col_c)

  res <- testthat::expect_silent(sorted_table)
  testthat::expect_snapshot(res)
})

testthat::test_that("score_occurrences_subtable functions as expected", {
  dfae <- dfae_local

  full_table_dfae <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE) %>%
    count_occurrences(vars = "AEDECOD") %>%
    build_table(dfae, alt_counts_df = attr(dfae, "dfsl")) %>%
    prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(full_table_dfae))
  testthat::expect_type(score_subtable_all, "closure")

  sorted_table <- full_table_dfae %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = score_subtable_all, decreasing = FALSE)

  res <- testthat::expect_silent(sorted_table)
  testthat::expect_snapshot(res)
})

testthat::test_that("score_occurrences_cont_cols functions as expected", {
  set.seed(1)
  dfae <- dfae_local
  dfae <- dfae %>%
    dplyr::mutate(USUBJID = factor(sample(1:10, size = nrow(dfae), replace = TRUE)))

  full_table_dfae <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AESUPSYS", child_labels = "visible") %>%
    summarize_num_patients("USUBJID") %>%
    build_table(df = dfae)

  score_cont_cols <- score_occurrences_cont_cols(col_names = c("A", "B"))
  testthat::expect_type(score_cont_cols, "closure")

  sorted_table <- full_table_dfae %>%
    sort_at_path(path = c("AESUPSYS"), scorefun = score_cont_cols, decreasing = TRUE)

  res <- testthat::expect_silent(sorted_table)
  testthat::expect_snapshot(res)
})
