testthat::test_that("s_count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )

  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_count_occurrences drops non appearing levels by default", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df)
  testthat::expect_false("MHX" %in% c(names(result$count), names(result$count_fraction), names(result$fraction)))
})

testthat::test_that("s_count_occurrences keeps non appearing levels if requested", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df, drop = FALSE)
  testthat::expect_true("MHX" %in% names(result$count))
  testthat::expect_true("MHX" %in% names(result$count_fraction))
  testthat::expect_true("MHX" %in% names(result$fraction))
})

testthat::test_that("s_count_occurrences fails when it receives empty .df_row and drop = TRUE", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  df_sub <- df[df$USUBJID == "5", ]
  testthat::expect_error(s_count_occurrences(
    df = df_sub,
    .N_col = 4L,
    .df_row = df_sub,
    drop = TRUE
  ))
})

testthat::test_that("s_count_occurrences functions as expected when requesting different denominator", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )

  result <- s_count_occurrences(df = df, denom = "n", .N_col = 4L, .df_row = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4, 6, 6, 6, 7, 7, 8)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3", "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"),
      levels = c("MH1", "MH2", "MH3", "MH4", "MHX")
    ),
    ARM = rep(c("A", "B"), each = 6)
  )
  df_adsl <- data.frame(
    USUBJID = 1:9,
    ARM = rep(c("A", "B"), c(5, 4))
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences(vars = "MHDECOD")

  result <- rtable_object <- lyt %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences functions as expected with label row specified", {
  df <- data.frame(
    USUBJID = as.character(c(1, 4, 4, 6, 6, 6, 7, 7, 8)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH1", "MH2", "MH1", "MH2"),
      levels = c("MH1", "MH2")
    )
  )
  df_adsl <- data.frame(
    USUBJID = 1:9
  )

  lyt <- basic_table() %>%
    count_occurrences(
      vars = "MHDECOD",
      var_labels = "MH Term",
      show_labels = "visible"
    )

  result <- rtable_object <- lyt %>%
    build_table(df, alt_counts_df = df_adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_occurrences can have totals", {
  library(tern)
  tbl <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("AEBODSYS", child_labels = "visible") %>%
    # summarize_num_patients("USUBJID", # Q: do we need this to be a label row (i.e. content row)?
    #                        .stats = c("unique", "nonunique"),
    #                        .labels = c("Total number of patients with at least one adverse event", "Total number of events")) %>%
    count_occurrences("AEDECOD", .indent_mods = -1L, add_total = TRUE) %>% # Q: do we always need this collapse?
    build_table(ex_adae)
  tbl[1:10,]
})
