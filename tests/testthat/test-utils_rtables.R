testthat::test_that("to_string_matrix works correctly", {
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE", mean, format = "xx.xx") %>%
    build_table(DM) %>%
    prune_table()

  # Initial intended use (wrapper of matrix_form(x)$strings)
  result <- to_string_matrix(tbl, with_spaces = FALSE, print_txt_to_copy = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Testing with spaces (respecting indentation and alignments)
  result <- to_string_matrix(tbl, with_spaces = TRUE, print_txt_to_copy = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Testing print_txt_to_copy with original table
  print_result <- capture.output(
    nowhere <- to_string_matrix(tbl, with_spaces = FALSE, print_txt_to_copy = TRUE)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(print_result)

  # Testing print_txt_to_copy with spaces
  print_result <- capture.output(
    nowhere <- to_string_matrix(tbl, with_spaces = TRUE, print_txt_to_copy = TRUE)
  )

  res <- testthat::expect_silent(print_result)
  testthat::expect_snapshot(res)
})

testthat::test_that("unlist_and_blank_na works as expected if not all missing", {
  x <- list(1, 3, 5, NA)
  result <- unlist_and_blank_na(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("unlist_and_blank_na works as expected if all missing", {
  x <- c(NA, NA)
  result <- unlist_and_blank_na(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("cfun_by_flag works as expected", {
  result_fun <- cfun_by_flag(analysis_var = "aval", flag_var = "is_result", format = "xx.xxxx")
  testthat::expect_type(result_fun, "closure")
  df <- data.frame(
    aval = c(1, 2, 3, 4, 5),
    arm = c("a", "a", "b", "b", "b"),
    is_result = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  result <- result_fun(df = df, labelstr = "bla")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("labels_or_names works correctly", {
  res <- testthat::expect_silent(labels_or_names(list(a = 5, b = formatters::with_label(3, "bla"))))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(labels_or_names(list(5, b = 3)))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(labels_or_names(list(formatters::with_label(1, "bli"), b = 3)))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(labels_or_names(list(1, 2)))
  testthat::expect_snapshot(res)
})

testthat::test_that("c_label_n works as expected", {
  result <- c_label_n(data.frame(a = c(1, 2)), "female", .N_row = 4)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("c_label_n_alt works as expected", {
  result <- c_label_n_alt(data.frame(a = c(1, 2)), "female", .alt_df_row = data.frame(a = 1:10))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("add_rowcounts works with one row split", {
  result <- basic_table() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("add_rowcounts works with multiple column and row splits", {
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA1") %>%
    split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    analyze("AGE", afun = mean, format = "xx.xx") %>%
    build_table(DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("add_rowcounts works with pruning", {
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    analyze("RACE") %>%
    build_table(DM) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  dm_f <- DM %>% dplyr::filter(SEX == "F")
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    analyze("RACE") %>%
    build_table(dm_f) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("add_rowcounts works with alt_counts = TRUE", {
  DM_alt <- DM[1:100, ] # nolint

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts(alt_counts = TRUE) %>%
    analyze("RACE") %>%
    build_table(DM, alt_counts_df = DM_alt) %>%
    prune_table()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_col_indices works as expected", {
  tab <- basic_table() %>%
    split_cols_by("ARM") %>%
    build_table(DM)
  result <- h_col_indices(tab, c("B: Placebo", "C: Combination"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("as.rtable.data.frame works correctly", {
  x <- data.frame(
    a = 1:10,
    b = seq(from = 10000, to = 20000, length = 10) / 1000
  )
  rownames(x) <- LETTERS[1:10]
  result <- as.rtable(x, format = "xx.x")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("as.rtable.data.frame fails when a column is not numeric", {
  x <- data.frame(
    a = 1:10,
    b = LETTERS[1:10]
  )
  testthat::expect_error(as.rtable(x))
})

testthat::test_that("as.rtable.data.frame uses variable labels for column headers when they are available", {
  x <- data.frame(
    a = 1:10,
    b = seq(from = 10000, to = 20000, length = 10) / 1000
  )
  formatters::var_labels(x) <- paste("label for", names(x))
  rownames(x) <- LETTERS[1:10]
  result <- as.rtable(x, format = "xx.x")

  res <- testthat::expect_silent(names(result))
  testthat::expect_snapshot(res)
})

testthat::test_that("h_split_param divides param values", {
  f <- list(
    surv = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci"),
    surv_diff = c("rate_diff", "rate_diff_ci", "ztest_pval")
  )

  .stats <- c("pt_at_risk", "rate_diff")
  result <- h_split_param(.stats, .stats, f = f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  .formats <- c("pt_at_risk" = "xx", "event_free_rate" = "xxx")
  result <- h_split_param(.formats, names(.formats), f = f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("afun_selected_stats works for NULL input", {
  result <- afun_selected_stats(NULL, "b")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("afun_selected_stats works for character input", {
  result <- afun_selected_stats(c("a", "c"), c("b", "c"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("append_varlabels works as expected", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("SEX") %>%
    append_varlabels(DM, "SEX") %>%
    analyze("AGE", afun = mean) %>%
    append_varlabels(DM, "AGE", indent = 1L)
  result <- build_table(lyt, DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("append_varlabels correctly concatenates multiple variable labels", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", afun = mean) %>%
    append_varlabels(DM, c("SEX", "AGE"))
  result <- build_table(lyt, DM)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("default na_str works properly", {
  tmp <- tern_ex_adsl[seq_len(10), seq_len(10)]
  tmp$AGE[1] <- NA
  df_to_tt(tmp)
  set_default_na_str("N/A")
  tbl <- basic_table() %>%
    split_rows_by("SEX") %>%
    split_cols_by("ARM") %>%
    analyze("AGE",
      afun = function(x) mean(x, na.rm = FALSE), inclNAs = TRUE,
      format = "xx.", na_str = default_na_str()
    ) %>%
    build_table(tmp)
  testthat::expect_identical(matrix_form(tbl)$strings[5, 2], "N/A")


  # lets try with some default function
  set_default_na_str(NULL)
  dt <- data.frame("VAR" = c(NA, NA_real_))
  tbl <- basic_table() %>%
    analyze_vars(vars = "VAR", .stats = c("n", "mean")) %>%
    build_table(dt)
  testthat::expect_identical(matrix_form(tbl)$strings[-1, 2], c("0", "NA"))

  set_default_na_str("<no-value>")
  dt <- data.frame("VAR" = c(NA, NA_real_))
  tbl <- basic_table() %>%
    analyze_vars(vars = "VAR", .stats = c("n", "mean")) %>%
    build_table(dt)
  testthat::expect_identical(matrix_form(tbl)$strings[-1, 2], c("0", "<no-value>"))
})
