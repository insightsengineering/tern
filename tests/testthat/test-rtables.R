testthat::test_that("to_string_matrix works correctly", {
  x <- basic_table() %>%
    analyze("AGE", mean, var_labels = "Age", format = "xx.xx") %>%
    build_table(DM)
  result <- to_string_matrix(x)
  expected <- matrix(
    c("", "all obs", "mean", "34.22"),
    byrow = TRUE,
    nrow = 2,
    ncol = 2
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("unlist_and_blank_na works as expected if not all missing", {
  x <- list(1, 3, 5, NA)
  result <- unlist_and_blank_na(x)
  expected <- c(1, 3, 5, NA)
  testthat::expect_identical(result, expected)
})

testthat::test_that("unlist_and_blank_na works as expected if all missing", {
  x <- c(NA, NA)
  result <- unlist_and_blank_na(x)
  expected <- character()
  testthat::expect_identical(result, expected)
})

testthat::test_that("cfun_by_flag works as expected", {
  result_fun <- cfun_by_flag(analysis_var = "aval", flag_var = "is_result", format = "xx.xxxx")
  testthat::expect_is(result_fun, "function")
  df <- data.frame(
    aval = c(1, 2, 3, 4, 5),
    arm = c("a", "a", "b", "b", "b"),
    is_result = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  result <- result_fun(df = df, labelstr = "bla")
  expected <- CellValue(
    1,
    format = "xx.xxxx",
    colspan = 1L,
    indent_mod = 0L,
    label = "bla"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("labels_or_names works correctly", {
  testthat::expect_identical(
    labels_or_names(list(a = 5, b = with_label(3, "bla"))),
    c(a = "a", b = "bla")
  )
  testthat::expect_identical(
    labels_or_names(list(5, b = 3)),
    c("", b = "b")
  )
  testthat::expect_identical(
    labels_or_names(list(with_label(1, "bli"), b = 3)),
    c("bli", b = "b")
  )
  testthat::expect_identical(
    labels_or_names(list(1, 2)),
    c("", "")
  )
})

testthat::test_that("c_label_n works as expected", {
  result <- c_label_n(data.frame(a = c(1, 2)), "female", .N_row = 4)
  expected <- CellValue(val = NULL, label = "female (N=4)")
  testthat::expect_identical(result, expected)
})

testthat::test_that("add_rowcounts works with one row split", {
  result <- basic_table() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    build_table(DM)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "F (N=187)", "M (N=169)", "all obs", "", ""),
    .Dim = 3:2
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("add_rowcounts works with multiple column and row splits", {
  skip_if_fail_rtables_refactor()

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA1") %>%
    split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    analyze("AGE", afun = mean, format = "xx.xx") %>%
    build_table(DM)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "CHN (N=179)", "F (N=94)", "mean", "M (N=85)",
      "mean", "USA (N=44)", "F (N=24)", "mean", "M (N=20)", "mean",
      "BRA (N=29)", "F (N=15)", "mean", "M (N=14)", "mean", "PAK (N=28)",
      "F (N=12)", "mean", "M (N=16)", "mean", "NGA (N=24)", "F (N=13)",
      "mean", "M (N=11)", "mean", "RUS (N=20)", "F (N=10)", "mean",
      "M (N=10)", "mean", "JPN (N=18)", "F (N=9)", "mean", "M (N=9)",
      "mean", "GBR (N=7)", "F (N=6)", "mean", "M (N=1)", "mean", "CAN (N=7)",
      "F (N=4)", "mean", "M (N=3)", "mean", "A: Drug X", "A", "", "",
      "30.92", "", "36.29", "", "", "33", "", "35", "", "", "29", "",
      "NaN", "", "", "NaN", "", "36.67", "", "", "26.5", "", "32",
      "", "", "30", "", "27", "", "", "NaN", "", "33", "", "", "NaN",
      "", "NaN", "", "", "32.5", "", "NaN", "A: Drug X", "B", "", "",
      "36.91", "", "38", "", "", "43", "", "36.5", "", "", "28.75",
      "", "31", "", "", "35", "", "37", "", "", "28.5", "", "37", "",
      "", "36.5", "", "NaN", "", "", "35", "", "NaN", "", "", "32",
      "", "NaN", "", "", "43", "", "NaN", "A: Drug X", "C", "", "",
      "35.36", "", "39.46", "", "", "41.33", "", "35.5", "", "", "47",
      "", "33", "", "", "NaN", "", "33", "", "", "24", "", "42.5",
      "", "", "32.75", "", "39", "", "", "NaN", "", "26.5", "", "",
      "NaN", "", "NaN", "", "", "NaN", "", "NaN", "B: Placebo", "A",
      "", "", "34.33", "", "30", "", "", "27.5", "", "40", "", "",
      "31", "", "32.33", "", "", "46", "", "28", "", "", "31", "",
      "NaN", "", "", "NaN", "", "30", "", "", "NaN", "", "29.5", "",
      "", "29", "", "NaN", "", "", "NaN", "", "NaN", "B: Placebo",
      "B", "", "", "32.89", "", "32", "", "", "NaN", "", "34", "",
      "", "30.5", "", "31.67", "", "", "29.67", "", "NaN", "", "",
      "NaN", "", "21", "", "", "NaN", "", "36.5", "", "", "41.5", "",
      "27.67", "", "", "NaN", "", "NaN", "", "", "30", "", "38", "B: Placebo",
      "C", "", "", "39.75", "", "32.8", "", "", "32.25", "", "28",
      "", "", "24", "", "35", "", "", "42", "", "32", "", "", "NaN",
      "", "37", "", "", "40", "", "28", "", "", "35", "", "NaN", "",
      "", "NaN", "", "NaN", "", "", "NaN", "", "NaN", "C: Combination",
      "A", "", "", "35.33", "", "34.82", "", "", "34.2", "", "39.25",
      "", "", "37", "", "NaN", "", "", "44", "", "41", "", "", "32",
      "", "35", "", "", "NaN", "", "35.5", "", "", "43", "", "33",
      "", "", "30", "", "NaN", "", "", "NaN", "", "29.5", "C: Combination",
      "B", "", "", "33.4", "", "33", "", "", "37", "", "31", "", "",
      "39", "", "48", "", "", "25.5", "", "38.5", "", "", "40", "",
      "44", "", "", "36.5", "", "27", "", "", "40", "", "NaN", "",
      "", "NaN", "", "30", "", "", "NaN", "", "NaN", "C: Combination",
      "C", "", "", "34.75", "", "31.87", "", "", "36", "", "39.5",
      "", "", "34", "", "31.67", "", "", "34", "", "36.33", "", "",
      "30.5", "", "NaN", "", "", "NaN", "", "27", "", "", "32.5", "",
      "NaN", "", "", "NaN", "", "NaN", "", "", "NaN", "", "NaN"
    ),
    .Dim = c(47L, 10L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_col_indices works as expected", {
  tab <- basic_table() %>%
    split_cols_by("ARM") %>%
    build_table(DM)
  result <- h_col_indices(tab, c("B: Placebo", "C: Combination"))
  expected <- c(2L, 3L)
  testthat::expect_identical(result, expected)
})


testthat::test_that("as.rtable.data.frame works correctly", {
  skip_if_fail_rtables_refactor()

  x <- data.frame(
    a = 1:10,
    b = seq(from = 10000, to = 20000, length = 10) / 1000
  )
  rownames(x) <- LETTERS[1:10]
  result <- as.rtable(x, format = "xx.x")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A", "B", "C", "D", "E", "F", "G", "H", "I",
      "J", "a", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
      "b", "10", "11.1", "12.2", "13.3", "14.4", "15.6", "16.7", "17.8",
      "18.9", "20"
    ),
    .Dim = c(11L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
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
  var_labels(x) <- paste("label for", names(x))
  rownames(x) <- LETTERS[1:10]
  result <- as.rtable(x, format = "xx.x")
  testthat::expect_identical(names(result), c("label for a", "label for b"))
})

testthat::test_that("h_split_param divides param values", {
  f <- list(
    surv = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci"),
    surv_diff = c("rate_diff", "rate_diff_ci", "ztest_pval")
  )

  .stats <- c("pt_at_risk", "rate_diff")
  result <- h_split_param(.stats, .stats, f = f)
  expected <- list(
    surv = "pt_at_risk",
    surv_diff = "rate_diff"
  )
  testthat::expect_identical(result, expected)

  .formats <- c("pt_at_risk" = "xx", "event_free_rate" = "xxx")
  result <- h_split_param(.formats, names(.formats), f = f)
  expected <- list(
    surv = c("pt_at_risk" = "xx", "event_free_rate" = "xxx"),
    surv_diff = NULL
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("afun_selected_stats works for NULL input", {
  result <- afun_selected_stats(NULL, "b")
  expected <- "b"
  testthat::expect_identical(result, expected)
})

testthat::test_that("afun_selected_stats works for character input", {
  result <- afun_selected_stats(c("a", "c"), c("b", "c"))
  expected <- "c"
  testthat::expect_identical(result, expected)
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "SEX", "  Age", "F", "mean", "M", "mean", "U", "mean",
      "UNDIFFERENTIATED", "mean", "A: Drug X", "(N=121)", "", "33.7142857142857",
      "", "36.5490196078431", "", "NA", "", "NA", "B: Placebo", "(N=106)",
      "", "33.8392857142857", "", "32.1", "", "NA", "", "NA", "C: Combination",
      "(N=129)", "", "34.8852459016393", "", "34.2794117647059", "",
      "NA", "", "NA"
    ),
    .Dim = c(10L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("append_varlabels correctly concatenates multiple variable labels", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", afun = mean) %>%
    append_varlabels(DM, c("SEX", "AGE"))
  result <- build_table(lyt, DM)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "SEX / Age", "F", "mean", "M", "mean", "U", "mean",
      "UNDIFFERENTIATED", "mean", "A: Drug X", "", "33.7142857142857",
      "", "36.5490196078431", "", "NA", "", "NA", "B: Placebo", "",
      "33.8392857142857", "", "32.1", "", "NA", "", "NA", "C: Combination",
      "", "34.8852459016393", "", "34.2794117647059", "", "NA", "",
      "NA"
    ),
    .Dim = c(9L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
