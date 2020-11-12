test_that("to_string_matrix works correctly", {
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
  expect_identical(result, expected)
})

test_that("flatten_list does not change input if there is just one list level", {
  x <- list(
    list(a = c(1, 2)),
    list(a = c(b = 1, c = 2), b = c(a = 3, d = 4)),
    list(a = with_label(c(1, 2), "bla"), b = with_label(c(a = 3, d = 4), "bli"))
  )
  purrr::map(
    x,
    ~expect_identical(flatten_list(.), .)
  )
})

test_that("flatten_list correctly flattens one list level only", {
  x <- list(
    a = list(
      1,
      2
    ),
    b = c(1, 2),
    c = list(
      list(
        3,
        4
      )
    )
  )
  result <- flatten_list(x)
  expected <- list(
    1,
    2,
    b = c(1, 2),
    list(3, 4)
  )
  expect_identical(result, expected)
})

test_that("list_length works correctly", {
  expect_identical(
    list_length(c(1, 2)),
    1L
  )
  expect_identical(
    list_length(list(a = c(1, 2), b = c(3, 4))),
    2L
  )
})

test_that("list_lengths_in_list works correctly", {
  x <- list(
    1,
    list(2, 3),
    c(4, 5),
    list(list(1, 2), list(3, 4))
  )
  result <- list_lengths_in_list(x)
  expected <- c(1L, 2L, 1L, 2L)
  expect_identical(result, expected)
})

test_that("labels_or_names works correctly", {
  expect_identical(
    labels_or_names(list(a = 5, b = with_label(3, "bla"))),
    c(a = "a", b = "bla")
  )
  expect_identical(
    labels_or_names(list(5, b = 3)),
    c("", b = "b")
  )
  expect_identical(
    labels_or_names(list(with_label(1, "bli"), b = 3)),
    c("bli", b = "b")
  )
  expect_identical(
    labels_or_names(list(1, 2)),
    c("", "")
  )
})

test_that("identical_without_attr works correctly", {
  x <- structure(
    "bla",
    a1 = "bli",
    a2 = "foo"
  )
  y <- "bla"
  expect_false(identical(x, y))
  expect_true(identical_without_attr(x, y))

  x <- "bla"
  class(x) <- "foo"
  y <- "bla"
  expect_false(identical(x, y))
  expect_false(isTRUE(all.equal(x, y, check.attributes = FALSE)))
  expect_true(identical_without_attr(x, y))
})

test_that("identical_without_attr works only on top level for lists, not in elements", {
  x <- list(
    a = with_label(1, "one"),
    b = with_label(2, "two")
  )
  y <- list(
    a = with_label(1, "ONE"),
    b = with_label(2, "TWO")
  )
  expect_false(identical(x, y))
  expect_false(identical_without_attr(x, y))

  x <- structure(
    list(
      a = 1,
      b = 2
    ),
    foo = "bar"
  )
  y <- structure(
    list(
      a = 1,
      b = 2
    ),
    zoo = "car"
  )
  expect_false(identical(x, y))
  expect_true(identical_without_attr(x, y))
})

test_that("format_wrap_df works with healthy input", {
  sfun <- function(df) {
    assert_that(is.data.frame(df))
    list(
      nrows = nrow(df),
      ncols = length(df)
    )
  }
  indent_mods <- c(nrows = 0L, ncols = 2L)
  formats <- c(nrows = "xx.", ncols = "xx.xx")

  afun <- expect_silent(
    format_wrap_df(
      sfun,
      indent_mods,
      formats
    )
  )

  expect_is(afun, "function")
  expect_identical(
    names(formals(afun)),
    c("df", "...", ".stats", ".indent_mods", ".formats", ".labels")
  )

  # Make sure function works with defaults.
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
  )
  result <- afun(df)
  expected <- in_rows(
    .list = list(
      nrows = CellValue(4L, format = "xx.", label = "nrows"),
      ncols = CellValue(3L, format = "xx.xx", label = "  ncols")
    )
  )
  expect_identical(result, expected)

  # And with custom overwrites.
  result <- afun(
    df,
    .stats = "ncols",
    .labels = c(ncols = "number columns"),
    .formats = c(ncols = "xx"),
    .indent_mods = c(ncols = 1)
  )
  expected <- in_rows(
    .list = list(
      ncols = CellValue(3L, "xx", label = " number columns")
    )
  )
  expect_identical(result, expected)
})

test_that("format_wrap_df processes additional rtables arguments correctly", {
  sfun <- function(df, .in_ref_col, .N_col) {  #nolint
    assert_that(is.data.frame(df))
    list(
      nrows = nrow(df),
      ncols = length(df),
      incol = .in_ref_col,
      nincol = .N_col
    )
  }
  indent_mods <- c(nrows = 0L, ncols = 2L, incol = 0L, nincol = 0L)
  formats <- c(nrows = "xx.", ncols = "xx.xx", incol = "xx", nincol = "xx")

  afun <- expect_silent(
    format_wrap_df(
      sfun,
      indent_mods,
      formats
    )
  )

  # Make sure the function signature is correct.
  expect_is(afun, "function")
  expect_identical(
    names(formals(afun)),
    c("df", "...", ".stats", ".indent_mods", ".formats", ".labels", ".in_ref_col", ".N_col")
  )

  # Make sure function works with defaults.
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
  )
  result <- afun(df, .in_ref_col = FALSE, .N_col = 3)
  expected <- in_rows(
    .list = list(
      nrows = CellValue(4L, "xx.", label = "nrows"),
      ncols = CellValue(3L, "xx.xx", label = "  ncols"),
      incol = CellValue(FALSE, "xx", label = "incol"),
      nincol = CellValue(3, "xx", label = "nincol")
    )
  )
  expect_identical(result, expected)
})

test_that("format_wrap_x works with healthy input", {
  sfun <- function(x) {
    list(
      n = length(x),
      mean = mean(x),
      median = median(x)
    )
  }
  indent_mods <- c(n = 0L, mean = 2L, median = 1L)
  formats <- c(n = "xx.", mean = "xx.xx", median = "xx")

  afun <- expect_silent(
    format_wrap_x(
      sfun,
      indent_mods,
      formats
    )
  )

  # Make sure the function signature is correct.
  expect_is(afun, "function")
  expect_identical(
    names(formals(afun)),
    c("x", "...", ".stats", ".indent_mods", ".formats", ".labels")
  )

  # Make sure function works with defaults.
  x <- c(1, 0, -1, 2, 5, 3, 2.5, 7.1)
  result <- afun(x)
  expected <- in_rows(
    .list = list(
      n = CellValue(8L, "xx.", label = "n"),
      mean = CellValue(2.45, "xx.xx", label = "  mean"),
      median = CellValue(2.25, "xx", label = " median")
    )
  )
  expect_identical(result, expected)

  # And with custom overwrites.
  result <- afun(
    x,
    .indent_mods = c(median = 3L),
    .stats = c("n", "median"),
    .formats = c(median = "xx.xx"),
    .labels = c(n = "Number of numbers")
  )
  expected <- in_rows(
    .list = list(
      n = CellValue(8L, "xx.", label = "Number of numbers"),
      median = CellValue(2.25, "xx.xx", label = "   median")
    )
  )
  expect_identical(result, expected)
})

test_that("format_wrap_x correctly processes the required rtables arguments", {
  # Function which requires rtables arguments .var and .N_total.
  sfun <- function(x, .var, .N_total) {  #nolint
    list(
      n = length(x),
      mean = mean(x),
      median = median(x),
      var = .var,
      N = .N_total
    )
  }
  indent_mods <- c(n = 0L, mean = 2L, median = 1L, var = 0L, N = 2L)
  formats <- c(n = "xx.", mean = "xx.xx", median = "xx", var = "xx", N = "xx")

  afun <- expect_silent(
    format_wrap_x(
      sfun,
      indent_mods,
      formats
    )
  )

  # Check that the selection worked.
  expect_identical(
    get("selected_arg_names", envir = environment(afun)),
    c(".var", ".N_total")
  )

  # Make sure the function signature is correct.
  expect_is(afun, "function")
  expect_identical(
    names(formals(afun)),
    c("x", "...",
      ".stats", ".indent_mods", ".formats", ".labels",
      ".var", ".N_total")
  )

  # Make sure function works with defaults.
  x <- c(1, 0, -1, 2, 5, 3, 2.5, 7.1)
  result <- afun(x, .var = "bla", .N_total = 10)
  expected <- in_rows(
    .list = list(
      n = CellValue(8L, "xx.", label = "n"),
      mean = CellValue(2.45, "xx.xx", label = "  mean"),
      median = CellValue(2.25, "xx", label = " median"),
      var = CellValue("bla", "xx", label = "var"),
      N = CellValue(10, "xx", label = "  N")
    )
  )
  expect_identical(result, expected)
})

test_that("format_wrap_x produces empty cells and keeps labels when applied to empty string results", {
  # sfun which returns empty string results when `in_ref` (toy example resembling comparison problems).
  sfun <- function(x, in_ref = FALSE) {
    list(
      n = with_label(`if`(in_ref, "", length(x)), "Number of patients"),
      mean = with_label(`if`(in_ref, "", mean(x)), "Mean"),
      median = with_label(`if`(in_ref, "", median(x)), "Median")
    )
  }
  indent_mods <- c(n = 0L, mean = 2L, median = 1L)
  formats <- c(n = "xx.", mean = "xx.xx", median = "xx")

  afun <- expect_silent(
    format_wrap_x(
      sfun,
      indent_mods,
      formats
    )
  )

  # Make sure the function signature is correct.
  expect_is(afun, "function")
  expect_identical(
    names(formals(afun)),
    c("x", "...", ".stats", ".indent_mods", ".formats", ".labels")
  )

  # Make sure function works when not `in_ref`.
  x <- c(1, 0, -1, 2, 5, 3, 2.5, 7.1)
  result <- afun(x)
  expected <- in_rows(
    .list = list(
      n = CellValue(with_label(8L, "Number of patients"), "xx.", label = "Number of patients"),
      mean = CellValue(with_label(2.45, "Mean"), "xx.xx", label = "  Mean"),
      median = CellValue(with_label(2.25, "Median"), "xx", label = " Median")
    )
  )
  expect_identical(result, expected)

  # And now with `in_ref`. Here we expect empty strings with format "xx" and labels.
  result <- afun(x, in_ref = TRUE)
  expected <- in_rows(
    .list = list(
      n = CellValue(with_label("", "Number of patients"), "xx", label = "Number of patients"),
      mean = CellValue(with_label("", "Mean"), "xx", label = "  Mean"),
      median = CellValue(with_label("", "Median"), "xx", label = " Median")
    )
  )
  expect_identical(result, expected)
})

test_that("format_wrap_df works with empty strings in end to end example", {
  sfun <- function(df, .var, .ref_group, .in_ref_col) {
    list(
      range = with_label(
        `if`(.in_ref_col, "", range(df[[.var]])),
        "Label for Range"
      ),
      mean = with_label(
        `if`(.in_ref_col, "", mean(df[[.var]])),
        "Label for Mean"
      )
    )
  }
  afun <- format_wrap_df(
    sfun,
    formats = c(range = c("(xx.xx, xx.xx)"), mean = "xx.xx"),
    indent_mods = c(range = 0L, mean = 2L)
  )
  result <- basic_table() %>%
    split_cols_by("Species", ref_group = "setosa") %>%
    analyze("Sepal.Length", afun = afun) %>%
    build_table(iris)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "Label for Range", "  Label for Mean", "setosa",
      "", "", "versicolor", "(4.9, 7)", "5.94", "virginica", "(4.9, 7.9)",
      "6.59"),
    .Dim = 3:4
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("format_wrap_df works with duplicate names in statistics function result list", {
  sfun <- function(df) {
    # Hardcode different results here for simplicity.
    list(
      a = with_label(5, "result 1"),
      ci = with_label(c(0.111, 7.32598), "CI 1"),
      a = with_label(10, "result 2"),
      ci = with_label(c(5.235235, 12.23423), "CI 2")
    )
  }
  afun <- format_wrap_df(
    sfun,
    formats = c(a = "xx.", ci = "(xx.x, xx.x)"),
    indent_mods = c(a = 0L, ci = 2L)
  )

  result <- basic_table() %>%
    analyze("Sepal.Length", afun = afun) %>%
    build_table(iris)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "result 1", "  CI 1", "result 2", "  CI 2", "all obs",
      "5", "(0.1, 7.3)", "10", "(5.2, 12.2)"),
    .Dim = c(5L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("format_wrap_x works with duplicate names in statistics function result list", {
  sfun <- function(x) {
    # Hardcode different results here for simplicity.
    list(
      mean = with_label(mean(x), "mean"),
      sd = with_label(sd(x), "sd"),
      mean = with_label(mean(x, na.rm = TRUE), "mean without NAs"),
      sd = with_label(sd(x, na.rm = TRUE), "sd without NAs")
    )
  }
  afun <- format_wrap_x(
    sfun,
    formats = c(mean = "xx.x", sd = "xx.xxxx"),
    indent_mods = c(mean = 0L, sd = 2L)
  )

  result <- basic_table() %>%
    analyze(
      "Sepal.Length",
      afun = afun,
      extra_args = list(
        .formats = c(sd = "xx"),
        .indent_mods = c(mean = 5L))
    ) %>%
    build_table(iris)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "     mean", "  sd", "     mean without NAs",
      "  sd without NAs", "all obs", "5.8", "0.828066127977863", "5.8",
      "0.828066127977863"),
    .Dim = c(5L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("c_label_n works as expected", {
  result <- c_label_n(data.frame(a = c(1, 2)), "female", .N_row = 4)
  expected <- CellValue(val = NULL, label = "female (N=4)")
  expect_identical(result, expected)
})

test_that("add_rowcounts works with one row split", {
  result <- basic_table() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts() %>%
    build_table(DM)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "F (N=187)", "M (N=169)", "all obs", "", ""),
    .Dim = 3:2
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("add_rowcounts works with multiple column and row splits", {
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
    c("", "", "CHN (N=179)", "F (N=94)", "mean", "M (N=85)",
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
      "NaN", "", "", "NaN", "", "NaN", "", "", "NaN", "", "NaN"),
    .Dim = c(47L, 10L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("h_col_indices works as expected", {
  tab <- basic_table() %>%
    split_cols_by("ARM") %>%
    build_table(DM)
  result <- h_col_indices(tab, c("B: Placebo", "C: Combination"))
  expected <- c(2L, 3L)
  expect_identical(result, expected)
})

test_that("groups_list_to_df works as expected", {
  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )
  result <- groups_list_to_df(grade_groups)
  expected <- structure(
    list(
      valname = c("AnyGrade", "Grade34", "Grade5"),
      label = c("Any Grade (%)", "Grade 3-4 (%)", "Grade 5 (%)"),
      levelcombo = list(c("1", "2", "3", "4", "5"), c("3", "4"), "5"),
      exargs = list(list(), list(), list())
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_identical(result, expected)
})

test_that("as.rtable.data.frame works correctly", {
  x <- data.frame(
    a = 1:10,
    b = seq(from = 10000, to = 20000, length = 10) / 1000
  )
  rownames(x) <- LETTERS[1:10]
  result <- as.rtable(x, format = "xx.x")
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "A", "B", "C", "D", "E", "F", "G", "H", "I",
      "J", "a", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
      "b", "10", "11.1", "12.2", "13.3", "14.4", "15.6", "16.7", "17.8",
      "18.9", "20"),
    .Dim = c(11L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("as.rtable.data.frame fails when a column is not numeric", {
  x <- data.frame(
    a = 1:10,
    b = LETTERS[1:10]
  )
  expect_error(as.rtable(x))
})

test_that("as.rtable.data.frame uses variable labels for column headers when they are available", {
  x <- data.frame(
    a = 1:10,
    b = seq(from = 10000, to = 20000, length = 10) / 1000
  )
  var_labels(x) <- paste("label for", names(x))
  rownames(x) <- LETTERS[1:10]
  result <- as.rtable(x, format = "xx.x")
  expect_identical(names(result), c("label for a", "label for b"))
})

test_that("h_split_param divides param values", {
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
  expect_identical(result, expected)

  .formats <- c("pt_at_risk" = "xx", "event_free_rate" = "xxx")
  result <- h_split_param(.formats, names(.formats), f =  f)
  expected <- list(
    surv = c("pt_at_risk" = "xx", "event_free_rate" = "xxx"),
    surv_diff = NULL
  )
  expect_identical(result, expected)
})

test_that("afun_selected_stats works for NULL input", {
  result <- afun_selected_stats(NULL, "b")
  expected <- "b"
  expect_identical(result, expected)
})

test_that("afun_selected_stats works for character input", {
  result <- afun_selected_stats(c("a", "c"), c("b", "c"))
  expected <- "c"
  expect_identical(result, expected)
})

test_that("split_cols_by_groups manages combinations of columns", {
  groups <- list(
    "Arms A+B" = c("A: Drug X", "B: Placebo"),
    "Arms A+C" = c("A: Drug X", "C: Combination")
  )
  result <- basic_table() %>%
    split_cols_by_groups("ARM", groups) %>%
    add_colcounts() %>%
    analyze("AGE") %>%
    build_table(DM)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Mean", "Arms A+B", "(N=227)", "34.03", "Arms A+C",
      "(N=250)", "34.73"
    ),
    .Dim = c(3L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("split_cols_by_groups manages combinations of columns with reference", {
  groups <- list(
    "Arms A+B" = c("A: Drug X", "B: Placebo"),
    "Arms A+C" = c("A: Drug X", "C: Combination")
  )
  result <- basic_table() %>%
    split_cols_by_groups("ARM", groups_list = groups, ref_group = "Arms A+B") %>%
    analyze(
      "AGE",
      afun = function(x, .ref_group, .in_ref_col){
        if (.in_ref_col) {
          in_rows("Diff. of Averages" = rcell(NULL))
        } else {
          in_rows("Diff. of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx"))
        }
      }
    ) %>%
    build_table(DM)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Diff. of Averages", "Arms A+B", "", "Arms A+C",
      "0.71"
    ),
    .Dim = 2:3
  )
  expect_identical(result_matrix, expected_matrix)
})
