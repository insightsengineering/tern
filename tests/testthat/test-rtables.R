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
      nrows = CellValue(4L, "xx."),
      ncols = CellValue(3L, "xx.xx")
    ),
    .labels = c(nrows = "nrows", ncols = "  ncols")
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
      ncols = CellValue(3L, "xx")
    ),
    .labels = c(ncols = " number columns")
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
      nrows = CellValue(4L, "xx."),
      ncols = CellValue(3L, "xx.xx"),
      incol = CellValue(FALSE, "xx"),
      nincol = CellValue(3, "xx")
    ),
    .labels = c(nrows = "nrows", ncols = "  ncols", incol = "incol", nincol = "nincol")
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
      n = CellValue(8L, "xx."),
      mean = CellValue(2.45, "xx.xx"),
      median = CellValue(2.25, "xx")
    ),
    .labels = c(n = "n", mean = "  mean", median = " median")
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
      n = CellValue(8L, "xx."),
      median = CellValue(2.25, "xx.xx")
    ),
    .labels = c(n = "Number of numbers", median = "   median")
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
      n = CellValue(8L, "xx."),
      mean = CellValue(2.45, "xx.xx"),
      median = CellValue(2.25, "xx"),
      var = CellValue("bla", "xx"),
      N = CellValue(10, "xx")
    ),
    .labels = c(n = "n", mean = "  mean", median = " median", var = "var", N = "  N")
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
      n = CellValue(with_label(8L, "Number of patients"), "xx."),
      mean = CellValue(with_label(2.45, "Mean"), "xx.xx"),
      median = CellValue(with_label(2.25, "Median"), "xx")
    ),
    .labels = c(n = "Number of patients", mean = "  Mean", median = " Median")
  )
  expect_identical(result, expected)

  # And now with `in_ref`. Here we expect empty strings with format "xx" and labels.
  result <- afun(x, in_ref = TRUE)
  expected <- in_rows(
    .list = list(
      n = CellValue(with_label("", "Number of patients"), "xx"),
      mean = CellValue(with_label("", "Mean"), "xx"),
      median = CellValue(with_label("", "Median"), "xx")
    ),
    .labels = c(n = "Number of patients", mean = "  Mean", median = " Median")
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
