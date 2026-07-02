example_data <- {
  my_data <- data.frame(
    v1 = factor(c("A", "B", "A", "B"), levels = c("B", "A")),
    v2 = factor(c("A", "B", "A", NA), levels = c("B", "A")),
    v3 = factor(c("A", "B", "A", ""), levels = c("", "B", "A")),
    v4 = c("D", "E", "F", "E"),
    v5 = c("A", "B", NA, "C"),
    v6 = c("A", "B", "", "C"),
    v7 = c(1, 2, 3, 4),
    v8 = c(TRUE, FALSE, NA, NA),
    stringsAsFactors = FALSE
  )
  formatters::var_labels(my_data) <- paste0("Variable ", seq_len(ncol(my_data)))
  my_data
}

testthat::test_that("Default fill in of missing values and conversion to factor works as expected", {
  my_data <- example_data
  result <- df_explicit_na(data = my_data, omit_columns = NULL, char_as_factor = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(sapply(result, levels))
  testthat::expect_snapshot(res)
})

testthat::test_that("Default settings work when input data does not have labels", {
  my_data <- example_data
  for (i in seq_along(my_data)) {
    attr(my_data[[i]], "label") <- NULL
  }
  result <- df_explicit_na(data = my_data, omit_columns = NULL, char_as_factor = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Only replace missing values without modifying character or logical variables", {
  my_data <- example_data
  result <- df_explicit_na(data = my_data, omit_columns = NULL, char_as_factor = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Conversion to factor works with some variables omitted", {
  my_data <- example_data
  result <- df_explicit_na(
    data = my_data,
    omit_columns = c("v2", "v6"),
    char_as_factor = TRUE,
    logical_as_factor = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Only convert logical variables but not character variables", {
  my_data <- example_data
  result <- df_explicit_na(
    data = my_data,
    omit_columns = NULL,
    char_as_factor = FALSE,
    logical_as_factor = TRUE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("Check Errors", {
  my_data <- example_data

  testthat::expect_error(df_explicit_na(my_data, na_level = NA), "na_level")
  testthat::expect_error(df_explicit_na(my_data, char_as_factor = "TRUE"), "logical")
  testthat::expect_error(df_explicit_na(my_data, omit_columns = 1), "character")
  testthat::expect_error(df_explicit_na(my_data, na_level = NULL), "string")
  testthat::expect_error(df_explicit_na(c("A")), "data.frame")
})

testthat::test_that("df_explicit_na just returns unmodified data if all columns are omitted", {
  my_data <- example_data
  result <- testthat::expect_silent(df_explicit_na(
    data = my_data,
    omit_columns = names(my_data),
    char_as_factor = FALSE,
    logical_as_factor = TRUE
  ))
  testthat::expect_identical(result, my_data)
})

testthat::test_that("factor_level_method = 'sort_auto' gives same result as default", {
  my_data <- example_data
  result_default <- df_explicit_na(my_data, char_as_factor = TRUE)
  result_auto <- df_explicit_na(my_data, char_as_factor = TRUE, factor_level_method = "sort_auto")
  testthat::expect_identical(result_default, result_auto)
})

testthat::test_that("factor_level_method = 'sort_radix' uses byte-order sort (uppercase before lowercase)", {
  # Mixed-case values: sort_auto is locale-aware (case-insensitive on en_US.UTF-8),
  # sort_radix is byte-order (uppercase < lowercase, consistent with SAS PROC SORT).
  my_data <- data.frame(x = c("UNKNOWN", "Unknown", "unknown", NA), stringsAsFactors = FALSE)
  result_radix <- df_explicit_na(my_data, factor_level_method = "sort_radix")
  result_auto  <- df_explicit_na(my_data, factor_level_method = "sort_auto")
  # radix: UNKNOWN < Unknown < unknown (ASCII byte order)
  testthat::expect_equal(levels(result_radix$x), c("UNKNOWN", "Unknown", "unknown", "<Missing>"))
  # auto and radix differ on mixed-case data
  testthat::expect_false(identical(levels(result_radix$x), levels(result_auto$x)))
})

testthat::test_that("factor_level_method = 'data' preserves first-appearance order", {
  my_data <- data.frame(x = c("C", "A", "B", NA), stringsAsFactors = FALSE)
  result <- df_explicit_na(my_data, factor_level_method = "data")
  testthat::expect_equal(levels(result$x), c("C", "A", "B", "<Missing>"))
})

testthat::test_that("factor_as_factor re-encodes existing factor levels using factor_level_method", {
  my_data <- data.frame(
    x = factor(c("C", "A", "B"), levels = c("C", "A", "B")),
    stringsAsFactors = FALSE
  )
  result <- df_explicit_na(my_data, factor_as_factor = TRUE, factor_level_method = "sort_auto")
  testthat::expect_equal(levels(result$x), c("A", "B", "C"))
})

testthat::test_that("factor_as_factor = FALSE preserves existing factor levels (original behavior)", {
  my_data <- data.frame(
    x = factor(c("C", "A", "B"), levels = c("C", "A", "B")),
    stringsAsFactors = FALSE
  )
  result <- df_explicit_na(my_data, factor_as_factor = FALSE)
  testthat::expect_equal(levels(result$x), c("C", "A", "B"))
})

testthat::test_that("factor_level_last_pattern moves matching levels to end", {
  my_data <- data.frame(x = c("Other", "A", "B", "Other2"), stringsAsFactors = FALSE)
  result <- df_explicit_na(my_data, factor_level_last_pattern = "^Other")
  lvls <- levels(result$x)
  # "A", "B" before "Other*"
  testthat::expect_true(which(lvls == "A") < which(lvls == "Other"))
  testthat::expect_true(which(lvls == "B") < which(lvls == "Other2"))
})

testthat::test_that("factor_level_last_pattern = NULL does not change level order", {
  my_data <- data.frame(x = c("C", "A", "B"), stringsAsFactors = FALSE)
  result_with <- df_explicit_na(my_data, factor_level_last_pattern = NULL)
  result_without <- df_explicit_na(my_data)
  testthat::expect_identical(levels(result_with$x), levels(result_without$x))
})

testthat::test_that("factor_level_last_pattern combined with na_level: na_level stays last", {
  my_data <- data.frame(x = c("Other", "A", NA), stringsAsFactors = FALSE)
  result <- df_explicit_na(my_data, factor_level_last_pattern = "^Other")
  lvls <- levels(result$x)
  testthat::expect_equal(tail(lvls, 1), "<Missing>")
  testthat::expect_true(which(lvls == "Other") < which(lvls == "<Missing>"))
})

testthat::test_that("Check new parameter errors", {
  my_data <- example_data
  testthat::expect_error(df_explicit_na(my_data, factor_as_factor = "TRUE"), "logical")
  testthat::expect_error(df_explicit_na(my_data, factor_level_last_pattern = 123), "string")
  testthat::expect_error(df_explicit_na(my_data, factor_level_method = "invalid"), "Must be element")
})
