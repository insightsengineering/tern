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
