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
  var_labels(my_data) <- paste0("Variable ", seq_len(ncol(my_data)))
  my_data
}

test_that("Default fill in of missing values and conversion to factor works as expected", {

  my_data <- example_data
  result <- df_explicit_na(data = my_data, omit_columns = NULL, char_as_factor = TRUE)

  expected <- data.frame(
    v1 = factor(
      c("A", "B", "A", "B"),
      levels = c("B", "A")
    ),
    v2 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v3 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v4 = factor(
      c("D", "E", "F", "E"),
      levels = c("D", "E", "F")
    ),
    v5 = factor(
      c("A", "B", "<Missing>", "C"),
      levels = c("A", "B", "C", "<Missing>")
    ),
    v6 = factor(
      c("A", "B", "<Missing>", "C"),
      levels = c("A", "B", "C", "<Missing>")
    ),
    v7 = c(1:4),
    v8 = c(TRUE, FALSE, NA, NA),
    stringsAsFactors = FALSE
  )
  var_labels(expected) <- paste0("Variable ", 1:8)

  expect_equal(result, expected)
})

test_that("Default settings work when input data does not have labels", {

  my_data <- example_data %>%
    var_labels_remove()

  result <- df_explicit_na(data = my_data, omit_columns = NULL, char_as_factor = TRUE)

  expected <- data.frame(
    v1 = factor(
      c("A", "B", "A", "B"),
      levels = c("B", "A")
    ),
    v2 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v3 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v4 = factor(
      c("D", "E", "F", "E"),
      levels = c("D", "E", "F")
    ),
    v5 = factor(
      c("A", "B", "<Missing>", "C"),
      levels = c("A", "B", "C", "<Missing>")
    ),
    v6 = factor(
      c("A", "B", "<Missing>", "C"),
      levels = c("A", "B", "C", "<Missing>")
    ),
    v7 = c(1:4),
    v8 = c(TRUE, FALSE, NA, NA),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("Only replace missing values without modifying character or logical variables", {

  my_data <- example_data
  result <- df_explicit_na(data = my_data, omit_columns = NULL, char_as_factor = FALSE)

  expected <- data.frame(
    v1 = factor(
      c("A", "B", "A", "B"),
      levels = c("B", "A")
    ),
    v2 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v3 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v4 = c("D", "E", "F", "E"),
    v5 = c("A", "B", "<Missing>", "C"),
    v6 = c("A", "B", "<Missing>", "C"),
    v7 = c(1:4),
    v8 = c(TRUE, FALSE, NA, NA),
    stringsAsFactors = FALSE
  )
  var_labels(expected) <- paste0("Variable ", 1:8)

  expect_equal(result, expected)

})

test_that("Conversion to factor works with some variables omitted", {

  my_data <- example_data
  result <- df_explicit_na(
    data = my_data,
    omit_columns = c("v2", "v6"),
    char_as_factor = TRUE,
    logical_as_factor = TRUE
  )

  expected <- data.frame(
    v1 = factor(
      c("A", "B", "A", "B"),
      levels = c("B", "A")
    ),
    v2 = factor(
      c("A", "B", "A", NA),
      levels = c("B", "A")
    ),
    v3 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v4 = factor(
      c("D", "E", "F", "E"),
      levels = c("D", "E", "F")
    ),
    v5 = factor(
      c("A", "B", "<Missing>", "C"),
      levels = c("A", "B", "C", "<Missing>")
    ),
    v6 = c("A", "B", "", "C"),
    v7 = c(1:4),
    v8 = factor(
      c("TRUE", "FALSE", "<Missing>", "<Missing>"),
      levels = c("FALSE", "TRUE", "<Missing>")
    ),
    stringsAsFactors = FALSE
  )
  var_labels(expected) <- paste0("Variable ", 1:8)

  expect_equal(result, expected)

})

test_that("Only convert logical variables but not character variables", {

  my_data <- example_data
  result <- df_explicit_na(
    data = my_data,
    omit_columns = NULL,
    char_as_factor = FALSE,
    logical_as_factor = TRUE
  )

  expected <- data.frame(
    v1 = factor(
      c("A", "B", "A", "B"),
      levels = c("B", "A")
    ),
    v2 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v3 = factor(
      c("A", "B", "A", "<Missing>"),
      levels = c("B", "A", "<Missing>")
    ),
    v4 = c("D", "E", "F", "E"),
    v5 = c("A", "B", "<Missing>", "C"),
    v6 = c("A", "B", "<Missing>", "C"),
    v7 = c(1:4),
    v8 = factor(
      c("TRUE", "FALSE", "<Missing>", "<Missing>"),
      levels = c("FALSE", "TRUE", "<Missing>")
    ),
    stringsAsFactors = FALSE
  )
  var_labels(expected) <- paste0("Variable ", 1:8)

  expect_equal(result, expected)

})

test_that("Check Errors", {

  my_data <- example_data

  expect_error(df_explicit_na(my_data, na_level = NA), "na_level")
  expect_error(df_explicit_na(my_data, char_as_factor = "TRUE"), "logical")
  expect_error(df_explicit_na(my_data, omit_columns = 1), "character")
  expect_error(df_explicit_na(my_data, na_level = NULL), "character")
  expect_error(df_explicit_na(c("A")), "data.frame")
  expect_error(df_explicit_na(my_data, omit_columns = names(my_data)), "character")

})
