context("df_explicit_na")


my_data <- data.frame(
  x = c("A", "B", NA, "C"),
  y = c("D", "E", "F", "E"),
  z = c(1, 2, 3, 4),
  stringsAsFactors = FALSE
)

test_that("Conversion to factor", {

  prepared_data <- expect_silent(df_explicit_na(my_data))

  expect_equal(levels(prepared_data$x), c("A", "B", "C", "<Missing>"))

  expect_equal(class(prepared_data$x), "factor")
  expect_equal(class(prepared_data$y), "factor")
  expect_equal(class(prepared_data$z), "numeric")
})


test_that("Only replace NA", {

  prepared_data <- expect_silent(df_explicit_na(my_data, char_as_factor = FALSE))

  expect_equal(unique(prepared_data$x), c("A", "B", "<Missing>", "C"))

  expect_equal(class(prepared_data$y), "character")

  my_data_fac <- my_data
  my_data_fac[["x"]] <- as.factor(my_data$x)

  prepared_data <- expect_silent(df_explicit_na(my_data_fac, char_as_factor = FALSE))

  expect_equal(levels(prepared_data$x), c("A", "B", "C", "<Missing>"))

})

test_that("Exclude", {

  prepared_data <- expect_silent(df_explicit_na(my_data, omit_columns = "x"))

  expect_equal(unique(prepared_data$x), c("A", "B", NA, "C"))

  expect_equal(class(prepared_data$x), "character")
  expect_equal(class(prepared_data$y), "factor")
})

test_that("Check Errors", {
  expect_error(df_explicit_na(my_data, na_level = NA), "na_level")
  expect_error(df_explicit_na(my_data, char_as_factor = "TRUE"), "logical")
  expect_error(df_explicit_na(my_data, omit_columns = 1), "character")
  expect_error(df_explicit_na(my_data, na_level = NULL), "character")
  expect_error(df_explicit_na(c("A")), "data.frame")

})


my_data <- data.frame(
  x = c("A", "B", NA, "C"),
  y = c("D", "E", "F", "E"),
  z = c(1, 2, 3, 4)
)

test_that("Keep factors", {

  prepared_data <- expect_silent(df_explicit_na(my_data))

  expect_equal(levels(prepared_data$x), c("A", "B", "C", "<Missing>"))

  expect_equal(class(prepared_data$x), "factor")
  expect_equal(class(prepared_data$y), "factor")
  expect_equal(class(prepared_data$z), "numeric")
})
