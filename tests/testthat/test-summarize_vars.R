
test_that("`summarize_vars` works with healthy input, default `na.rm = TRUE`.", {

  dta_test <- data.frame(AVAL = c(1:4, NA, NA))

  l <- summarize_vars(lyt = NULL, vars = "AVAL")
  result <- build_table(l, df = dta_test)

  expected <- structure(
    c(
      "", "n", "Mean (SD)", "Median", "Min - Max", "all obs",
      "4", "2.5 (1.3)", "2.5", "1 - 4"
    ),
    .Dim = c(5L, 2L)
  )

  expect_identical(to_string_matrix(result), expected)

})


test_that("`summarize_vars` works with healthy input, alternative `na.rm = FALSE`", {

  dta_test <- data.frame(AVAL = c(1:4, NA, NA))

  l <- summarize_vars(lyt = NULL, vars = "AVAL", na.rm = FALSE)
  result <- build_table(l, df = dta_test)

  expected <- structure(
    c(
      "", "n", "Mean (SD)", "Median", "Min - Max", "all obs",
      "6", "NA (NA)", "NA", "NA - NA"
    ),
    .Dim = c(5L, 2L)
  )

  expect_identical(to_string_matrix(result), expected)

})
