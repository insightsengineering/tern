# Format wrapper for `s_summary`.
afun_s_summary <- format_wrap_x(
  sfun = s_summary,
  indent_mods = c(n = 0L, mean_sd = 0L, median = 0L, range = 0L, counts = 0L),
  formats = c(
    n = "xx", mean_sd = "xx.x (xx.x)", median = "xx.x", range = "xx.x - xx.x", counts = "xx.x (xx.x)"
  )
)

test_that("s_summary (+ afun wrapper) return NA for x length 0L.", {

  x <- numeric()
  expected <-  list(
    n = 0, range = c(NA_real_, NA_real_),
    median_sd = NA_real_, mean = c(NA_real_, NA_real_)
  )

  expect_equivalent(s_summary(x), expected)
  expect_equivalent(afun_s_summary(x), lapply(expected, rcell))

})


test_that("s_summary (+ afun wrapper) handles NA.", {

  x <- c(NA_real_, 1)

  # With `na.rm = TRUE`.
  expected <- list(n = 1, mean_sd = c(1, NA), median = 1, range = c(1, 1))
  expect_equivalent(s_summary(x), expected)
  expect_equivalent(afun_s_summary(x), lapply(expected, rcell))

  # With `na.rm = FALSE`.
  expected <- list(
    n = 2, mean_sd = c(NA_real_, NA_real_),
    median = NA_real_, range = c(NA_real_, NA_real_)
  )
  expect_equivalent(s_summary(x, na.rm = FALSE), expected)
  expect_equivalent(
    afun_s_summary(x, na.rm = FALSE),
    lapply(expected, rcell)
  )

})


test_that("s_summary (+ afun wrapper) returns right results.", {

  x <- c(NA_real_, 1, 2)
  expected <- list(
    n = 2, mean_sd = c(1.5, 0.7071068), median = 1.5, range = c(1, 2)
  )

  expect_equivalent(s_summary(x), expected, tolerance = .00001)
  expect_equivalent(
    afun_s_summary(x), lapply(expected, rcell),
    tolerance = .00001
  )

})

test_that("s_summary works with factors", {

  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown"))

  result <- s_summary(x)
  expected <- list(
    n = with_label(9L, "n"),
    count_fraction = list(
      Female = c(2, 2 / 9),
      Male = c(3, 3 / 9),
      Unknown = c(4, 4 / 9)
    )
  )

  expect_identical(result, expected)
})

test_that("s_summary works with factors with NA values and correctly removes them by default", {

  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown", NA))

  result <- s_summary(x)
  expected <- list(
    n = with_label(9L, "n"),
    count_fraction = list(
      Female = c(2, 2 / 9),
      Male = c(3, 3 / 9),
      Unknown = c(4, 4 / 9)
    )
  )

  expect_identical(result, expected)
})

test_that("s_summary fails with factors that have no levels or have empty string levels", {

  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown", ""))
  expect_error(
    s_summary(x),
    "x is not a valid factor, please check the factor levels (no empty strings allowed)",
    fixed = TRUE
  )

  x <- factor()
  expect_error(
    s_summary(x),
    "x is not a valid factor, please check the factor levels (no empty strings allowed)",
    fixed = TRUE
  )
})

test_that("s_summary works with length 0 factors that have levels", {

  x <- factor(levels = c("a", "b", "c"))

  result <- s_summary(x)
  expected <- list(
    n = with_label(0L, "n"),
    count_fraction = list(
      a = c(0L, NA),
      b = c(0L, NA),
      c = c(0L, NA)
    )
  )

  expect_identical(result, expected)
})

test_that("s_summary works with factors and different denominator choices", {

  x <- factor(c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown"))

  result <- s_summary(x, denom = "N_row", .N_row = 20)
  expected <- list(
    n = with_label(9L, "n"),
    count_fraction = list(
      Female = c(2, 2 / 20),
      Male = c(3, 3 / 20),
      Unknown = c(4, 4 / 20)
    )
  )
  expect_identical(result, expected)

  result <- s_summary(x, denom = "N_col", .N_col = 30)
  expected <- list(
    n = with_label(9L, "n"),
    count_fraction = list(
      Female = c(2, 2 / 30),
      Male = c(3, 3 / 30),
      Unknown = c(4, 4 / 30)
    )
  )
  expect_identical(result, expected)
})

test_that("s_summary works with characters by converting to character", {

  x <- c("Female", "Male", "Female", "Male", "Male", "Unknown", "Unknown", "Unknown", "Unknown")

  result <- expect_warning(s_summary(x, denom = "N_row", .N_row = 20))
  expected <- s_summary(factor(x), denom = "N_row", .N_row = 20)

  expect_identical(result, expected)
})

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

test_that("`summarize_vars` works with healthy factor input", {

  dta <- data.frame(foo = factor(c("a", "b", "a")))

  result <- basic_table() %>%
    summarize_vars(vars = "foo") %>%
    build_table(dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "n", "a", "b", "all obs", "3", "2 (66.7%)", "1 (33.3%)"),
    .Dim = c(4L, 2L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("`summarize_vars` works with factors and different denominators", {

  start <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    add_rowcounts()

  result_n <- start %>%
    summarize_vars("RACE", denom = "n") %>%
    build_table(DM)

  result_ncol <- start %>%
    summarize_vars("RACE", denom = "N_col") %>%
    build_table(DM)

  result <- start %>%
    summarize_vars("RACE", denom = "N_row") %>%
    build_table(DM)

  expect_false(identical(result_n, result_ncol))
  expect_false(identical(result_n, result))
  expect_false(identical(result_ncol, result))

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "", "F (N=187)", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "OTHER", "UNKNOWN", "M (N=169)", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "OTHER", "UNKNOWN", "A: Drug X", "(N=121)", "", "70", "44 (23.5%)",
      "18 (9.6%)", "8 (4.3%)", "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)",
      "0 (0%)", "", "51", "35 (20.7%)", "10 (5.9%)", "6 (3.6%)", "0 (0%)",
      "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)", "B: Placebo", "(N=106)",
      "", "56", "37 (19.8%)", "12 (6.4%)", "7 (3.7%)", "0 (0%)", "0 (0%)",
      "0 (0%)", "0 (0%)", "0 (0%)", "", "50", "31 (18.3%)", "12 (7.1%)",
      "7 (4.1%)", "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)",
      "C: Combination", "(N=129)", "", "61", "40 (21.4%)", "13 (7%)",
      "8 (4.3%)", "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)",
      "", "68", "44 (26%)", "14 (8.3%)", "10 (5.9%)", "0 (0%)", "0 (0%)",
      "0 (0%)", "0 (0%)", "0 (0%)"),
    .Dim = c(22L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("`summarize_vars` works with character input and gives the same result as with factor", {

  dta <- data.frame(
    foo = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )

  l <- basic_table() %>%
    summarize_vars(vars = "foo")
  result <- expect_warning(build_table(l, dta))

  dta_factor <- dta %>%
    dplyr::mutate(foo = factor(foo))
  expected <- build_table(l, dta_factor)

  expect_identical(result, expected)
})
