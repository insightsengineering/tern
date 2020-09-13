context("test tabulate_aggregation_functions")

library(rtables)

test_that("`ttest_ci_one_arm` gives same results as `t.test`", {
  # Case 1: No missing values.
  x1 <- c(-12, 14, 20, 5, 8, 7)
  result1 <- ttest_ci_one_arm(x = x1, conf_level = 0.95)
  expected1 <- t.test(x = x1, conf.level = 0.95)$conf.int
  expect_equal(result1, expected1, check.attributes = FALSE)

  # Case 2: Some missing values.
  x2 <- c(-12, 14, NA, 5, NA, 7)
  result2 <- ttest_ci_one_arm(x = x2, conf_level = 0.8)
  expected2 <- t.test(x = x2, conf.level = 0.8)$conf.int
  expect_equal(result2, expected2, check.attributes = FALSE)

  # Case 3: Only 2 values.
  x3 <- c(-12, 14)
  result3 <- ttest_ci_one_arm(x = x3, conf_level = 0.6)
  expected3 <- t.test(x = x3, conf.level = 0.6)$conf.int
  expect_equal(result3, expected3, check.attributes = FALSE)
})

test_that(
  "`ttest_ci_one_arm` returns empty `rcell` if less than 2 values available", {
    # Case 1: All values are missing.
    x1 <- as.numeric(rep(NA, 3))
    result1 <- ttest_ci_one_arm(x1)
    expected1 <- rtables::rcell(" ")
    expect_equal(result1, expected1, check.attributes = FALSE)

    # Case 2: Only one value is available.
    x2 <- as.numeric(c(NA, 2))
    result2 <- ttest_ci_one_arm(x2)
    expected2 <-  rtables::rcell(" ")
    expect_equal(result2, expected2, check.attributes = FALSE)
  })

test_that("`ttest_ci_one_arm` returns constant if `x` is constant", {
  x <- c(-12, -12, -12)
  result <- ttest_ci_one_arm(x)
  expected <- c(-12, -12)
  expect_equal(result, expected, check.attributes = FALSE)
})


# Data for aggregate functions:
dta_test <- list(
  a = c(1, 2, 3),
  b = c(1, 2, NA),
  c = c(1, 1, NA),
  d = c(1),
  e = c(),
  f = c(NA, NA),
  g = NULL
)

test_that("`a_mean_sd` estimates good mean and standard deviations.", {
  expect_true(
    all(
      a_mean_sd(dta_test$a) == c(2, 1),
      a_mean_sd(dta_test$b) == c(1.5, sd(c(1, 2))),
      all(is.na(a_mean_sd(dta_test$b, na.rm = FALSE))),
      all(is.na(a_mean_sd(dta_test$b, na.rm = FALSE, na = "Not Estimated"))),
      a_mean_sd(dta_test$c) == c(1, 0),
      all(is.na(a_mean_sd(dta_test$c, na.rm = FALSE))),
      all(is.na(a_mean_sd(dta_test$c, na.rm = FALSE, na = "Not Estimated"))),
      { # nolint
        d <- a_mean_sd(dta_test$d)
        d <- a_mean_sd(dta_test$d, na = "NE")
        d[1] == 1 & is.na(d[2])
      },
      all(is.na(a_mean_sd(dta_test$e))),
      all(is.na(a_mean_sd(dta_test$e, na = "NE"))),
      all(is.na(a_mean_sd(dta_test$f))),
      all(is.na(a_mean_sd(dta_test$f, na = "NE"))),
      all(is.na(a_mean_sd(dta_test$g))),
      all(is.na(a_mean_sd(dta_test$g, na = "NE")))
    )
  )
})

test_that("`a_mean_sd` returns always a `rcell`.", {
  expect_true(
    all(
      vapply(
        lapply(dta_test, a_mean_sd, rcell = TRUE), FUN = class, FUN.VALUE = "class"
      ) == "rcell"
    )
  )
})


test_that("`a_median` returns the right numbers, displays NA, or <na>.", {

  expect_true(
    all(
      a_median(dta_test$a) == 2,
      a_median(dta_test$b) == 1.5,
      is.na(a_median(dta_test$b, na.rm = FALSE)),
      is.na(
        a_median(
          dta_test$b, na.rm = FALSE, na = "missing", rcell = TRUE
        )
      ),
      a_median(dta_test$c) == 1,
      is.na(a_median(dta_test$c, na.rm = FALSE)),
      utils::capture.output(
        a_median(
          dta_test$c, na.rm = FALSE, na = "missing",
          rcell = TRUE
        )
      ) == "missing",
      utils::capture.output(
        a_median(
          dta_test$c, na.rm = FALSE, na = "missing"
        )
      ) == "[1] NA",
      utils::capture.output(
        a_median(dta_test$e)
      ) == "[1] NA",
      utils::capture.output(
        a_median(dta_test$e, na = "missing")
      ) == "[1] NA",
      utils::capture.output(
        a_median(dta_test$e, na = "missing", rcell = TRUE)
      ) == "missing",
      all(is.na(a_median(dta_test$f))),
      all(is.na(a_median(dta_test$g)))
    )
  )
})

test_that(
  "`a_count` with `na.rm = FALSE` gives the same values as `n_not_na2`",
  testthat::expect_equivalent(
    addNA(unlist(lapply(dta_test, a_count, na.rm = FALSE))),
    addNA(as.numeric(unlist(lapply(dta_test, n_not_na2))))
  )
)

test_that(
  "`a_count` with `na.rm = TRUE` gives the same values as `count_n`.",
  expect_equivalent(
    unlist(lapply(dta_test, count_n)), unlist(lapply(dta_test, a_count))
  )
)

test_that(
  "`a_count` with `na.rm = TRUE` gives the same values as `n_not_na3`.",
  expect_equivalent(
    unlist(lapply(dta_test, n_not_na3)), unlist(lapply(dta_test, a_count))
  )
)

test_that(
  "`a_count` with `na.rm = TRUE` gives expected values.",
  expect_equivalent(
    lapply(dta_test, a_count, na.rm = TRUE),
    list(a = 3, b = 2, c = 2, d = 1, e = 0, f = 0, g = 0)
  )
)

test_that(
  "`a_count` with `na.rm = FALSE` gives expected values.",
  expect_equivalent(
    lapply(dta_test, a_count, na.rm = FALSE),
    list(a = 3, b = 2, c = 2, d = 1, e = NA, f = NA, g = NA)
  )
)

test_that(
  "NA string is modifiable in `a_count`.",
  expect_identical(
    capture.output(a_count(dta_test$e, na.rm = FALSE, na = "NE", rcell = TRUE)),
    "NE"
  )
)


# Dataset for `a_n_true_and_freq` tests.
booleans <- list(
  a = c(TRUE, TRUE, TRUE),
  b = c(FALSE, FALSE, FALSE),
  c = c(TRUE, FALSE, NA),
  d = c(),
  e = NULL
)

test_that("`a_n_true_and_freq` return expected values given na.rm = TRUE.", {

  output <- lapply(booleans, a_n_true_and_freq, na.rm = TRUE)
  expect_identical(
    output,
    list(a = c(3, 1), b = c(0, 0), c = c(1, .5), d = c(NA, NA), e = c(NA, NA))
  )

})

test_that("`a_n_true_and_freq` return expected values given na.rm = FALSE.", {

  output <- lapply(booleans, a_n_true_and_freq, na.rm = FALSE)
  expect_identical(
    output,
    list(
      a = c(3, 1), b = c(0, 0), c = c(NA_real_, NA_real_),
      d = c(NA, NA), e = c(NA, NA)
    )
  )

})

test_that("`a_n_true_and_freq` modifies character string for missing data.", {

  expect_identical(
    utils::capture.output(
      a_n_true_and_freq(
        booleans$c, na.rm = FALSE, na = "missing",
        rcell = TRUE
      )
    ),
    "missing (missing)"
  )

})



context("Aggregation function based on quantile estimations.")

dta_quant <- list(
  a = sample(c(1, 1, 2, 3, 4, 4, 5, 7, 8, 9)),
  b = sample(c(1, 1, 2, 3, 4, 4, 5, 7, 8, 9, 9)),
  c = sample(c(1, 1, 2, 3, 4, 4, 5, 7, 8, NA)),
  d = c(NA, NA),
  e = c(),
  f = NULL
)

test_that(
  "Results of `a_q1q3` are correct",
  expect_equal(
    lapply(dta_quant, a_q1q3, type = 1),
    list(
      a = c(2, 7), b = c(2, 8), c = c(2, 5),
      d = c(NA, NA), e = c(NA, NA), f = c(NA, NA)
    )
  )
)

test_that(
  "The results provided by `a_q1q3` accounts for quantile algorithms.",
  expect_equal(
    lapply(dta_quant, a_q1q3, type = 7, na.rm = FALSE),
    list(
      a = c(2.25, 6.50), b = c(2.5, 7.5), c = c(NA, NA),
      d = c(NA, NA), e = c(NA, NA), f = c(NA, NA)
    )
  )
)

test_that(
  "`a_iqr` gives equal results to (soon deprecated) `iqr_num`.",
  expect_equal(
    unname(unlist(lapply(dta_quant, iqr_num, type = 5))),
    unname(unlist(lapply(dta_quant, a_iqr, type = 5)))
  )
)

test_that(
  "`a_iqr` gives equal results to (soon deprecated) `iqr_num2`.",
  expect_identical(
    addNA(as.numeric(lapply(dta_quant, iqr_num2, type = 9))),
    addNA(as.numeric(lapply(dta_quant, a_iqr, type = 9)))
  )
)

test_that(
  "`a_iqr` can render similar output to (soon deprecated) `iqr_num3`.",
  expect_identical(
    capture.output(lapply(dta_quant[c("d", "e", "f")], iqr_num3, type = 3)),
    capture.output(
      lapply(
        dta_quant[c("d", "e", "f")], a_iqr, type = 3, rcell = TRUE, na = "NE")
    )
  )
)

test_that(
  "`a_range` result in the same estimations as (soon deprecated) `range_t2`",
  expect_identical(
    lapply(dta_quant[c("a", "b", "c")], a_range, na.rm = TRUE),
    lapply(dta_quant[c("a", "b", "c")], range_t2)
  )
)

test_that(
  "`a_range` result in the same estimations as (soon deprecated) `range_t3`",
  expect_identical(
    lapply(dta_quant[c("a", "b", "c")], a_range, na.rm = TRUE),
    lapply(dta_quant[c("a", "b", "c")], range_t3)
  )
)
