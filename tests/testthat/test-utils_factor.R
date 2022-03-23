# combine_vectors ----

testthat::test_that("combine_vectors works correctly", {
  x <- c(1:3)
  y <- c(4:6)
  result <- combine_vectors(x, y)

  expected <- list(
    c(1, 4),
    c(2, 5),
    c(3, 6)
  )

  testthat::expect_equal(result, expected)
})

# as_factor_keep_attributes ----

testthat::test_that("as_factor_keep_attributes works correctly for a character vector", {
  foo <- formatable::with_label(c("a", "b"), "alphabet")
  result <- testthat::expect_warning(
    as_factor_keep_attributes(foo),
    "automatically converting character variable foo to factor"
  )
  expected <- formatable::with_label(factor(c("a", "b")), "alphabet")
  testthat::expect_identical(result, expected)
})

testthat::test_that("as_factor_keep_attributes converts empty strings for a character vector", {
  foo <- formatable::with_label(c("a", "", "b"), "alphabet")
  result <- testthat::expect_warning(
    as_factor_keep_attributes(foo, na_level = "missing"),
    "automatically converting character variable foo to factor"
  )
  expected <- formatable::with_label(factor(c("a", "missing", "b"), levels = c("a", "b", "missing")), "alphabet")
  testthat::expect_identical(result, expected)
})

testthat::test_that("as_factor_keep_attributes shows correct name of vector in warning", {
  foo <- formatable::with_label(c("a", "b"), "alphabet")
  testthat::expect_warning(
    as_factor_keep_attributes(foo, x_name = "FOO"),
    "automatically converting character variable FOO to factor"
  )
})

testthat::test_that("as_factor_keep_attributes does not modify a factor at all", {
  foo <- factor(c(1, 2))
  result <- testthat::expect_silent(as_factor_keep_attributes(foo))
  testthat::expect_identical(result, foo)
})

# bins_percent_labels ----

testthat::test_that("bins_percent_labels works as expected", {
  testthat::expect_identical(
    bins_percent_labels(c(0.2, 0.555, 0.8)),
    c("[0%,20%]", "(20%,56%]", "(56%,80%]", "(80%,100%]")
  )
  testthat::expect_identical(
    bins_percent_labels(c(0.2, 0.555, 0.8), digits = 1),
    c("[0%,20%]", "(20%,55.5%]", "(55.5%,80%]", "(80%,100%]")
  )
  testthat::expect_identical(
    bins_percent_labels(c()),
    c("[0%,100%]")
  )
})

# cut_quantile_bins ----

testthat::test_that("cut_quantile_bins works as expected with default settings", {
  result <- cut_quantile_bins(cars$speed)
  testthat::expect_is(result, "ordered")
  testthat::expect_identical(levels(result), c("[0%,25%]", "(25%,50%]", "(50%,75%]", "(75%,100%]"))
  testthat::expect_identical(length(result), length(cars$speed))
  q1 <- stats::quantile(cars$speed, probs = 0.25)
  testthat::expect_true(identical(result == "[0%,25%]", cars$speed <= q1))
  testthat::expect_true(identical(result != "[0%,25%]", cars$speed > q1))
})

testthat::test_that("cut_quantile_bins works with custom quantiles", {
  result <- cut_quantile_bins(cars$speed, probs = c(0.1, 0.3, 0.8))
  testthat::expect_identical(levels(result), c("[0%,10%]", "(10%,30%]", "(30%,80%]", "(80%,100%]"))
})

testthat::test_that("cut_quantile_bins works with custom labels", {
  result <- cut_quantile_bins(
    cars$speed,
    probs = c(0.1, 0.3, 0.8),
    labels = c("low", "medium", "high", "top")
  )
  testthat::expect_identical(levels(result), c("low", "medium", "high", "top"))
})

testthat::test_that("cut_quantile_bins preserves NAs in result", {
  x <- airquality$Ozone
  assertthat::assert_that(
    any(is.na(x)),
    !all(is.na(x))
  )
  result <- cut_quantile_bins(x)
  testthat::expect_identical(is.na(x), is.na(result))
})

testthat::test_that("cut_quantile_bins also works when there are only NAs", {
  x <- rep(NA_real_, 10)
  result <- testthat::expect_silent(cut_quantile_bins(x))
  testthat::expect_true(all(is.na(result)))
  testthat::expect_identical(levels(result), c("[0%,25%]", "(25%,50%]", "(50%,75%]", "(75%,100%]"))
})

testthat::test_that("cut_quantile_bins gives understandable error message if there are duplicate quantiles", {
  x <- c(rep(NA_real_, 10), 1)
  testthat::expect_error(
    cut_quantile_bins(x),
    "Duplicate quantiles produced, please use a coarser `probs` vector"
  )
})

testthat::test_that("cut_quantile_bins works if an empty `probs` vector is used", {
  x <- 1:10
  result <- testthat::expect_silent(cut_quantile_bins(x, probs = c()))
  testthat::expect_true(all(result == "[0%,100%]"))
})

# fct_discard ----

testthat::test_that("fct_discard works as expected", {
  x <- factor(c("a", "b", "c"))
  result <- fct_discard(x, "b")
  expected <- factor(c("a", "c"))
  testthat::expect_identical(result, expected)
})

# fct_explicit_na_if ----

testthat::test_that("fct_explicit_na_if works as expected with factor input", {
  x <- factor(c("a", "b", NA))
  cond <- c(TRUE, FALSE, FALSE)
  result <- fct_explicit_na_if(x, cond)
  expected <- factor(c("<Missing>", "b", "<Missing>"), levels = c("a", "b", "<Missing>"))
  testthat::expect_identical(result, expected)
})

# fct_collapse_only ----

testthat::test_that("fct_collapse_only works as expected", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("c", "d"))
  expected <- factor(c("<Missing>", "TRT", "CTRL", "CTRL"), levels = c("TRT", "CTRL", "<Missing>"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("fct_collapse_only uses the customized `na_level` as expected", {
  x <- factor(c(NA, "b", "c", "d"), levels = c("b", "c", "d"))
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("d"), .na_level = "Missing")
  expected <- factor(c(NA, "TRT", "Missing", "CTRL"), levels = c("TRT", "CTRL", "Missing"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("fct_collapse_only works as expected with character input", {
  x <- c("a", "b", "c", "d")
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("c", "d"))
  expected <- factor(c("<Missing>", "TRT", "CTRL", "CTRL"), levels = c("TRT", "CTRL", "<Missing>"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("fct_collapse_only gives comprehensible error when `.na_level` is contained in new levels", {
  x <- factor(c("a", "b", "c"))
  testthat::expect_error(
    fct_collapse_only(x, d = "a", e = "b", missing = "c", .na_level = "missing"),
    ".na_level currently set to 'missing' must not be contained in the new levels"
  )
})
