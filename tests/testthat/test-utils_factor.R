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
  foo <- formatters::with_label(c("a", "b"), "alphabet")
  testthat::expect_warning(
    result <- as_factor_keep_attributes(foo, verbose = TRUE),
    "automatically converting character variable foo to factor"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("as_factor_keep_attributes converts empty strings for a character vector", {
  foo <- formatters::with_label(c("a", "", "b"), "alphabet")
  testthat::expect_warning(
    result <- as_factor_keep_attributes(foo, na_level = "missing", verbose = TRUE),
    "automatically converting character variable foo to factor"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("as_factor_keep_attributes shows correct name of vector in warning", {
  foo <- formatters::with_label(c("a", "b"), "alphabet")
  testthat::expect_warning(
    as_factor_keep_attributes(foo, x_name = "FOO", verbose = TRUE),
    "automatically converting character variable FOO to factor"
  )
})

testthat::test_that("as_factor_keep_attributes does not modify a factor at all", {
  result <- testthat::expect_silent(as_factor_keep_attributes(factor(c(1, 2))))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# bins_percent_labels ----

testthat::test_that("bins_percent_labels works as expected", {
  res <- testthat::expect_silent(bins_percent_labels(c(0.2, 0.555, 0.8)))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(bins_percent_labels(c(0.2, 0.555, 0.8), digits = 1))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(bins_percent_labels(c()))
  testthat::expect_snapshot(res)
})

# cut_quantile_bins ----

testthat::test_that("cut_quantile_bins works as expected with default settings", {
  result <- cut_quantile_bins(cars$speed)
  testthat::expect_s3_class(result, "ordered")

  res <- testthat::expect_silent(levels(result))
  testthat::expect_snapshot(res)
  testthat::expect_identical(length(result), length(cars$speed))

  q1 <- stats::quantile(cars$speed, probs = 0.25)
  testthat::expect_true(identical(result == "[0%,25%]", cars$speed <= q1))
  testthat::expect_true(identical(result != "[0%,25%]", cars$speed > q1))
})

testthat::test_that("cut_quantile_bins works with custom quantiles", {
  result <- cut_quantile_bins(cars$speed, probs = c(0.1, 0.3, 0.8))

  res <- testthat::expect_silent(levels(result))
  testthat::expect_snapshot(res)
})

testthat::test_that("cut_quantile_bins works with custom labels", {
  result <- cut_quantile_bins(
    cars$speed,
    probs = c(0.1, 0.3, 0.8),
    labels = c("low", "medium", "high", "top")
  )

  res <- testthat::expect_silent(levels(result))
  testthat::expect_snapshot(res)
})

testthat::test_that("cut_quantile_bins preserves NAs in result", {
  x <- airquality$Ozone
  checkmate::assert_true(
    any(is.na(x)),
    !all(is.na(x))
  )
  result <- cut_quantile_bins(x)
  testthat::expect_identical(is.na(x), is.na(result))
})

testthat::test_that("cut_quantile_bins also works when there are only NAs", {
  x <- rep(NA_real_, 10)
  result <- testthat::expect_silent(cut_quantile_bins(x))

  res <- testthat::expect_silent(levels(result))
  testthat::expect_snapshot(res)
  testthat::expect_true(all(is.na(result)))
})

testthat::test_that("cut_quantile_bins gives error message if there are duplicate quantiles", {
  x <- c(rep(NA_real_, 10), 1)
  testthat::expect_error(cut_quantile_bins(x))
})

testthat::test_that("cut_quantile_bins does work also if an empty `probs` vector is used", {
  x <- 1:10
  result <- testthat::expect_silent(cut_quantile_bins(x, probs = c(0, 1)))
  result <- testthat::expect_silent(cut_quantile_bins(x, probs = c()))
  testthat::expect_true(all(result == "[0%,100%]"))
})

# fct_discard ----

testthat::test_that("fct_discard works as expected", {
  x <- factor(c("a", "b", "c"))
  result <- fct_discard(x, "b")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# fct_explicit_na_if ----

testthat::test_that("fct_explicit_na_if works as expected with factor input", {
  x <- factor(c("a", "b", NA))
  cond <- c(TRUE, FALSE, FALSE)
  result <- fct_explicit_na_if(x, cond)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# fct_collapse_only ----

testthat::test_that("fct_collapse_only works as expected", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("c", "d"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("fct_collapse_only uses the customized `na_level` as expected", {
  x <- factor(c(NA, "b", "c", "d"), levels = c("b", "c", "d"))
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("d"), .na_level = "Missing")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("fct_collapse_only works as expected with character input", {
  x <- c("a", "b", "c", "d")
  result <- fct_collapse_only(x, TRT = "b", CTRL = c("c", "d"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("fct_collapse_only gives comprehensible error when `.na_level` is contained in new levels", {
  x <- factor(c("a", "b", "c"))
  testthat::expect_error(
    fct_collapse_only(x, d = "a", e = "b", missing = "c", .na_level = "missing"),
    ".na_level currently set to 'missing' must not be contained in the new levels"
  )
})
