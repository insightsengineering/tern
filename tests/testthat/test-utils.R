test_that("as_factor_keep_attributes works correctly for a character vector", {
  foo <- with_label(c("a", "b"), "alphabet")
  result <- expect_warning(
    as_factor_keep_attributes(foo),
    "automatically converting character variable foo to factor"
  )
  expected <- with_label(factor(c("a", "b")), "alphabet")
  expect_identical(result, expected)
})

test_that("as_factor_keep_attributes converts empty strings for a character vector", {
  foo <- with_label(c("a", "", "b"), "alphabet")
  result <- expect_warning(
    as_factor_keep_attributes(foo, na_level = "missing"),
    "automatically converting character variable foo to factor"
  )
  expected <- with_label(factor(c("a", "missing", "b"), levels = c("a", "b", "missing")), "alphabet")
  expect_identical(result, expected)
})

test_that("as_factor_keep_attributes shows correct name of vector in warning", {
  foo <- with_label(c("a", "b"), "alphabet")
  expect_warning(
    as_factor_keep_attributes(foo, x_name = "FOO"),
    "automatically converting character variable FOO to factor"
  )
})

test_that("as_factor_keep_attributes does not modify a factor at all", {
  foo <- factor(c(1, 2))
  result <- expect_silent(as_factor_keep_attributes(foo))
  expect_identical(result, foo)
})

test_that("f_conf_level works for proportion", {
  result <- f_conf_level(0.95)
  expected <- "95% CI"
  expect_identical(result, expected)
})
test_that("f_conf_level fails for non-proportion input", {
  expect_error(f_conf_level(1.1))
  expect_error(f_conf_level(-1))
})

test_that("make_names works as expected", {
  nams <- c("Any Grade (%)", "Total AE numbers!", "No adverse events ...")
  result <- make_names(nams)
  expected <- c("AnyGrade", "TotalAEnumbers", "Noadverseevents")
  expect_identical(result, expected)
})

test_that("get_covariates works for a character vector", {
  result <- get_covariates(c("a * b", "c"))
  expected <- list(a = "a", b = "b", c = "c")
  expect_identical(result, expected)
})
test_that("get_covariates fails for non-character input", {
  expect_error(get_covariates(c(1, 2)))
  expect_error(get_covariates(factor(c("a", "b", "b"))))
})

test_that("month2day works correctly", {
  x <- c(13.25, 8.15, 1, 2.834)
  result <- month2day(x)
  expected <- c(403.296875, 248.065625, 30.4375, 86.259875)
  expect_equal(result, expected)
})

test_that("day2month works correctly", {
  x <- c(403, 248, 30, 86)
  result <- day2month(x)
  expected <- c(13.2402464065708, 8.14784394250513, 0.985626283367556, 2.82546201232033)
  expect_equal(result, expected)
})

test_that("combine_vectors works correctly", {
  x <- c(1:3)
  y <- c(4:6)
  result <- combine_vectors(x, y)

  expected <- list(
    c(1, 4),
    c(2, 5),
    c(3, 6)
  )

  expect_equal(result, expected)
})

test_that("empty_vector_if_na works correctly", {
  x <- c(NA, NA, NA)
  result <- empty_vector_if_na(x)
  expected <- numeric()
  expect_equal(result, expected)
})

test_that("extract works for NULL input", {
  expect_identical(extract(NULL, "bla"), NULL)
})

test_that("extract works for non-NULL input", {
  result <- extract(
    x = c(a = "xx", b = function(x) paste(x, "bla")),
    c("b", "c")
  )
  expected <- c(b = function(x) paste(x, "bla"))
  expect_identical(result, expected)
})

test_that("extract returns NULL when there is no overlap", {
  result <- extract(
    x = c(a = "xx", b = "xx.xx"),
    c("d", "c")
  )
  expected <- NULL
  expect_identical(result, expected)
})

test_that("aesi_label works as expected for SMQ", {

  smq01nam <- c("AESI 1", "", NA)
  smq01sc <- c("NARROW", "", NA)

  result <- aesi_label(smq01nam, smq01sc)
  expected <- "AESI 1 (NARROW)"
  expect_identical(result, expected)
}) #test with NA

test_that("aesi_label works as expected for CQ", {

  aesi1 <- c("AESI CQ1", "", NA)
  result <- aesi_label(aesi1, scope = NULL)
  expected <- "AESI CQ1"
  expect_identical(result, expected)
})

test_that("aesi_label works as expected when input includes multiple values", {

  aesi1 <- c("AESI CQ1", "AESI CQ2")
  result <- aesi_label(aesi1, scope = NULL)

  expected <- NULL
  expect_identical(result, expected)

  aesi2 <- with_label(c("AESI CQ1", "AESI CQ2"), label = "CQ: ABC")
  result <- aesi_label(aesi2, scope = NULL)

  expected <- "CQ: ABC"
  expect_identical(result, expected)

})
