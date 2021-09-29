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
  x <- c(13.25, 8.15, 1, 2.834, NA)
  result <- month2day(x)
  expected <- c(403.296875, 248.065625, 30.4375, 86.259875, NA)
  expect_equal(result, expected)
})

test_that("day2month works correctly", {
  x <- c(403, 248, 30, 86, NA)
  result <- day2month(x)
  expected <- c(13.2402464065708, 8.14784394250513, 0.985626283367556, 2.82546201232033, NA)
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

test_that("get_smooths dimensions without grouping", {
  air_smooths <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone")

  expect_identical(nrow(air_smooths), sum(complete.cases(airquality[, c("Solar.R", "Ozone")])))
  expect_identical(ncol(air_smooths), 4L)
})

test_that("get_smooths dimensions with grouping", {
  mt_smooths <- get_smooths(mtcars, "wt", "mpg", c("am"))

  expect_identical(nrow(mt_smooths), sum(complete.cases(mtcars[, c("wt", "mpg", "am")])))
  expect_identical(ncol(mt_smooths), 5L)
})

test_that("get_smooths proper splits across groups", {
  air_smooths2 <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone", groups = "Month")
  air_full <- airquality[complete.cases(airquality[, c("Solar.R", "Ozone", "Month")]), ]

  expect_identical(
    unlist(by(air_full, air_full$Month, function(d) range(d$`Solar.R`, na.rm = TRUE))),
    unlist(by(air_smooths2, air_smooths2$Month, function(d) range(d$`x`)))
  )
})

test_that("get_smooths relative intervals level", {
  air_smooths3a <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone", groups = "Month", level = 0.95)
  air_smooths3b <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone", groups = "Month", level = 0.8)

  expect_true(all(air_smooths3b$ylow >= air_smooths3a$ylow))
  expect_true(all(air_smooths3b$yhigh <= air_smooths3a$yhigh))
})

test_that("n_available works as expected", {
  x <- c(1, 2, 3, NA)
  result <- n_available(x)
  expected <- 3L
  expect_identical(result, expected)
})

##################
## range_noinf
##################

# INTEGER no zero-len data, no NAs, no Inf

test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] and with all default options", {

  x <- 5:1

  result <- range_noinf(x)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (d)]", {

  x <- 5:1

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- 5:1

  result <- range_noinf(x, finite = TRUE)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- 5:1

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

# INTEGER no zero-len data, with NAs, no Inf

test_that("range_noinf for INT [no zero-len data, with NAs, no Inf] and with default options", {

  x <- c(NA, 5:1, NA)

  result <- range_noinf(x)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(NA, 5:1, NA)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(NA, 5:1, NA)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(NA, 5:1, NA)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(1L, 5L)

  expect_identical(result, expected)

})

# INTEGER  with zero-len data, no NAs, no Inf

test_that("range_noinf for INT [with zero-len data, no NAs, no Inf] and with all default options", {

  x <- vector(mode = "integer", length = 0)

  result <- range_noinf(x)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- vector(mode = "integer", length = 0)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- vector(mode = "integer", length = 0)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- vector(mode = "integer", length = 0)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

# INTEGER with zero-len data, with NAs, no Inf

test_that("range_noinf for INT [with zero-len data, with NAs, no Inf] and with default options", {

  x <- rep(NA_integer_, 4)

  result <- range_noinf(x)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- rep(NA_integer_, 4)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- rep(NA_integer_, 4)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

test_that("range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- rep(NA_integer_, 4)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(NA_integer_, NA_integer_)

  expect_identical(result, expected)

})

# DOUBLE no zero-len data, no NAs, no Inf

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] and with all default options", {

  x <- c(3.5, 1.5, 2.5)

  result <- range_noinf(x)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(3.5, 1.5, 2.5)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(3.5, 1.5, 2.5)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(3.5, 1.5, 2.5)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

# DOUBLE no zero-len data, no NAs, with Inf

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] and with all default options", {

  x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

  result <- range_noinf(x)
  expected <- c(-Inf, Inf)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(-Inf, Inf)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

# DOUBLE no zero-len data, with NAs, no Inf

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] and with default options", {

  x <- c(NA, 3.5, 1.5, 2.5, NA)

  result <- range_noinf(x)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(NA, 3.5, 1.5, 2.5, NA)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(NA, 3.5, 1.5, 2.5, NA)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(NA, 3.5, 1.5, 2.5, NA)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

# DOUBLE  no zero-len data, with NAs, with Inf

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] and with default options", {

  x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

  result <- range_noinf(x)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(-Inf, Inf)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(1.5, 3.5)

  expect_identical(result, expected)

})

# DOUBLE with zero-len data, no NAs, no Inf

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] and with all default options", {

  x <- vector(mode = "double", length = 0)

  result <- range_noinf(x)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- vector(mode = "double", length = 0)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- vector(mode = "double", length = 0)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- vector(mode = "double", length = 0)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

# DOUBLE with zero-len data, no NAs, with Inf

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] and with all default options", {

  x <- c(Inf, -Inf, Inf, -Inf)

  result <- range_noinf(x)
  expected <- c(-Inf, Inf)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(Inf, -Inf, Inf, -Inf)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(-Inf, Inf)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(Inf, -Inf, Inf, -Inf)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(Inf, -Inf, Inf, -Inf)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

# DOUBLE with zero-len data, with NAs, no Inf

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] and with default options", {

  x <- rep(NA_real_, 4)

  result <- range_noinf(x)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- rep(NA_real_, 4)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- rep(NA_real_, 4)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- rep(NA_real_, 4)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

# DOUBLE with zero-len data, with NAs, with Inf

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] and with default options", {

  x <- c(NA, Inf, -Inf, NA)

  result <- range_noinf(x)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]", {

  x <- c(NA, Inf, -Inf, NA)

  result <- range_noinf(x, na.rm = TRUE)
  expected <- c(-Inf, Inf)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]", {

  x <- c(NA, Inf, -Inf, NA)

  result <- range_noinf(x, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = TRUE]", {

  x <- c(NA, Inf, -Inf, NA)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)
  expected <- c(NA_real_, NA_real_)

  expect_identical(result, expected)

})

test_that("wrong inputs try_fun", {
  expect_error(try_fun("FUN_NOTEXISTS", a = 2))
  expect_error(try_fun("mean", aa = 2))
})

test_that("too long evaluation try_fun", {
  expect_error(try_fun("Sys.sleep", 10, timeout = 1))
})

test_that("correct try_fun", {
  expect_identical(try_fun("mean", 10), 10)
  expect_equal(coef(try_fun("clogit", I(Ozone > 140 ~ Solar.R), airquality)), c(Solar.R = 0.00830372))
})
