testthat::test_that("f_conf_level works for proportion", {
  result <- f_conf_level(0.95)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("f_conf_level fails for non-proportion input", {
  testthat::expect_error(f_conf_level(1.1))
  testthat::expect_error(f_conf_level(-1))
})

testthat::test_that("f_pval works for numeric input", {
  result <- f_pval(0.05)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("f_pval fails for non-numeric input", {
  testthat::expect_error(f_pval("a"))
  testthat::expect_error(f_pval(NULL))
})

testthat::test_that("make_names works as expected", {
  nams <- c("Any Grade (%)", "Total AE numbers!", "No adverse events ...")
  result <- make_names(nams)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("get_covariates works for a character vector", {
  result <- get_covariates(c("a * b", "c"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("get_covariates fails for non-character input", {
  testthat::expect_error(get_covariates(c(1, 2)))
  testthat::expect_error(get_covariates(factor(c("a", "b", "b"))))
})

testthat::test_that("to_n fails for non-character input", {
  testthat::expect_equal(to_n(NULL, 3), NULL)
  testthat::expect_equal(to_n("a", 3), rep("a", 3))
  testthat::expect_equal(to_n(rep("a", 3), 3), rep("a", 3))
  testthat::expect_error(to_n(rep("a", 2), 3))
})

testthat::test_that("month2day works correctly", {
  x <- c(13.25, 8.15, 1, 2.834, NA)
  result <- month2day(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("day2month works correctly", {
  x <- c(403, 248, 30, 86, NA)
  result <- day2month(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("empty_vector_if_na works correctly", {
  x <- c(NA, NA, NA)
  result <- empty_vector_if_na(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("extract_by_name works for NULL input", {
  testthat::expect_identical(extract_by_name(NULL, "bla"), NULL)
})

testthat::test_that("extract_by_name works for non-NULL input", {
  result <- extract_by_name(
    x = c(a = "xx", b = function(x) paste(x, "bla")),
    c("b", "c")
  )
  expected <- c(b = function(x) paste(x, "bla"))
  testthat::expect_identical(result, expected)
})

testthat::test_that("extract_by_name returns NULL when there is no overlap", {
  result <- extract_by_name(
    x = c(a = "xx", b = "xx.xx"),
    c("d", "c")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("aesi_label works as expected for SMQ", {
  smq01nam <- c("AESI 1", "", NA)
  smq01sc <- c("NARROW", "", NA)
  result <- aesi_label(smq01nam, smq01sc)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
}) # test with NA

testthat::test_that("aesi_label works as expected for CQ", {
  aesi1 <- c("AESI CQ1", "", NA)
  result <- aesi_label(aesi1, scope = NULL)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("aesi_label works as expected when input includes multiple values", {
  aesi1 <- c("AESI CQ1", "AESI CQ2")
  result <- aesi_label(aesi1, scope = NULL)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  aesi2 <- formatters::with_label(c("AESI CQ1", "AESI CQ2"), label = "CQ: ABC")
  result <- aesi_label(aesi2, scope = NULL)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("get_smooths dimensions without grouping", {
  air_smooths <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone")

  testthat::expect_identical(nrow(air_smooths), sum(stats::complete.cases(airquality[, c("Solar.R", "Ozone")])))
  testthat::expect_identical(ncol(air_smooths), 4L)
})

testthat::test_that("get_smooths dimensions with grouping", {
  mt_smooths <- get_smooths(mtcars, "wt", "mpg", c("am"))

  testthat::expect_identical(nrow(mt_smooths), sum(stats::complete.cases(mtcars[, c("wt", "mpg", "am")])))
  testthat::expect_identical(ncol(mt_smooths), 5L)
})

testthat::test_that("get_smooths proper splits across groups", {
  air_smooths2 <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone", groups = "Month")
  air_full <- airquality[stats::complete.cases(airquality[, c("Solar.R", "Ozone", "Month")]), ]

  testthat::expect_identical(
    unlist(by(air_full, air_full$Month, function(d) range(d$`Solar.R`, na.rm = TRUE))),
    unlist(by(air_smooths2, air_smooths2$Month, function(d) range(d$`x`)))
  )
})

testthat::test_that("get_smooths relative intervals level", {
  air_smooths3a <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone", groups = "Month", level = 0.95)
  air_smooths3b <- get_smooths(df = airquality, x = "Solar.R", y = "Ozone", groups = "Month", level = 0.8)

  testthat::expect_true(all(air_smooths3b$ylow >= air_smooths3a$ylow))
  testthat::expect_true(all(air_smooths3b$yhigh <= air_smooths3a$yhigh))
})

testthat::test_that("n_available works as expected", {
  x <- c(1, 2, 3, NA)
  result <- n_available(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

##################
## range_noinf
##################

# INTEGER no zero-len data, no NAs, no Inf

testthat::test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] and with all default options", {
  x <- 5:1

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (d)]", {
  x <- 5:1

  result <- range_noinf(x, na.rm = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- 5:1

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {
  x <- 5:1

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# INTEGER no zero-len data, with NAs, no Inf

testthat::test_that("range_noinf for INT [no zero-len data, with NAs, no Inf] and with default options", {
  x <- c(NA, 5:1, NA)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- c(NA, 5:1, NA)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- c(NA, 5:1, NA)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {
  x <- c(NA, 5:1, NA)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# INTEGER  with zero-len data, no NAs, no Inf

testthat::test_that("range_noinf for INT [with zero-len data, no NAs, no Inf] and with all default options", {
  x <- vector(mode = "integer", length = 0)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- vector(mode = "integer", length = 0)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- vector(mode = "integer", length = 0)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {
  x <- vector(mode = "integer", length = 0)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# INTEGER with zero-len data, with NAs, no Inf

testthat::test_that("range_noinf for INT [with zero-len data, with NAs, no Inf] and with default options", {
  x <- rep(NA_integer_, 4)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- rep(NA_integer_, 4)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- rep(NA_integer_, 4)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {
  x <- rep(NA_integer_, 4)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# DOUBLE no zero-len data, no NAs, no Inf

testthat::test_that("range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] and with all default options", {
  x <- c(3.5, 1.5, 2.5)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- c(3.5, 1.5, 2.5)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- c(3.5, 1.5, 2.5)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {
  x <- c(3.5, 1.5, 2.5)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# DOUBLE no zero-len data, no NAs, with Inf

testthat::test_that("range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] and with all default options", {
  x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = TRUE]",
  code = {
    x <- c(Inf, 3.5, 1.5, -Inf, 2.5)

    result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# DOUBLE no zero-len data, with NAs, no Inf

testthat::test_that("range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] and with default options", {
  x <- c(NA, 3.5, 1.5, 2.5, NA)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- c(NA, 3.5, 1.5, 2.5, NA)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- c(NA, 3.5, 1.5, 2.5, NA)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]",
  code = {
    x <- c(NA, 3.5, 1.5, 2.5, NA)

    result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# DOUBLE  no zero-len data, with NAs, with Inf

testthat::test_that("range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] and with default options", {
  x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

  result <- range_noinf(x)
  expected <- c(NA_real_, NA_real_)

  testthat::expect_identical(result, expected)
})

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = TRUE]",
  code = {
    x <- c(NA, 3.5, Inf, 1.5, -Inf, 2.5, NA)

    result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# DOUBLE with zero-len data, no NAs, no Inf

testthat::test_that("range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] and with all default options", {
  x <- vector(mode = "double", length = 0)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- vector(mode = "double", length = 0)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- vector(mode = "double", length = 0)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that("range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = TRUE]", {
  x <- vector(mode = "double", length = 0)

  result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# DOUBLE with zero-len data, no NAs, with Inf

testthat::test_that("range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] and with all default options", {
  x <- c(Inf, -Inf, Inf, -Inf)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  {
    x <- c(Inf, -Inf, Inf, -Inf)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]",
  {
    x <- c(Inf, -Inf, Inf, -Inf)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = TRUE]",
  {
    x <- c(Inf, -Inf, Inf, -Inf)

    result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# DOUBLE with zero-len data, with NAs, no Inf

testthat::test_that("range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] and with default options", {
  x <- rep(NA_real_, 4)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- rep(NA_real_, 4)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- rep(NA_real_, 4)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]",
  code = {
    x <- rep(NA_real_, 4)

    result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

# DOUBLE with zero-len data, with NAs, with Inf

testthat::test_that("range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] and with default options", {
  x <- c(NA, Inf, -Inf, NA)

  result <- range_noinf(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]",
  code = {
    x <- c(NA, Inf, -Inf, NA)

    result <- range_noinf(x, na.rm = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]",
  code = {
    x <- c(NA, Inf, -Inf, NA)

    result <- range_noinf(x, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = TRUE]",
  code = {
    x <- c(NA, Inf, -Inf, NA)

    result <- range_noinf(x, na.rm = TRUE, finite = TRUE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)
