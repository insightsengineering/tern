
# Format wrapper for `s_summary`.
afun_s_summary <- format_wrap_x(
  sfun = s_summary,
  indent_mods = c(n = 0L, mean_sd = 0L, median = 0L, range = 0L),
  formats = c(
    n = "xx", mean_sd = "xx.x (xx.x)", median = "xx.x", range = "xx.x - xx.x"
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
