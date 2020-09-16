testthat::test_that("s_summary.numeric returns NA for x length 0L.", {

  testthat::expect_equivalent(
    s_summary(numeric()),
    list(
      n = 0, range = c(NA_real_, NA_real_),
      median_sd = NA_real_, mean = c(NA_real_, NA_real_)
    )
  )

})

testthat::test_that("s_summary.numeric handles NA.", {

  x <- c(NA_real_, 1)
  s_summary(x, na.rm = TRUE)
  testthat::expect_equivalent(
    s_summary(x),
    list(n = 1, mean_sd = c(1, NA), median = 1, range = c(1, 1))
  )

  testthat::expect_equivalent(
    s_summary(x, na.rm = FALSE),
    list(
      n = 2, mean_sd = c(NA_real_, NA_real_),
      median = NA_real_, range = c(NA_real_, NA_real_)
    )
  )

})

testthat::test_that("s_summary.numeric returns right results.", {

  x <- c(NA_real_, 1, 2)
  testthat::expect_equivalent(
    s_summary(x),
    list(
      n = 2, mean_sd = c(1.5, 0.7071068), median = 1.5, range = c(1, 2)
    ),
    tolerance = .00001
  )

})
