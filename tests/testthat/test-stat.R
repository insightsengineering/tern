testthat::test_that("stat_mean_ci works for series without NAs
                    (including extreme case n = 1 and various n_min values)", {

  # n = 1, na.rm = TRUE, n_min = 2
  result <- stat_mean_ci(x = 1, gg_helper = FALSE)
  result_gg <- stat_mean_ci(x = 1)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  expected_gg <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 2, na.rm = TRUE, n_min = 2
  result <- round(stat_mean_ci(x = 1:2, gg_helper = FALSE), digits = 2)
  expected <- c(mean_ci_lwr = -4.85, mean_ci_upr = 7.85)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 3
  result <- stat_mean_ci(x = 1:2, n_min = 3, gg_helper = FALSE)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  testthat::expect_identical(result, expected)
})

testthat::test_that("stat_mean_ci works for series with NAs
                    (including extreme case n = 1 and various n_min values)", {

  # n = 0, na.rm = TRUE, n_min = 2
  result <- stat_mean_ci(x = rep(NA, 10), gg_helper = FALSE)
  result_gg <- stat_mean_ci(x = rep(NA, 10))
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  expected_gg <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 0, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = rep(NA, 10), na.rm = FALSE, gg_helper = FALSE)
  result_gg <- stat_mean_ci(x = rep(NA, 10), na.rm = FALSE)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  expected_gg <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 1, na.rm = TRUE, n_min = 2
  result <- stat_mean_ci(x = c(1, NA, NA, NA), gg_helper = FALSE)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 1, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = c(1, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 2
  result <- round(stat_mean_ci(x = c(1:2, NA, NA, NA), gg_helper = FALSE), digits = 2)
  result_gg <- round(stat_mean_ci(x = c(1:2, NA, NA, NA)), digits = 2)
  expected <- c(mean_ci_lwr = -4.85, mean_ci_upr = 7.85)
  expected_gg <- data.frame(y = 1.5, ymin = -4.85, ymax = 7.85)
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 2, na.rm = TRUE, n_min = 3
  result <- stat_mean_ci(x = c(1:2, NA, NA, NA), n_min = 3, gg_helper = FALSE)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = c(1:2, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 3
  result <- round(
    stat_mean_ci(
      x = c(1:2, NA, NA, NA), na.rm = FALSE, n_min = 3, gg_helper = FALSE
    ),
    digits = 2
  )
  expected <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  testthat::expect_identical(result, expected)
})

testthat::test_that("stat_mean_pval works for series without NAs
                    (including extreme case n = 1 and various n_min values)", {

  # n = 1, na.rm = TRUE, n_min = 2
  result <- stat_mean_pval(x = 1)
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 2
  result <- round(stat_mean_pval(x = 1:2), digits = 2)
  expected <- c(p_value = 0.2)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 3
  result <- stat_mean_pval(x = 1:2, n_min = 3)
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)
})

testthat::test_that("stat_mean_pval works for series with NAs
                    (including extreme case n = 1 and various n_min values)", {

  # n = 0, na.rm = TRUE, n_min = 2
  result <- stat_mean_pval(x = rep(NA, 10))
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 0, na.rm = FALSE, n_min = 2
  result <- stat_mean_pval(x = rep(NA, 10), na.rm = FALSE)
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 1, na.rm = TRUE, n_min = 2
  result <- stat_mean_pval(x = c(1, NA, NA, NA))
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 1, na.rm = FALSE, n_min = 2
  result <- stat_mean_pval(x = c(1, NA, NA, NA), na.rm = FALSE)
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 2
  result <- round(stat_mean_pval(x = c(1:2, NA, NA, NA)), digits = 2)
  expected <- c(p_value = 0.2)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 3
  result <- stat_mean_pval(x = c(1:2, NA, NA, NA), n_min = 3)
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 2
  result <- stat_mean_pval(x = c(1:2, NA, NA, NA), na.rm = FALSE)
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 3
  result <- round(
    stat_mean_pval(
      x = c(1:2, NA, NA, NA), na.rm = FALSE, n_min = 3
    ),
    digits = 2
  )
  expected <- c(p_value = NA_real_)
  testthat::expect_identical(result, expected)
})

testthat::test_that("stat_mean_pval returns the correct p-value", {

  # n = 5, na.rm = TRUE, test_mean = 0
  x <- 1:5
  result <- stat_mean_pval(x)
  expected <- c(p_value = t.test(x)$p.value)
  testthat::expect_equal(result, expected)

  # n = 3, na.rm = TRUE, test_mean = 0.5
  x <- 1:3
  result <- stat_mean_pval(x, test_mean = 0.5)
  expected <- c(p_value = t.test(x, mu = 0.5)$p.value)
  testthat::expect_equal(result, expected)
})

testthat::test_that("stat_median_ci works for series without NAs (including extreme case n = 1)", {

  # n = 1, na.rm = TRUE
  result <- stat_median_ci(x = 1, gg_helper = FALSE)
  result_gg <- stat_median_ci(x = 1)
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  expected_gg <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  attr(expected_gg, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 10, na.rm = TRUE
  result <- stat_median_ci(x = 1:10, gg_helper = FALSE)
  attr(result, "conf_level") <- round(attr(result, "conf_level"), digits = 2)
  expected <- c(median_ci_lwr = 2L, median_ci_upr = 9L)
  attr(expected, "conf_level") <- 0.98
  testthat::expect_identical(result, expected)
})

testthat::test_that("stat_median_ci works for series with NAs (including extreme case n = 1)", {

  # n = 0, na.rm = TRUE
  result <- stat_median_ci(x = rep(NA_real_, 10), gg_helper = FALSE)
  result_gg <- stat_median_ci(x = rep(NA_real_, 10))
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  expected_gg <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  attr(expected_gg, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 0, na.rm = FALSE
  result <- stat_median_ci(x = rep(NA_real_, 10), na.rm = FALSE, gg_helper = FALSE)
  result_gg <- stat_median_ci(x = rep(NA_real_, 10), na.rm = FALSE)
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  expected_gg <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  attr(expected_gg, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 1, na.rm = TRUE
  result <- stat_median_ci(x = c(1, NA, NA, NA), gg_helper = FALSE)
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)

  # n = 1, na.rm = FALSE
  result <- stat_median_ci(x = c(1, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)

  # n = 10, na.rm = TRUE
  result <- stat_median_ci(x = c(1:10, NA, NA, NA), gg_helper = FALSE)
  result_gg <- stat_median_ci(x = c(1:10, NA, NA, NA))
  attr(result, "conf_level") <- round(attr(result, "conf_level"), digits = 2)
  attr(result_gg, "conf_level") <- round(attr(result_gg, "conf_level"), digits = 2)
  expected <- c(median_ci_lwr = 2L, median_ci_upr = 9L)
  expected_gg <- data.frame(y = 5.5, ymin = 2L, ymax = 9L)
  attr(expected, "conf_level") <- 0.98
  attr(expected_gg, "conf_level") <- 0.98
  testthat::expect_identical(result, expected)
  testthat::expect_identical(result_gg, expected_gg)

  # n = 10, na.rm = FALSE
  result <- stat_median_ci(x = c(1:10, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)
})

testthat::test_that("stat_median_ci works for named numeric values when name is missing)", {
  x <- integer(0)
  attr(x, "names") <- character(0)

  result <- stat_median_ci(x, gg_helper = FALSE)
  expected <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  testthat::expect_identical(result, expected)
})
