testthat::test_that(
  "stat_mean_ci works for series without NAs (including extreme case n = 1 and various n_min values)",
  {
    # n = 1, na.rm = TRUE, n_min = 2
    result <- stat_mean_ci(x = 1, gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    result_gg <- stat_mean_ci(x = 1)

    res <- testthat::expect_silent(result_gg)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 2
    result <- round(stat_mean_ci(x = 1:2, gg_helper = FALSE), digits = 2)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 3
    result <- stat_mean_ci(x = 1:2, n_min = 3, gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "stat_mean_ci works for series with NAs (including extreme case n = 1 and various n_min values)",
  {
    # n = 0, na.rm = TRUE, n_min = 2
    result <- stat_mean_ci(x = rep(NA, 10), gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    result_gg <- stat_mean_ci(x = rep(NA, 10))

    res <- testthat::expect_silent(result_gg)
    testthat::expect_snapshot(res)

    # n = 0, na.rm = FALSE, n_min = 2
    result <- stat_mean_ci(x = rep(NA, 10), na.rm = FALSE, gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    result_gg <- stat_mean_ci(x = rep(NA, 10), na.rm = FALSE)

    res <- testthat::expect_silent(result_gg)
    testthat::expect_snapshot(res)

    # n = 1, na.rm = TRUE, n_min = 2
    result <- stat_mean_ci(x = c(1, NA, NA, NA), gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 1, na.rm = FALSE, n_min = 2
    result <- stat_mean_ci(x = c(1, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 2
    result <- round(stat_mean_ci(x = c(1:2, NA, NA, NA), gg_helper = FALSE), digits = 2)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    result_gg <- round(stat_mean_ci(x = c(1:2, NA, NA, NA)), digits = 2)

    res <- testthat::expect_silent(result_gg)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 3
    result <- stat_mean_ci(x = c(1:2, NA, NA, NA), n_min = 3, gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = FALSE, n_min = 2
    result <- stat_mean_ci(x = c(1:2, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = FALSE, n_min = 3
    result <- round(
      stat_mean_ci(
        x = c(1:2, NA, NA, NA), na.rm = FALSE, n_min = 3, gg_helper = FALSE
      ),
      digits = 2
    )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "stat_mean_pval works for series without NAs (including extreme case n = 1 and various n_min values)",
  {
    # n = 1, na.rm = TRUE, n_min = 2
    result <- stat_mean_pval(x = 1)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 2
    result <- round(stat_mean_pval(x = 1:2), digits = 2)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 3
    result <- stat_mean_pval(x = 1:2, n_min = 3)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

testthat::test_that(
  "stat_mean_pval works for series with NAs (including extreme case n = 1 and various n_min values)",
  {
    # n = 0, na.rm = TRUE, n_min = 2
    result <- stat_mean_pval(x = rep(NA, 10))

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 0, na.rm = FALSE, n_min = 2
    result <- stat_mean_pval(x = rep(NA, 10), na.rm = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 1, na.rm = TRUE, n_min = 2
    result <- stat_mean_pval(x = c(1, NA, NA, NA))

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 1, na.rm = FALSE, n_min = 2
    result <- stat_mean_pval(x = c(1, NA, NA, NA), na.rm = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 2
    result <- round(stat_mean_pval(x = c(1:2, NA, NA, NA)), digits = 2)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = TRUE, n_min = 3
    result <- stat_mean_pval(x = c(1:2, NA, NA, NA), n_min = 3)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = FALSE, n_min = 2
    result <- stat_mean_pval(x = c(1:2, NA, NA, NA), na.rm = FALSE)

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)

    # n = 2, na.rm = FALSE, n_min = 3
    result <- round(
      stat_mean_pval(
        x = c(1:2, NA, NA, NA), na.rm = FALSE, n_min = 3
      ),
      digits = 2
    )

    res <- testthat::expect_silent(result)
    testthat::expect_snapshot(res)
  }
)

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_gg <- stat_median_ci(x = 1)

  res <- testthat::expect_silent(result_gg)
  testthat::expect_snapshot(res)

  # n = 10, na.rm = TRUE
  result <- stat_median_ci(x = 1:10, gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("stat_median_ci works for series with NAs (including extreme case n = 1)", {
  # n = 0, na.rm = TRUE
  result <- stat_median_ci(x = rep(NA_real_, 10), gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_gg <- stat_median_ci(x = rep(NA_real_, 10))

  res <- testthat::expect_silent(result_gg)
  testthat::expect_snapshot(res)

  # n = 0, na.rm = FALSE
  result <- stat_median_ci(x = rep(NA_real_, 10), na.rm = FALSE, gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_gg <- stat_median_ci(x = rep(NA_real_, 10), na.rm = FALSE)

  res <- testthat::expect_silent(result_gg)
  testthat::expect_snapshot(res)

  # n = 1, na.rm = TRUE
  result <- stat_median_ci(x = c(1, NA, NA, NA), gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # n = 1, na.rm = FALSE
  result <- stat_median_ci(x = c(1, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # n = 10, na.rm = TRUE
  result <- stat_median_ci(x = c(1:10, NA, NA, NA), gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result_gg <- stat_median_ci(x = c(1:10, NA, NA, NA))

  res <- testthat::expect_silent(result_gg)
  testthat::expect_snapshot(res)

  # n = 10, na.rm = FALSE
  result <- stat_median_ci(x = c(1:10, NA, NA, NA), na.rm = FALSE, gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("stat_median_ci works for named numeric values when name is missing)", {
  x <- integer(0)
  attr(x, "names") <- character(0)

  result <- stat_median_ci(x, gg_helper = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("stat_propdiff_ci works with names and multiple values in x and y)", {
  x <- list(5, 7.5, 10)
  y <- list(5, 1, 10)
  list_names <- c("A", "B", "C")

  result <- stat_propdiff_ci(x = x, y = y, N_x = 10, N_y = 20, list_names = list_names)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("stat_propdiff_ci works with custom arguments)", {
  x <- integer(0)
  attr(x, "names") <- character(0)

  result <- stat_propdiff_ci(x = list(1.95), y = list(0.05), N_x = 5, N_y = 5, conf_level = 0.9, pct = FALSE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
