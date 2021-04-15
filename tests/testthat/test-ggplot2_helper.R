library(dplyr)


test_that("stat_mean_ci works for series without NAs (including extreme case n = 1 and various n_min values)", {

  # n = 1, na.rm = TRUE, n_min = 2
  result <- stat_mean_ci(x = 1)
  expected <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 1, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = 1, na.rm = FALSE)
  expected <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 2
  result <- round(stat_mean_ci(x = 1:2), digits = 2)
  expected <- data.frame(y = 1.5, ymin = -4.85, ymax = 7.85)
  expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 3
  result <- stat_mean_ci(x = 1:2, n_min = 3)
  expected <- data.frame(y = 1.5, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 2
  result <- round(stat_mean_ci(x = 1:2, na.rm = FALSE), digits = 2)
  expected <- data.frame(y = 1.5, ymin = -4.85, ymax = 7.85)
  expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 3
  result <- stat_mean_ci(x = 1:2, n_min = 3, na.rm = FALSE)
  expected <- data.frame(y = 1.5, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

})

test_that("stat_mean_ci works for series with NAs (including extreme case n = 1 and various n_min values)", {

  # n = 0, na.rm = TRUE, n_min = 2
  result <- stat_mean_ci(x = rep(NA, 10))
  expected <- data.frame(y = NaN, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 0, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = rep(NA, 10), na.rm = FALSE)
  expected <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 1, na.rm = TRUE, n_min = 2
  result <- stat_mean_ci(x = c(1, NA, NA, NA))
  expected <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 1, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = c(1, NA, NA, NA), na.rm = FALSE)
  expected <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 2
  result <- round(stat_mean_ci(x = c(1:2, NA, NA, NA)), digits = 2)
  expected <- data.frame(y = 1.5, ymin = -4.85, ymax = 7.85)
  expect_identical(result, expected)

  # n = 2, na.rm = TRUE, n_min = 3
  result <- stat_mean_ci(x = c(1:2, NA, NA, NA), n_min = 3)
  expected <- data.frame(y = 1.5, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 2
  result <- stat_mean_ci(x = c(1:2, NA, NA, NA), na.rm = FALSE)
  expected <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

  # n = 2, na.rm = FALSE, n_min = 3
  result <- round(stat_mean_ci(x = c(1:2, NA, NA, NA), na.rm = FALSE, n_min = 3), digits = 2)
  expected <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  expect_identical(result, expected)

})

test_that("stat_median_ci works for series without NAs (including extreme case n = 1)", {

  # n = 1, na.rm = TRUE
  result <- stat_median_ci(x = 1)
  expected <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

  # n = 1, na.rm = FALSE
  result <- stat_median_ci(x = 1, na.rm = FALSE)
  expected <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

  # n = 10, na.rm = TRUE
  result <- stat_median_ci(x = 1:10)
  attr(result, "conf_level") <- round(attr(result, "conf_level"), digits = 2)
  expected <- data.frame(y = 5.5, ymin = 2L, ymax = 9L)
  attr(expected, "conf_level") <- 0.98
  expect_identical(result, expected)

  # n = 10, na.rm = FALSE
  result <- stat_median_ci(x = 1:10, na.rm = FALSE)
  attr(result, "conf_level") <- round(attr(result, "conf_level"), digits = 2)
  expected <- data.frame(y = 5.5, ymin = 2L, ymax = 9L)
  attr(expected, "conf_level") <- 0.98
  expect_identical(result, expected)

})

test_that("stat_median_ci works for series with NAs (including extreme case n = 1)", {

  # n = 0, na.rm = TRUE
  result <- stat_median_ci(x = rep(NA, 10))
  expected <- data.frame(y = NA, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

  # n = 0, na.rm = FALSE
  result <- stat_median_ci(x = rep(NA, 10), na.rm = FALSE)
  expected <- data.frame(y = NA, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

  # n = 1, na.rm = TRUE
  result <- stat_median_ci(x = c(1, NA, NA, NA))
  expected <- data.frame(y = 1, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

  # n = 1, na.rm = FALSE
  result <- stat_median_ci(x = c(1, NA, NA, NA), na.rm = FALSE)
  expected <- data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

  # n = 10, na.rm = TRUE
  result <- stat_median_ci(x = c(1:10, NA, NA, NA))
  attr(result, "conf_level") <- round(attr(result, "conf_level"), digits = 2)
  expected <- data.frame(y = 5.5, ymin = 2L, ymax = 9L)
  attr(expected, "conf_level") <- 0.98
  expect_identical(result, expected)

  # n = 10, na.rm = FALSE
  result <- stat_median_ci(x = c(1:10, NA, NA, NA), na.rm = FALSE)
  expected <- data.frame(y = NA_integer_, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)

})


test_that("stat_median_ci works for named numeric values when name is missing)", {

  dta <- tibble(
    foo = factor(c("b", "b", "c", "c"), levels = c("a", "b", "c")),
    boo = setNames(3:6, c("b", "b", "c", "c"))
  )

  # empty data.frame
  dta_a <- dta %>% filter(foo == "a")

  result <- stat_median_ci(dta_a$boo)
  expected <- data.frame(y = NA_integer_, ymin = NA_real_, ymax = NA_real_)
  attr(expected, "conf_level") <- NA_real_
  expect_identical(result, expected)
})
