library(dplyr)

test_that("s_ancova_items works with healthy inputs", {
  testfun <- function(formula, data) {
    tern:::s_ancova_items(
      formula = formula,
      cl = match.call(),
      data = data,
      env = parent.frame())
  }
  formula <- y ~ x + arm(group)
  data <- data.frame(
    y = rnorm(10),
    x = rexp(10),
    group = factor(rep(1:2, 5))
  )
  result <- testfun(formula, data)
  expected <- list(
    rsp = data$y,
    rsp_name = "y",
    arm = data$group,
    arm_name = "group",
    formula = formula,
    model_frame = data
  )
  expect_equal(result, expected, check.attributes = FALSE)
})

test_that("s_ancova_items by default discards missing data, but respects `na.action` attribute of `data`", {
  testfun <- function(formula, data) {
    tern:::s_ancova_items(
      formula = formula,
      cl = match.call(),
      data = data,
      env = parent.frame())
  }
  formula <- y ~ x + arm(group)
  data <- data.frame(
    y = rnorm(10),
    x = rexp(10),
    group = factor(rep(1:2, 5))
  )
  # Insert missing values.
  data$y[1] <- NA
  data$x[2] <- NA
  data$group[3] <- NA
  # We expect that the call works and only complete rows are included in the result.
  result <- testfun(formula, data)
  data_complete <- na.omit(data)
  expected <- list(
    rsp = data_complete$y,
    rsp_name = "y",
    arm = data_complete$group,
    arm_name = "group",
    formula = formula,
    model_frame = data_complete
  )
  expect_equal(result, expected, check.attributes = FALSE)
  # If we set the `na.action` attribute of `data` to `na.fail`, then the call fails instead.
  attr(data, "na.action") <- na.fail
  expect_error(testfun(formula, data), "missing values in object")
})


# package version dependent sometimes returns factor or character
match_char_fct <- function(x) {
  if (is.factor(x)) {
    function(x) factor(x)
  } else {
    function(x) x
  }
}


test_that("s_ancova works with healthy inputs", {
  formula <- y ~ x + arm(group)
  set.seed(123)
  data <- data.frame(
    y = rnorm(10),
    x = rexp(10),
    group = factor(rep(1:2, 5))
  )
  result <- s_ancova(formula, data)

  f_coerce <- match_char_fct(result$sum_contrasts$contrast)

  expected <- list(
    sum_fit = data.frame(
      group = factor(c(1, 2)),
      emmean = c(0.02524, 0.1240),
      SE = c(0.4028, 0.4028),
      df = c(7, 7),
      lower.CL = c(-0.9273, -0.8286),
      upper.CL = c(0.9778, 1.0766),
      n_total = c(5, 5),
      n_complete = c(5, 5)
    ),
    sum_contrasts = data.frame(
      contrast = f_coerce("2 - 1"),
      estimate = 0.09876,
      SE = 0.582,
      df = 7,
      lower.CL = -1.277,
      upper.CL = 1.475,
      t.ratio = 0.1697,
      p.value = 0.870
    )
  )
  expect_equal(result, expected, tol = 0.001, check.attributes = FALSE)
})

test_that("s_ancova works with missing values in `data`", {
  formula <- y ~ x + arm(group)
  set.seed(123)
  data <- data.frame(
    y = rnorm(10),
    x = rexp(10),
    group = factor(rep(1:2, 5))
  )
  # Insert missing values.
  data$y[1] <- NA
  data$x[2] <- NA
  data$group[3] <- NA
  result <- s_ancova(formula, data)
  f_coerce <- match_char_fct(result$sum_contrasts$contrast)

  expected <- list(
    sum_fit = data.frame(
      group = factor(c(1, 2)),
      emmean = c(-0.1551, 0.2181),
      SE = c(0.4874, 0.4310),
      df = c(4, 4),
      lower.CL = c(-1.5085, -0.9785),
      upper.CL = c(1.198, 1.415),
      n_total = c(4, 5),
      n_complete = c(3, 4)
    ),
    sum_contrasts = data.frame(
      contrast = f_coerce("2 - 1"),
      estimate = 0.3732,
      SE = 0.6606,
      df = 4,
      lower.CL = -1.461,
      upper.CL = 2.207,
      t.ratio = 0.565,
      p.value = 0.6023
    )
  )
  expect_equal(result, expected, tol = 0.001, check.attributes = FALSE)
})
