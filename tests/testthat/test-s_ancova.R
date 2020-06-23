context("test s_ancova and associated helper functions")

library(dplyr)

test_that("s_ancova_items works with healthy inputs", {
  testfun <- function(formula,
                      data) {
    s_ancova_items(
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
    model_frame = dplyr::rename(data, "arm(group)" = group)
  )
  expect_equal(result, expected, check.attributes = FALSE)
})

context("test s_ancova and associated helper functions")

library(dplyr)

test_that("s_ancova works with healthy inputs", {
  formula <- y ~ x + arm(group)
  set.seed(123)
  data <- data.frame(
    y = rnorm(10),
    x = rexp(10),
    group = factor(rep(1:2, 5))
  )
  result <- s_ancova(formula, data)
  expected <- list(
    sum_fit = data.frame(
      group = factor(c(1, 2)),
      emmean = c(0.02524, 0.1240),
      SE = c(0.4028, 0.4028),
      df = c(7, 7),
      lower.CL = c(-0.9273, -0.8286),
      upper.CL = c(0.9778, 1.0766)
    ),
    sum_contrasts = data.frame(
      contrast = factor("2 - 1"),
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
