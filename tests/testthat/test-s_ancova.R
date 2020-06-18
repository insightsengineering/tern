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
