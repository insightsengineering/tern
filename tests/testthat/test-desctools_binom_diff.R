testthat::test_that("desctools_binom produces correct output", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
  grp <- factor(c(rep("A", 10), rep("B", 10)))
  tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))

  result <- desctools_binom(
    tbl[1], sum(tbl[1], tbl[3]), tbl[2], sum(tbl[2], tbl[4]),
    conf.level = 0.90, method = "waldcc"
  )
  expected <- c(est = 0, lwr.ci = -0.4678005, upr.ci = 0.4678005)
  testthat::expect_equal(result[1, ], expected, tolerance = 1e-4)
})

testthat::test_that("desctools_binomci produces correct output with default settings", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)

  result <- desctools_binomci(x = sum(rsp), n = 20)
  expected <- c(est = 0.5, lwr.ci = 0.3274038, upr.ci = 0.6725962)
  testthat::expect_equal(result[1, ], expected, tolerance = 1e-4)
})

testthat::test_that("desctools_binomci produces correct output with custom settings", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)

  result <- desctools_binomci(x = sum(rsp), n = 20, conf.level = 0.90, sides = "left", method = "lik")
  expected <- c(est = 0.5, lwr.ci = 0.3596101, upr.ci = 1)
  testthat::expect_equal(result[1, ], expected, tolerance = 1e-4)
})
