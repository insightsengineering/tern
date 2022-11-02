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
