set.seed(2)
rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
grp <- factor(c(rep("A", 10), rep("B", 10)))
tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))

methods <- c("ac", "wald", "waldcc", "score", "scorecc", "mn", "mee", "blj", "ha", "hal", "jp")
methodsci <- c(
  "wilson", "wald", "waldcc", "agresti-coull", "jeffreys", "modified wilson", "wilsoncc", "modified jeffreys",
  "clopper-pearson", "arcsine", "logit", "witting", "pratt", "midp", "lik", "blaker"
)

testthat::test_that("desctools_binom produces correct output", {
  result <- desctools_binom(
    tbl[1], sum(tbl[1], tbl[3]), tbl[2], sum(tbl[2], tbl[4])
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("desctools_binom produces correct output for all methods", {
  result <- sapply(
    methods,
    function(x) {
      desctools_binom(
        tbl[1], sum(tbl[1], tbl[3]), tbl[2], sum(tbl[2], tbl[4]),
        conf.level = 0.90, method = x
      )
    }
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("desctools_binomci produces correct output with default settings", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
  result <- desctools_binomci(x = sum(rsp), n = 20)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("desctools_binomci produces correct output with custom settings", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
  result <- desctools_binomci(x = sum(rsp), n = 20, conf.level = 0.90, sides = "left", method = "lik")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("desctools_binomci produces correct output for all methods", {
  result <- sapply(
    methodsci,
    function(x) {
      desctools_binomci(x = sum(rsp), n = 20, conf.level = 0.90, method = x)
    }
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
