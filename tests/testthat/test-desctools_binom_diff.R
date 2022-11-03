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
  expected <- c(est = 0, lwr.ci = -0.400076, upr.ci = 0.400076)
  testthat::expect_equal(result[1, ], expected, tolerance = 1e-4)
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
  expected <- matrix(
    c(
      c(0, -0.3357543, 0.3357543), c(0, -0.3678005, 0.3678005),
      c(0, -0.4678005, 0.4678005), c(0, -0.3262989, 0.3262989),
      c(0, -0.3806657, 0.3806657), c(0, -0.3530546, 0.3530546),
      c(0, -0.3451998, 0.3451998), c(0, -0.3678005, 0.3678005),
      c(0, -0.4376957, 0.4376957), c(0, -0.3451925, 0.3451925),
      c(0, -0.3451925, 0.3451925)
    ),
    nrow = 3,
    dimnames = list(NULL, methods)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("desctools_binomci produces correct output with default settings", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)

  result <- desctools_binomci(x = sum(rsp), n = 20)
  expected <- c(est = 0.5, lwr.ci = 0.299298, upr.ci = 0.700702)
  testthat::expect_equal(result[1, ], expected, tolerance = 1e-4)
})

testthat::test_that("desctools_binomci produces correct output with custom settings", {
  set.seed(2)
  rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)

  result <- desctools_binomci(x = sum(rsp), n = 20, conf.level = 0.90, sides = "left", method = "lik")
  expected <- c(est = 0.5, lwr.ci = 0.3596101, upr.ci = 1)
  testthat::expect_equal(result[1, ], expected, tolerance = 1e-4)
})

testthat::test_that("desctools_binomci produces correct output for all methods", {
  result <- sapply(
    methodsci,
    function(x) {
      desctools_binomci(x = sum(rsp), n = 20, conf.level = 0.90, method = x)
    }
  )
  expected <- matrix(
    c(
      c(0.5, 0.3274038, 0.6725962), c(0.5, 0.3160998, 0.6839002),
      c(0.5, 0.2910998, 0.7089002), c(0.5, 0.3274038, 0.6725962),
      c(0.5, 0.3242344, 0.6757656), c(0.5, 0.3274038, 0.6725962),
      c(0.5, 0.3055729, 0.6944271), c(0.5, 0.3242344, 0.6757656),
      c(0.5, 0.3019539, 0.6980461), c(0.5, 0.3202181, 0.6797819),
      c(0.5, 0.3239669, 0.6760331), c(0.5, 0.3488144, 0.6316294),
      c(0.5, 0.3347123, 0.6980960), c(0.5, 0.3200831, 0.6799169),
      c(0.5, 0.3221477, 0.6778523), c(0.5, 0.3217139, 0.6782861)
    ),
    nrow = 3,
    dimnames = list(NULL, methodsci)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})
