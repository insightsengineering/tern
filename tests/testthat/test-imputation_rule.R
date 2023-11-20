set.seed(1)
df <- data.frame(
  ARM = with_label(rep("A: Drug X", 162), "Arm"),
  AVAL = runif(162, 0, 100),
  AVALCAT1 = as.factor(sample(c(1, "BLQ"), 162, replace = TRUE)),
  VISIT = with_label(as.factor(rep(c(rep("Day 1", 5), rep("Day 2", 4)), 18)), "Visit"),
  NFRLT = with_label(as.factor(rep(c(0, seq(0, 42, 6)), 18)), "Nominal Time")
)

testthat::test_that("imputation_rule works correctly for 1/3 imputation rule", {
  x_stats <- s_summary(df$AVAL[df$NFRLT == 0])
  result <- imputation_rule(df, x_stats, "max", "1/3")
  res <- testthat::expect_snapshot(result)

  x_stats <- s_summary(df$AVAL[df$NFRLT == 0])
  result <- imputation_rule(df, x_stats, "mean", "1/3")
  res <- testthat::expect_snapshot(result)

  x_stats <- s_summary(df$AVAL[df$NFRLT == 6])
  result <- imputation_rule(df, x_stats, "geom_mean", "1/3", post = TRUE)
  res <- testthat::expect_snapshot(result)

  x_stats <- s_summary(df$AVAL[df$NFRLT == 18])
  result <- imputation_rule(df, x_stats, "max", "1/3", post = TRUE)
  res <- testthat::expect_snapshot(result)
})

testthat::test_that("imputation_rule works correctly for 1/2 imputation rule", {
  x_stats <- s_summary(df$AVAL[df$NFRLT == 0])
  result <- imputation_rule(df, x_stats, "max", "1/2")
  res <- testthat::expect_snapshot(result)

  x_stats <- s_summary(df$AVAL[df$NFRLT == 0])
  result <- imputation_rule(df, x_stats, "mean", "1/2")
  res <- testthat::expect_snapshot(result)

  x_stats <- s_summary(df$AVAL[df$NFRLT == 6])
  result <- imputation_rule(df, x_stats, "geom_mean", "1/2", post = TRUE)
  res <- testthat::expect_snapshot(result)

  x_stats <- s_summary(df$AVAL[df$NFRLT == 18])
  result <- imputation_rule(df, x_stats, "max", "1/2", post = TRUE)
  res <- testthat::expect_snapshot(result)
})
