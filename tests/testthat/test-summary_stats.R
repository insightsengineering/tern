testthat::test_that("summary_formats works as expected", {
  result <- summary_formats()
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- summary_formats(type = "counts", include_pval = TRUE)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summary_labels works as expected", {
  result <- summary_labels()
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- summary_labels(type = "counts", include_pval = TRUE)
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summary_custom works as expected", {
  result <- summary_custom()
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  result <- summary_custom(
    type = "counts", stats_custom = c("n", "count"),
    formats_custom = c(n = "xx.xx"), labels_custom = c(count = "#"), indent_mods_custom = 2L
  )
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_analyze_vars works with customized parameters", {
  result <- control_analyze_vars(
    conf_level = 0.9,
    quantiles = c(0.1, 0.9)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_analyze_vars fails wrong inputs", {
  testthat::expect_error(control_analyze_vars(quantiles = c(25, 75)))
  testthat::expect_error(control_analyze_vars(conf_level = 95))
})
