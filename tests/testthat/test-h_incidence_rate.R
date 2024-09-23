testthat::test_that("h_incidence_rate_normal works as expected with healthy input", {
  result <- h_incidence_rate_normal(200, 2, 0.1)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_incidence_rate_normal_log works as expected with healthy input", {
  result <- h_incidence_rate_normal_log(200, 2, 0.1)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_incidence_rate_exact works as expected with healthy input", {
  result <- h_incidence_rate_exact(200, 2, 0.1)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_incidence_rate_byar works as expected with healthy input", {
  result <- h_incidence_rate_byar(200, 2, 0.1)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_incidence_rate works as expected with healthy input", {
  result <- h_incidence_rate(
    200,
    2,
    control_incidence_rate(conf_level = 0.9, conf_type = "normal_log", num_pt_year = 100)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
