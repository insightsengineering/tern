testthat::test_that("control_incidence_rate works with customized parameters", {
  result <- control_incidence_rate(
    conf_level = 0.9,
    conf_type = "exact",
    input_time_unit = "month",
    num_pt_year = 100
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_incidence_rate fails with wrong input", {
  testthat::expect_error(control_incidence_rate(conf_level = 1.1))
  testthat::expect_error(control_incidence_rate(conf_type = "wald"))
  testthat::expect_error(control_incidence_rate(input_time_unit = "decade"))
  testthat::expect_error(control_incidence_rate(num_pt_year = "one"))
})

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

testthat::test_that("s_incidence_rate works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(seq(6)),
    CNSR = c(0, 1, 1, 0, 0, 0),
    AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
    ARM = factor(c("A", "A", "A", "B", "B", "B"))
  ) %>%
    dplyr::mutate(is_event = CNSR == 0) %>%
    dplyr::mutate(n_events = as.integer(is_event))
  result <- s_incidence_rate(
    df,
    .var = "AVAL",
    n_events = "n_events",
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "normal_log",
      input_time_unit = "month",
      num_pt_year = 100
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_incidence_rate works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(seq(6)),
    CNSR = c(0, 1, 1, 0, 0, 0),
    AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
    ARM = factor(c("A", "A", "A", "B", "B", "B"))
  ) %>%
    dplyr::mutate(is_event = CNSR == 0) %>%
    dplyr::mutate(n_events = as.integer(is_event))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(
        conf_level = 0.9,
        conf_type = "normal_log",
        input_time_unit = "month",
        num_pt_year = 100
      )
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
