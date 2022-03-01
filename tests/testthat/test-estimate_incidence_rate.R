testthat::test_that("control_incidence_rate works with customized parameters", {
  result <- control_incidence_rate(
    conf_level = 0.9,
    conf_type = "exact",
    time_unit_input = "month",
    time_unit_output = 100
  )
  expected <- list(
    conf_level = 0.9,
    conf_type = "exact",
    time_unit_input = "month",
    time_unit_output = 100
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("control_incidence_rate fails with wrong input", {
  testthat::expect_error(control_incidence_rate(conf_level = 1.1))
  testthat::expect_error(control_incidence_rate(conf_type = "wald"))
  testthat::expect_error(control_incidence_rate(time_unit_input = "decade"))
  testthat::expect_error(control_incidence_rate(time_unit_output = "one"))
})

testthat::test_that("h_incidence_rate_normal works as expected with healthy input", {
  result <- h_incidence_rate_normal(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(-0.001630872, 0.021630872)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("h_incidence_rate_normal_log works as expected with healthy input", {
  result <- h_incidence_rate_normal_log(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(0.003125199, 0.031997963)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("h_incidence_rate_exact works as expected with healthy input", {
  result <- h_incidence_rate_exact(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(0.001776808, 0.031478968)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("h_incidence_rate_byar works as expected with healthy input", {
  result <- h_incidence_rate_byar(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(0.002820411, 0.027609866)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("h_incidence_rate works as expected with healthy input", {
  result <- h_incidence_rate(
    200,
    2,
    control_incidence_rate(conf_level = 0.9, conf_type = "normal_log", time_unit_output = 100)
  )
  expected <- list(
    rate = 1,
    rate_ci = c(0.3125199, 3.1997963)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-4)
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
      time_unit_input = "month",
      time_unit_output = 100
    )
  )

  expected <- list(
    person_years = with_label(9.058333, "Total patient-years at risk"),
    n_events = with_label(4, "Number of adverse events observed"),
    rate = with_label(44.15823, "AE rate per 100 patient-years"),
    rate_ci = with_label(c(19.40154, 100.50487), "90% CI")
  )

  testthat::expect_equal(result, expected, tolerance = 1e-4, check.attributes = TRUE)
})

testthat::test_that("estimate_incidence_rate works as expected with healthy input", {
  skip_if_fail_rtables_refactor()

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
        time_unit_input = "month",
        time_unit_output = 100
      )
    ) %>%
    build_table(df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total patient-years at risk", "Number of adverse events observed",
      "AE rate per 100 patient-years", "90% CI", "A", "(N=3)", "3.8",
      "1", "26.2", "(5.06, 135.73)", "B", "(N=3)", "5.2", "3", "57.23",
      "(22.14, 147.94)"
    ),
    .Dim = c(6L, 3L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
