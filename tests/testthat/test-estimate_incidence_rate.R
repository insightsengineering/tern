test_that("control_incidence_rate works with customized parameters", {
  result <- control_incidence_rate(
    conf_level = 0.9, conf_type = "exact", time_unit = 100
  )
  expected <- list(
    conf_level = 0.9,
    conf_type = "exact",
    time_unit = 100
  )
  expect_identical(result, expected)
})

test_that("control_incidence_rate fails with wrong input", {
  expect_error(control_incidence_rate(conf_level = 1.1))
  expect_error(control_incidence_rate(conf_type = "wald"))
  expect_error(control_incidence_rate(time_unit = "one"))
})

test_that("h_incidence_rate_normal works as expected with healthy input", {
  result <- h_incidence_rate_normal(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(-0.001630872, 0.021630872)
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_incidence_rate_normal_log works as expected with healthy input", {
  result <- h_incidence_rate_normal_log(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(0.003125199, 0.031997963)
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_incidence_rate_exact works as expected with healthy input", {
  result <- h_incidence_rate_exact(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(0.001776808, 0.031478968)
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_incidence_rate_byar works as expected with healthy input", {
  result <- h_incidence_rate_byar(200, 2, 0.1)
  expected <- list(
    rate = 0.01,
    rate_ci = c(0.002820411, 0.027609866)
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("h_incidence_rate works as expected with healthy input", {
  result <- h_incidence_rate(
    200,
    2,
    control_incidence_rate(conf_level = 0.9, conf_type = "normal_log", time_unit = 100))
  expected <- list(
    rate = 1,
    rate_ci = c(0.3125199, 3.1997963)
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("s_incidence_rate works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(seq(6)),
    CNSR = c(0, 1, 1, 0, 0, 0),
    AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
    ARM = factor(c("A", "A", "A", "B", "B", "B"))
  ) %>%
    mutate(is_event = CNSR == 0)
  result <- s_incidence_rate(
    df,
    .var = "AVAL",
    is_event = "is_event",
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "normal_log",
      time_unit = 100
    )
  )

  expected <- list(
    person_years = with_label(108.7, "Total patient-years at risk"),
    num_events = with_label(4, "Number of adverse events observed"),
    rate = with_label(3.679853, "AE rate per 100 patient-years"),
    rate_ci = with_label(c(1.616795, 8.375406), "90% CI")
  )

  expect_equal(result, expected, tolerance = 1e-4, check.attributes = TRUE)
})

test_that("estimate_incidence_rate works as expected with healthy input", {
  df <- data.frame(
    USUBJID = as.character(seq(6)),
    CNSR = c(0, 1, 1, 0, 0, 0),
    AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
    ARM = factor(c("A", "A", "A", "B", "B", "B"))
  ) %>%
    mutate(is_event = CNSR == 0)
  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    estimate_incidence_rate(
      vars = "AVAL",
      is_event = "is_event",
      control = control_incidence_rate(
        conf_level = 0.9,
        conf_type = "normal_log",
        time_unit = 100
      )
    ) %>%
    build_table(df, col_counts = table(df$ARM))
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("", "", "Total patient-years at risk", "Number of adverse events observed",
      "AE rate per 100 patient-years", "90% CI", "A", "(N=3)", "45.8",
      "1", "2.18", "(0.42, 11.31)", "B", "(N=3)", "62.9", "3", "4.77",
      "(1.85, 12.33)"),
    .Dim = c(6L, 3L)
  )

  expect_identical(result_matrix, expected_matrix)
})
