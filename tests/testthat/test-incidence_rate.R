df <- data.frame(
  USUBJID = as.character(seq(6)),
  CNSR = c(0, 1, 1, 0, 0, 0),
  AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
  ARM = factor(c("A", "A", "A", "B", "B", "B")),
  STRATA1 = factor(c("X", "Y", "Y", "X", "X", "Y"))
) %>%
  dplyr::mutate(n_events = 1 - CNSR)

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

testthat::test_that("s_incidence_rate works as expected with healthy input", {
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

testthat::test_that("a_incidence_rate works with default arguments", {
  result <- a_incidence_rate(
    df,
    .df_row = df,
    .var = "AVAL",
    n_events = "n_events"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_incidence_rate works with customized arguments", {
  result <- a_incidence_rate(
    df,
    .var = "AVAL",
    n_events = "n_events",
    control = control_incidence_rate(
      conf_level = 0.9,
      conf_type = "normal_log",
      input_time_unit = "month",
      num_pt_year = 100
    ),
    .df_row = df,
    .stats = c("n_rate", "n_unique"),
    .formats = c(n_rate = "xx.xx (xx.xx)", n_unique = "xx.xx"),
    .labels = c(n_rate = "Total number of applicable adverse events (rate)"),
    .indent_mods = c(n_rate = 3L)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_incidence_rate works as expected with default input", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_incidence_rate works as expected with custom input", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(
        conf_level = 0.9,
        conf_type = "normal_log",
        input_time_unit = "month",
        num_pt_year = 100
      ),
      .stats = c("n_rate", "n_unique"),
      .formats = c(n_rate = "xx.xx (xx.xx)", n_unique = "xx.xx"),
      .labels = c(n_rate = "Total number of applicable adverse events (rate)"),
      .indent_mods = c(n_rate = 3L)
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_incidence_rate works with default arguments with summarize = TRUE", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      summarize = TRUE
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_incidence_rate works with custom arguments with summarize = TRUE", {
  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      .stats = c("person_years", "n_events", "rate")
    ) %>%
    split_rows_by("STRATA1", child_labels = "visible") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      .stats = c("n_unique", "n_rate"),
      summarize = TRUE,
      label_fmt = "%.labels"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
