# Test variants for AET05.
adsl <- adsl_raw
adaette <- adaette_raw

testthat::test_that("AET05 variant 1 is produced correctly", {
  anl <- adaette %>%
    dplyr::filter(PARAM == "Time to first occurrence of any adverse event") %>%
    dplyr::mutate(is_event = CNSR == 0) %>%
    dplyr::mutate(n_events = as.integer(is_event))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(time_unit_output = 100)
    ) %>%
    build_table(anl, alt_counts_df = adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("AET05 variant 2 is produced correctly", {
  anl <- adaette %>%
    dplyr::filter(PARAM == "Time to first occurrence of any adverse event") %>%
    dplyr::mutate(is_event = CNSR == 0) %>%
    dplyr::mutate(n_events = as.integer(is_event))

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(conf_type = "exact", time_unit_output = 100)
    ) %>%
    build_table(anl, alt_counts_df = adsl)

  res <- expect_silent(result)
  expect_snapshot(res)
})
