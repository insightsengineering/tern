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
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total patient-years at risk",
      "Number of adverse events observed", "AE rate per 100 patient-years",
      "95% CI", "A: Drug X", "(N=134)", "162.4", "78", "48.03",
      "(37.37, 58.69)", "B: Placebo", "(N=134)", "103.8", "104", "100.15",
      "(80.90, 119.40)", "C: Combination", "(N=132)", "172.6", "67", "38.82",
      "(29.53, 48.12)"
    ),
    .Dim = c(6L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
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
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total patient-years at risk",
      "Number of adverse events observed", "AE rate per 100 patient-years",
      "95% CI", "A: Drug X", "(N=134)", "162.4", "78", "48.03",
      "(37.97, 59.94)", "B: Placebo", "(N=134)", "103.8", "104", "100.15",
      "(81.83, 121.35)", "C: Combination", "(N=132)", "172.6", "67", "38.82",
      "(30.09, 49.30)"
    ),
    .Dim = c(6L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
