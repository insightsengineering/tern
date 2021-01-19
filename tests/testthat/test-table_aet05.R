# Test variants for AET05.

library(random.cdisc.data)
library(dplyr)

test_that("AET05 variant 1 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  anl <- radaette(cached = TRUE) %>%
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
    c("", "", "Total patient-years at risk", "Number of adverse events observed",
      "AE rate per 100 patient-years", "95% CI", "A: Drug X", "(N=134)",
      "25.8", "80", "309.57", "(241.74, 377.41)", "B: Placebo", "(N=134)",
      "36.4", "106", "291.47", "(235.98, 346.96)", "C: Combination",
      "(N=132)", "21.9", "81", "369.16", "(288.77, 449.55)"),
    .Dim = c(6L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("AET05 variant 2 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  anl <- radaette(cached = TRUE) %>%
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
    c("", "", "Total patient-years at risk", "Number of adverse events observed",
      "AE rate per 100 patient-years", "95% CI", "A: Drug X", "(N=134)",
      "25.8", "80", "309.57", "(245.47, 385.29)", "B: Placebo", "(N=134)",
      "36.4", "106", "291.47", "(238.63, 352.52)", "C: Combination",
      "(N=132)", "21.9", "81", "369.16", "(293.17, 458.83)"),
    .Dim = c(6L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
