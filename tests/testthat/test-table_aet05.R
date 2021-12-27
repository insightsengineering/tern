# Test variants for AET05.

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
adaette <- synthetic_cdisc_data("rcd_2021_05_05")$adaette

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
      "", "", "Total patient-years at risk", "Number of adverse events observed",
      "AE rate per 100 patient-years", "95% CI", "A: Drug X", "(N=134)",
      "93.1", "88", "94.57", "(74.81, 114.32)", "B: Placebo", "(N=134)",
      "65.8", "109", "165.72", "(134.61, 196.83)", "C: Combination",
      "(N=132)", "105.6", "76", "72", "(55.81, 88.18)"
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
      "", "", "Total patient-years at risk", "Number of adverse events observed",
      "AE rate per 100 patient-years", "95% CI", "A: Drug X", "(N=134)",
      "93.1", "88", "94.57", "(75.84, 116.51)", "B: Placebo", "(N=134)",
      "65.8", "109", "165.72", "(136.07, 199.9)", "C: Combination",
      "(N=132)", "105.6", "76", "72", "(56.72, 90.11)"
    ),
    .Dim = c(6L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
