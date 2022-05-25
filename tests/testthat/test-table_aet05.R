# Test variants for AET05.

library(scda)
library(dplyr)
library(rtables)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adaette <- synthetic_cdisc_data("rcd_2022_02_28")$adaette

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
      "95% CI", "A: Drug X", "(N=134)", "112.3", "78", "69.49",
      "(54.07, 84.91)", "B: Placebo", "(N=134)", "77.4", "104", "134.37",
      "(108.54, 160.19)", "C: Combination", "(N=132)", "119.4", "67", "56.10",
      "(42.67, 69.53)"
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
      "95% CI", "A: Drug X", "(N=134)", "112.3", "78", "69.49",
      "(54.93, 86.72)", "B: Placebo", "(N=134)", "77.4", "104", "134.37",
      "(109.79, 162.81)", "C: Combination", "(N=132)", "119.4", "67", "56.10",
      "(43.48, 71.25)"
    ),
    .Dim = c(6L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
