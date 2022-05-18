# Preparation of the test case.
library(scda)
library(dplyr)
adpp <- synthetic_cdisc_data("rcd_2022_02_28")$adpp
adpp <- adpp %>% filter(PPSPEC == "Plasma", AVISIT == "CYCLE 1 DAY 1", PARAMCD
                        %in% c("AUCIFO", "CMAX"))

testthat::test_that("PKPT02 is produced correctly", {
  l <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      split_fun = trim_levels_in_group("ARMCD"),
      # label_pos = "topleft",
      split_label = "Treatment Arm"
    ) %>%
    split_rows_by(
      var = "PARAM",
      label_pos = "topleft",
      split_label = "PK Parameter"
    ) %>%
    tern::summarize_vars(
      vars = "AVAL",
      .stats = c("n", "mean_sd", "cv", "geom_mean", "geom_cv", "median", "range"),
      .formats = c(
        n = "xx.",
        mean_sd = sprintf_format("%.3e (%.3e)"),
        cv = "xx.x",
        geom_mean = sprintf_format("%.3e"),
        geom_cv = "xx.x",
        median = sprintf_format("%.3e"),
        range = sprintf_format("%.3e - %.3e")
      )
    )

  result <- build_table(l, df = adpp)

  result_matrix <- to_string_matrix(result)
  result_matrix <- result_matrix[1:17, 1:3]
  expected_matrix <- structure(
    c(
      "PK Parameter", "AUC Infinity Obs", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max",
      "Max Conc", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "ARM A", "", "134",
      "1.974e+02 (4.021e+01)", "20.4", "1.929e+02", "23.1", "1.964e+02",
      "7.070e+01 - 2.807e+02", "", "134", "3.121e+01 (6.094e+00)", "19.5",
      "3.057e+01", "21.1", "3.169e+01", "1.609e+01 - 4.591e+01", "ARM C", "",
      "264", "2.015e+02 (3.959e+01)", "19.6", "1.974e+02", "21.3", "2.003e+02",
      "7.760e+01 - 2.988e+02", "", "264", "2.975e+01 (5.994e+00)", "20.1", "2.911e+01",
      "21.6", "2.996e+01", "1.480e+01 - 4.430e+01"
    ),
    .Dim = c(17L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
