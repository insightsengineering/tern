# Preparation of the test case.
library(scda)
library(rtables)
adpp <- synthetic_cdisc_data("latest")$adpp
adpp <- adpp %>%
  filter(PPSPEC == "Urine", AVISIT == "CYCLE 1 DAY 1")

testthat::test_that("PKPT04 is produced correctly for Drug X", {
  adpp0 <- adpp %>%
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  l <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      split_fun = trim_levels_in_group("ARMCD"),
      # label_pos = "topleft",
      split_label = "Treatment Arm"
    ) %>%
    split_rows_by(
      var = "PKPARAM",
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

  result <- build_table(l, df = adpp0)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "CLR (L/hr)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max",
      "RENALCLD (L/hr/mg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "ARM A", "", "134",
      "5.089e-02 (9.769e-03)", "19.2", "4.992e-02", "20.3", "5.020e-02",
      "2.384e-02 - 8.111e-02", "", "134", "5.076e-03 (1.027e-03)", "20.2",
      "4.971e-03", "21.0", "5.065e-03", "2.896e-03 - 7.598e-03", "ARM C", "",
      "132", "4.993e-02 (9.577e-03)", "19.2", "4.895e-02", "20.6", "5.003e-02",
      "2.690e-02 - 7.318e-02", "", "132", "4.893e-03 (9.184e-04)", "18.8",
      "4.800e-03", "20.6", "4.945e-03", "2.177e-03 - 6.667e-03"
    ),
    .Dim = c(17L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("PKPT04 is produced correctly for Drug Y", {
  adpp1 <- adpp %>%
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  l <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      split_fun = trim_levels_in_group("ARMCD"),
      # label_pos = "topleft",
      split_label = "Treatment Arm"
    ) %>%
    split_rows_by(
      var = "PKPARAM",
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

  result <- build_table(l, df = adpp1)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "CLR (L/hr)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max",
      "RENALCLD (L/hr/mg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "ARM C", "", "132",
      "4.975e-02 (8.945e-03)", "18.0", "4.891e-02", "19.2", "4.956e-02",
      "2.242e-02 - 7.184e-02", "", "132", "5.068e-03 (1.038e-03)", "20.5",
      "4.951e-03", "22.7", "5.209e-03", "2.453e-03 - 7.425e-03"
    ),
    .Dim = c(17L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
