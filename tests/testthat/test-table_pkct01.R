# Preparation of the test case.
library(rtables)
library(dplyr)

adpc <- scda::synthetic_cdisc_data("latest")$adpc

adpc <- adpc %>%
  dplyr::mutate(
    NRELTM1 = as.factor(NRELTM1),
    AVALC = as.factor(AVALC)
  ) %>%
  dplyr::filter(ACTARM %in% c("A: Drug X")) %>%
  dplyr::mutate(ACTARM = factor(ACTARM, levels = c("A: Drug X")))

adpc_1 <- adpc %>%
  dplyr::mutate(
    NRELTM1 = as.factor(NRELTM1),
    AVAL = AVALC
  ) %>%
  dplyr::filter(ACTARM %in% c("A: Drug X")) %>%
  dplyr::mutate(ACTARM = factor(ACTARM, levels = c("A: Drug X")))

testthat::test_that("PKCT01 is produced correctly", {
  l_rows <- basic_table() %>%
    split_rows_by(
      var = "ACTARM",
      split_label = "Cohort/Treatment",
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      var = "VISIT",
      split_label = "Visit",
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      var = "NRELTM1",
      split_label = "Norminal Time from First Dose",
      label_pos = "topleft"
    )

  l1 <- l_rows %>%
    summarize_vars_in_cols(
      var = "AVAL",
      col_split = TRUE,
      .stats = c("n", "mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
      .formats = c(
        n = "xx.",
        mean = sprintf_format("%.3e"),
        sd = sprintf_format("%.3e"),
        cv = "xx.x",
        median = sprintf_format("%.3e"),
        geom_mean = sprintf_format("%.3e"),
        geom_cv = "xx.x",
        min = sprintf_format("%.3e"),
        max = sprintf_format("%.3e")
      ),
      .labels = c(
        n = "n",
        mean = "Mean",
        sd = "SD",
        cv = "CV (%)",
        geom_mean = "Geometric Mean",
        geom_cv = "CV % Geometric Mean",
        median = "Median",
        min = "Minimum",
        max = "Maximum"
      )
    )


  l2 <- l_rows %>%
    summarize_vars_in_cols(
      var = "AVAL",
      var_type = "character",
      col_split = TRUE,
      .stats = c("n_blq"),
      .labels = c(
        n_blq = "Number\nof\n<LTR/BLQ>s"
      )
    )


  result1 <- build_table(l1, df = adpc)
  result2 <- build_table(l2, df = adpc_1)
  result <- cbind_rtables(result1[, 1], result2, result1[, 2:ncol(result1)]) %>% trim_rows()
  main_title(result) <- "Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable\n Protocol: xxxxx"
  subtitles(result) <- paste("Analyte: ", unique(unique(adpc$PARAM)), "Treatment:", unique(unique(adpc$ACTARM)))

  result_matrix <- to_string_matrix(result)
  expected_matrix <- c(
    "Cohort/Treatment", "", "", "", "", "", "", "", "", "", "",
    "  Visit", "", "", "", "", "", "", "", "", "", "",
    "    Norminal Time from First Dose", "", "Number", "", "", "", "", "", "", "", "",
    "", "", "of", "", "", "", "", "", "", "", "",
    "", "n", "<LTR/BLQ>s", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "A: Drug X", "", "", "", "", "", "", "", "", "", "",
    "Day 1", "", "", "", "", "", "", "", "", "", "",
    "0", "134", "134", "0.000e+00", "0.000e+00", "NA", "NA", "NA", "0.000e+00", "0.000e+00", "0.000e+00",
    "0.5", "134", "0", "1.261e+01", "1.509e+00", "12.0", "1.252e+01", "12.2", "1.263e+01", "9.716e+00", "1.557e+01",
    "1", "134", "0", "1.618e+01", "1.626e+00", "10.0", "1.610e+01", "10.1", "1.619e+01", "1.260e+01", "1.986e+01",
    "1.5", "134", "0", "1.562e+01", "1.456e+00", "9.3", "1.556e+01", "9.3", "1.546e+01", "1.233e+01", "1.902e+01",
    "2", "134", "0", "1.344e+01", "1.353e+00", "10.1", "1.338e+01", "10.0", "1.330e+01", "1.075e+01", "1.646e+01",
    "3", "134", "0", "8.468e+00", "1.246e+00", "14.7", "8.376e+00", "15.0", "8.395e+00", "5.880e+00", "1.095e+01",
    "4", "134", "0", "4.794e+00", "1.017e+00", "21.2", "4.685e+00", "22.0", "4.785e+00", "2.695e+00", "7.094e+00",
    "8", "134", "0", "3.478e-01", "1.798e-01", "51.7", "3.032e-01", "58.4", "3.185e-01", "7.600e-02", "8.660e-01",
    "12", "134", "0", "2.239e-02", "1.895e-02", "84.6", "1.556e-02", "111.6", "1.700e-02", "2.000e-03", "8.300e-02",
    "24", "134", "134", "0.000e+00", "0.000e+00", "NA", "NA", "NA", "0.000e+00", "0.000e+00", "0.000e+00",
    "Day 2", "", "", "", "", "", "", "", "", "", "",
    "48", "134", "134", "0.000e+00", "0.000e+00", "NA", "NA", "NA", "0.000e+00", "0.000e+00", "0.000e+00"
  )
  expected_matrix <- matrix(expected_matrix, nrow = 19, ncol = 11, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})
