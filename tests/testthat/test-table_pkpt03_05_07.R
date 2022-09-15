# Data generation
adpp <- adpp_raw
adpp_plasma <- adpp %>% dplyr::filter(PPSPEC == "Plasma", AVISIT == "CYCLE 1 DAY 1")
adpp_urine <- adpp %>% dplyr::filter(PPSPEC == "Urine", AVISIT == "CYCLE 1 DAY 1")
adpp_norm_dose <- adpp %>% dplyr::filter(
  AVISIT == "CYCLE 1 DAY 1",
  stringr::str_detect(
    tolower(PARAM),
    stringr::regex("norm by dose", ignore_case = TRUE)
  )
)

# Define template layout
l <- basic_table() %>%
  split_rows_by(
    var = "ARMCD",
    split_fun = trim_levels_in_group("ARMCD"),
    label_pos = "topleft",
    split_label = "Treatment Arm"
  ) %>%
  split_rows_by(
    var = "PKPARAM",
    label_pos = "topleft",
    split_label = "PK Parameter"
  ) %>%
  summarize_vars_in_cols(
    var = "AVAL",
    col_split = TRUE,
    .stats = c(
      "n", "mean", "sd", "cv",
      "geom_mean", "geom_cv", "median",
      "min", "max"
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
    ),
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
    )
  )

# PKPT03
testthat::test_that("PKPT03 Drug X is produced correctly", {

  # Plasma Drug x
  adpp0 <- adpp_plasma %>%
    dplyr::filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))


  result_matrix <- to_string_matrix(result)
  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean",
    "Median", "Minimum", "Maximum", "ARM A", "", "", "", "", "", "", "", "", "",
    "AUCinf obs (day*ug/mL)", "134", "2.042e+02", "3.925e+01", "19.2", "2.002e+02", "20.6",
    "2.072e+02", "1.095e+02", "3.017e+02", "CL obs (ml/day/kg)", "134", "5.025e+00",
    "9.394e-01", "18.7", "4.934e+00", "19.8", "5.082e+00", "3.144e+00", "7.218e+00",
    "Cmax (ug/mL)", "134", "2.975e+01", "5.758e+00", "19.4", "2.918e+01", "20.1",
    "2.941e+01", "1.561e+01", "4.600e+01", "ARM C", "", "", "", "", "", "", "", "", "",
    "AUCinf obs (day*ug/mL)", "132", "1.999e+02", "4.169e+01", "20.9", "1.950e+02",
    "23.6", "2.050e+02", "7.412e+01", "2.778e+02", "CL obs (ml/day/kg)", "132",
    "5.221e+00", "1.015e+00", "19.4", "5.118e+00", "20.6", "5.202e+00", "2.513e+00",
    "7.912e+00", "Cmax (ug/mL)", "132", "2.948e+01", "6.405e+00", "21.7", "2.868e+01",
    "25.1", "3.014e+01", "9.297e+00", "4.324e+01"
  )
  expected_matrix <- matrix(expected_matrix, nrow = 10, ncol = 10, byrow = TRUE)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("PKPT03 Drug Y is produced correctly", {
  # Plasma Drug Y__
  adpp1 <- adpp_plasma %>%
    dplyr::filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp1)
  main_title(result) <- paste("Summary of", unique(adpp1$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp1$PPCAT), "\nVisit:", unique(adpp1$AVISIT))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean",
    "Median", "Minimum", "Maximum", "ARM C", "", "", "", "", "", "", "", "", "", "AUCinf obs (day*ug/mL)",
    "132", "1.953e+02", "3.828e+01", "19.6", "1.916e+02", "20.0", "1.960e+02", "1.110e+02",
    "3.182e+02", "CL obs (ml/day/kg)", "132", "4.968e+00", "9.921e-01", "20.0", "4.864e+00",
    "21.3", "4.970e+00", "2.261e+00", "7.958e+00", "Cmax (ug/mL)", "132", "2.945e+01",
    "5.762e+00", "19.6", "2.888e+01", "20.3", "2.872e+01", "1.455e+01", "4.856e+01"
  )
  expected_matrix <- matrix(expected_matrix, nrow = 6, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})


# PKPT05 Drug X
testthat::test_that("PKPT05 Drug X is produced correctly", {
  adpp0 <- adpp_urine %>%
    dplyr::filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean",
    "Median", "Minimum", "Maximum", "ARM A", "", "", "", "", "", "", "", "", "",
    "Ae (mg)", "268", "1.539e+00", "2.988e-01", "19.4", "1.508e+00", "20.8",
    "1.518e+00", "6.223e-01", "2.328e+00", "CLR (L/hr)", "134", "5.010e-02",
    "1.026e-02", "20.5", "4.897e-02", "22.3", "4.935e-02", "2.370e-02",
    "7.332e-02", "Fe (%)", "268", "1.591e+01", "2.990e+00", "18.8", "1.562e+01",
    "19.8", "1.592e+01", "7.714e+00", "2.379e+01", "RENALCLD (L/hr/mg)", "134",
    "5.009e-03", "1.009e-03", "20.1", "4.905e-03", "21.1", "4.909e-03",
    "2.426e-03", "7.670e-03", "ARM C", "", "", "", "", "", "", "", "", "",
    "Ae (mg)", "264", "1.593e+00", "3.043e-01", "19.1", "1.563e+00", "19.8",
    "1.575e+00", "8.536e-01", "2.419e+00", "CLR (L/hr)", "132", "4.959e-02",
    "1.031e-02", "20.8", "4.854e-02", "21.0", "4.906e-02", "3.149e-02", "8.150e-02",
    "Fe (%)", "264", "1.562e+01", "2.909e+00", "18.6", "1.534e+01", "19.6", "1.557e+01",
    "7.197e+00", "2.294e+01", "RENALCLD (L/hr/mg)", "132", "4.963e-03", "1.006e-03",
    "20.3", "4.859e-03", "21.2", "4.980e-03", "2.344e-03", "7.306e-03"
  )

  expected_matrix <- matrix(expected_matrix, nrow = 12, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT05 Drug Y
testthat::test_that("PKPT05 Drug Y is produced correctly", {
  # Plasma Drug Y
  adpp1 <- adpp_urine %>%
    dplyr::filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp1)
  main_title(result) <- paste("Summary of", unique(adpp1$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp1$PPCAT), "\nVisit:", unique(adpp1$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)",
    "Geometric Mean", "CV % Geometric Mean", "Median",
    "Minimum", "Maximum", "ARM C", "", "", "", "", "", "", "", "", "",
    "Ae (mg)", "264", "1.587e+00", "2.881e-01", "18.2", "1.559e+00",
    "19.4", "1.593e+00", "7.698e-01", "2.152e+00", "CLR (L/hr)", "132",
    "5.056e-02", "1.011e-02", "20.0", "4.950e-02", "21.4", "5.101e-02",
    "2.226e-02", "7.606e-02", "Fe (%)", "264", "1.552e+01", "2.932e+00", "18.9",
    "1.524e+01", "19.7", "1.550e+01", "8.133e+00", "2.639e+01", "RENALCLD (L/hr/mg)",
    "132", "5.124e-03", "9.578e-04", "18.7", "5.029e-03", "20.0", "5.175e-03", "2.676e-03", "7.541e-03" # nolint
  )

  expected_matrix <- matrix(expected_matrix, nrow = 7, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})


# PKPT07 Drug X
testthat::test_that("PKPT07 Drug X is produced correctly", {
  # Plasma Drug X__
  adpp0 <- adpp_norm_dose %>%
    dplyr::filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean",
    "Median", "Minimum", "Maximum", "ARM A", "", "", "", "", "", "", "", "", "",
    "RENALCLD (L/hr/mg)", "134", "5.009e-03", "1.009e-03", "20.1", "4.905e-03", "21.1",
    "4.909e-03", "2.426e-03", "7.670e-03", "ARM C", "", "", "", "", "", "", "", "", "",
    "RENALCLD (L/hr/mg)", "132", "4.963e-03", "1.006e-03", "20.3", "4.859e-03",
    "21.2", "4.980e-03", "2.344e-03", "7.306e-03"
  )

  expected_matrix <- matrix(expected_matrix, nrow = 6, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})


# PKPT07 Drug Y
testthat::test_that("PKPT07 Drug Y is produced correctly", {
  # Plasma Drug Y
  adpp1 <- adpp_norm_dose %>%
    dplyr::filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp1)
  main_title(result) <- paste("Summary of", unique(adpp1$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp1$PPCAT), "\nVisit:", unique(adpp1$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean",
    "CV % Geometric Mean", "Median", "Minimum", "Maximum", "ARM C",
    "", "", "", "", "", "", "", "", "", "RENALCLD (L/hr/mg)", "132", "5.124e-03",
    "9.578e-04", "18.7", "5.029e-03", "20.0", "5.175e-03", "2.676e-03", "7.541e-03"
  )

  expected_matrix <- matrix(expected_matrix, nrow = 4, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})
