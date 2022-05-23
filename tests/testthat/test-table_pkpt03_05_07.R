# Preparation of the test case.
library(rtables)

# Data generation
adpp <- scda::synthetic_cdisc_data("rcd_2022_02_28")$adpp
adpp_03 <- adpp %>% dplyr::filter(PPSPEC == "Plasma", AVISIT == "CYCLE 1 DAY 1")
adpp_urine <- adpp %>% dplyr::filter(PPSPEC == "Urine", AVISIT == "CYCLE 1 DAY 1")
adpp_norm_dose <- adpp %>% dplyr::filter(
  AVISIT == "CYCLE 1 DAY 1",
  stringr::str_detect(
    tolower(PARAM),
    stringr::regex("norm by dose", ignore_case = TRUE)
  )
)

# PKPT03
testthat::test_that("PKPT03 Drug X is produced correctly", {
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
  # Plasma Drug x
  adpp0 <- adpp_03 %>%
    dplyr::filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(l, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))


  result_matrix <- to_string_matrix(result)
  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "ARM A", "", "", "", "", "", "", "", "", "",
    "AUCinf obs (day*ug/mL)", "134", "1.974e+02", "4.021e+01", "20.4", "1.929e+02", "23.1", "1.964e+02", "7.070e+01", "2.807e+02", # nolint
    "CL obs (ml/day/kg)", "134", "5.104e+00", "9.812e-01", "19.2", "5.005e+00", "20.6", "5.180e+00", "2.737e+00", "7.542e+00", # nolint
    "Cmax (ug/mL)", "134", "3.121e+01", "6.094e+00", "19.5", "3.057e+01", "21.1", "3.169e+01", "1.609e+01", "4.591e+01",
    "ARM C", "", "", "", "", "", "", "", "", "",
    "AUCinf obs (day*ug/mL)", "132", "2.048e+02", "3.910e+01", "19.1", "2.009e+02", "20.4", "2.056e+02", "9.943e+01", "2.988e+02", # nolint
    "CL obs (ml/day/kg)", "132", "4.998e+00", "1.042e+00", "20.9", "4.875e+00", "23.6", "5.126e+00", "1.853e+00", "6.946e+00", # nolint
    "Cmax (ug/mL)", "132", "2.982e+01", "5.698e+00", "19.1", "2.926e+01", "19.9", "3.013e+01", "1.657e+01", "4.385e+01"
  )
  expected_matrix <- matrix(expected_matrix, nrow = 10, ncol = 10, byrow = TRUE)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("PKPT03 Drug Y is produced correctly", {
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
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "ARM C", "", "", "", "", "", "", "", "", "",
    "AUCinf obs (day*ug/mL)", "132", "1.982e+02", "3.995e+01", "20.2", "1.939e+02", "22.1", "1.988e+02", "7.760e+01", "2.931e+02", # nolint
    "CL obs (ml/day/kg)", "132", "4.882e+00", "9.571e-01", "19.6", "4.790e+00", "20.0", "4.900e+00", "2.775e+00", "7.955e+00", # nolint
    "Cmax (ug/mL)", "132", "2.968e+01", "6.297e+00", "21.2", "2.896e+01", "23.2", "2.990e+01", "1.480e+01", "4.430e+01" # nolint
  )
  expected_matrix <- matrix(expected_matrix, nrow = 6, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})


# PKPT05 Drug X
testthat::test_that("PKPT05 Drug X is produced correctly", {
  lyt <- basic_table() %>%
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
  adpp0 <- adpp_urine %>%
    dplyr::filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(lyt, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "ARM A", "", "", "", "", "", "", "", "", "",
    "CLR (L/hr)", "134", "5.089e-02", "9.769e-03", "19.2", "4.992e-02", "20.3", "5.020e-02", "2.384e-02", "8.111e-02",
    "RENALCLD (L/hr/mg)", "134", "5.076e-03", "1.027e-03", "20.2", "4.971e-03", "21.0", "5.065e-03", "2.896e-03", "7.598e-03", # nolint
    "ARM C", "", "", "", "", "", "", "", "", "",
    "CLR (L/hr)", "132", "4.993e-02", "9.577e-03", "19.2", "4.895e-02", "20.6", "5.003e-02", "2.690e-02", "7.318e-02",
    "RENALCLD (L/hr/mg)", "132", "4.893e-03", "9.184e-04", "18.8", "4.800e-03", "20.6", "4.945e-03", "2.177e-03", "6.667e-03" # nolint
  )

  expected_matrix <- matrix(expected_matrix, nrow = 8, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT05 Drug Y
testthat::test_that("PKPT05 Drug Y is produced correctly", {
  lyt <- basic_table() %>%
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

  # Plasma Drug Y
  adpp1 <- adpp_urine %>%
    dplyr::filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(lyt, df = adpp1)
  main_title(result) <- paste("Summary of", unique(adpp1$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp1$PPCAT), "\nVisit:", unique(adpp1$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "ARM C", "", "", "", "", "", "", "", "", "",
    "CLR (L/hr)", "132", "4.975e-02", "8.945e-03", "18.0", "4.891e-02", "19.2", "4.956e-02", "2.242e-02", "7.184e-02",
    "RENALCLD (L/hr/mg)", "132", "5.068e-03", "1.038e-03", "20.5", "4.951e-03", "22.7", "5.209e-03", "2.453e-03", "7.425e-03" # nolint
  )

  expected_matrix <- matrix(expected_matrix, nrow = 5, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})


# PKPT07 Drug X
testthat::test_that("PKPT07 Drug X is produced correctly", {
  lyt <- basic_table() %>%
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

  # Plasma Drug X__
  adpp0 <- adpp_norm_dose %>%
    dplyr::filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(lyt, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "ARM A", "", "", "", "", "", "", "", "", "",
    "RENALCLD (L/hr/mg)", "134", "5.076e-03", "1.027e-03", "20.2", "4.971e-03", "21.0", "5.065e-03", "2.896e-03", "7.598e-03", # nolint
    "ARM C", "", "", "", "", "", "", "", "", "",
    "RENALCLD (L/hr/mg)", "132", "4.893e-03", "9.184e-04", "18.8", "4.800e-03", "20.6", "4.945e-03", "2.177e-03", "6.667e-03" # nolint
  )

  expected_matrix <- matrix(expected_matrix, nrow = 6, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})


# PKPT07 Drug Y
testthat::test_that("PKPT07 Drug Y is produced correctly", {
  lyt <- basic_table() %>%
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

  # Plasma Drug Y
  adpp1 <- adpp_norm_dose %>%
    dplyr::filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    dplyr::mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(lyt, df = adpp1)
  main_title(result) <- paste("Summary of", unique(adpp1$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp1$PPCAT), "\nVisit:", unique(adpp1$AVISIT))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- c(
    "Treatment Arm", "", "", "", "", "", "", "", "", "",
    "  PK Parameter", "n", "Mean", "SD", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum", # nolint
    "ARM C", "", "", "", "", "", "", "", "", "",
    "RENALCLD (L/hr/mg)", "132", "5.068e-03", "1.038e-03", "20.5", "4.951e-03", "22.7", "5.209e-03", "2.453e-03", "7.425e-03" # nolint
  )

  expected_matrix <- matrix(expected_matrix, nrow = 4, ncol = 10, byrow = TRUE)
  testthat::expect_identical(result_matrix, expected_matrix)
})
