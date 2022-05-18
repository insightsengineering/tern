# Preparation of the test case.
library(scda)
library(rtables)
library(dplyr)
library(tern)

# Data generation
adpp <- synthetic_cdisc_data("latest")$adpp
adpp_03 <- adpp %>% filter(PPSPEC == "Plasma", AVISIT == "CYCLE 1 DAY 1")
adpp_urine <- adpp %>% filter(PPSPEC == "Urine", AVISIT == "CYCLE 1 DAY 1")
adpp_norm_dose <- adpp %>% filter(
  AVISIT == "CYCLE 1 DAY 1",
  stringr::str_detect(
    tolower(PARAM),
    stringr::regex("norm by dose", ignore_case = TRUE)
  )
)

# PKPT03
testthat::test_that("PKPT03 is produced correctly", {
  l <- basic_table() %>%
    split_rows_by(
      var = "ARM",
      split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))
    ) %>%
    split_rows_by(var = "PARAM") %>%
    summarize_vars_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpp_03)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "A: Drug X", "AUC Infinity Obs", "Max Conc", "Renal CL", "Renal CL Norm by Dose",
      "Time of Maximum Response", "Time to Onset", "Total CL Obs", "C: Combination",
      "AUC Infinity Obs", "Max Conc", "Renal CL", "Renal CL Norm by Dose",
      "Time of Maximum Response", "Time to Onset", "Total CL Obs", "n", "", "402",
      "402", "402", "402", "402", "402", "402", "", "792", "792", "792", "792",
      "792", "792", "792", "Mean", "", "199.2", "30.2", "0.1", "0.0", "9.9", "3.0",
      "5.0", "", "200.2", "30.1", "0.0", "0.0", "10.0", "3.0", "5.0", "SD", "", "39.1",
      "6.1", "0.0", "0.0", "1.9", "0.6", "1.0", "", "40.3", "6.0", "0.0", "0.0", "2.0",
      "0.6", "1.0", "SE", "", "1.9", "0.3", "0.0", "0.0", "0.1", "0.0", "0.1", "",
      "1.4", "0.2", "0.0", "0.0", "0.1", "0.0", "0.0", "CV (%)", "", "19.6", "20.3",
      "19.8", "19.8", "19.7", "19.3", "21.1", "", "20.1", "20.0", "19.5", "19.3",
      "20.3", "19.7", "20.2", "CV % Geometric Mean", "", "21.5", "21.7", "20.7",
      "20.4", "20.9", "20.3", "22.8", "", "21.6", "21.6", "21.0", "20.5", "21.9", "20.8", "21.6"
    ),
    .Dim = c(17L, 7L)
  )
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
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
  result <- build_table(lyt, df = adpp0)
  main_title(result) <- paste("Summary of", unique(adpp0$PPSPEC), "PK Parameter by Treatment Arm, PK Population")
  subtitles(result) <- paste("Analyte:", unique(adpp0$PPCAT), "\nVisit:", unique(adpp0$AVISIT))
  result <- build_table(lyt, df = adpp0)
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
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
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
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
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
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))
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
