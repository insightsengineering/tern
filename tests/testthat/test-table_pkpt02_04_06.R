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
  split_cols_by(
    var = "ARMCD",
    split_fun = trim_levels_in_group("ARMCD"),
    # label_pos = "topleft", # nolint
    split_label = "Treatment Arm"
  ) %>%
  split_rows_by(
    var = "PKPARAM",
    label_pos = "topleft",
    split_label = "PK Parameter"
  ) %>%
  summarize_vars(
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

# PKPT02 Drug X
testthat::test_that("PKPT02 is produced correctly for Drug X", {
  adpp0 <- adpp_plasma %>%
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp0)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "AUCinf obs (day*ug/mL)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max",
      "CL obs (ml/day/kg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "Cmax (ug/mL)", "n",
      "Mean (SD)", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median",
      "Min - Max", "ARM A", "", "134", "2.028e+02 (3.766e+01)", "18.6",
      "1.994e+02", "18.7", "1.971e+02", "1.253e+02 - 3.110e+02", "", "134",
      "5.043e+00 (1.041e+00)", "20.6", "4.929e+00", "22.4", "5.078e+00",
      "2.255e+00 - 7.395e+00", "", "134", "3.025e+01 (6.239e+00)", "20.6",
      "2.961e+01", "21.0", "2.986e+01", "1.753e+01 - 4.871e+01", "ARM C", "",
      "132", "1.955e+02 (3.785e+01)", "19.4", "1.917e+02", "20.1", "1.962e+02",
      "1.030e+02 - 3.145e+02", "", "132", "5.009e+00 (9.846e-01)", "19.7",
      "4.907e+00", "21.1", "4.969e+00", "2.102e+00 - 7.489e+00", "", "132",
      "3.004e+01 (5.457e+00)", "18.2", "2.954e+01", "18.9", "2.977e+01",
      "1.585e+01 - 4.757e+01"
    ),
    .Dim = c(25L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT02 Drug Y
testthat::test_that("PKPT02 is produced correctly for Drug Y", {
  adpp1 <- adpp_plasma %>%
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp1)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "AUCinf obs (day*ug/mL)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max",
      "CL obs (ml/day/kg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "Cmax (ug/mL)", "n",
      "Mean (SD)", "CV (%)", "Geometric Mean", "CV % Geometric Mean",
      "Median", "Min - Max", "ARM C", "", "132", "1.986e+02 (3.792e+01)",
      "19.1", "1.952e+02", "18.9", "1.953e+02", "1.264e+02 - 3.183e+02", "",
      "132", "4.955e+00 (8.951e-01)", "18.1", "4.873e+00", "18.7", "4.936e+00",
      "2.987e+00 - 7.211e+00", "", "132", "2.990e+01 (5.550e+00)", "18.6",
      "2.935e+01", "20.1", "2.969e+01", "1.406e+01 - 4.345e+01"
    ),
    .Dim = c(25L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT04 Drug X
testthat::test_that("PKPT04 is produced correctly for Drug X", {
  adpp0 <- adpp_urine %>%
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp0)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "Ae (mg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "CLR (L/hr)", "n",
      "Mean (SD)", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median",
      "Min - Max", "Fe (%)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "RENALCLD (L/hr/mg)", "n",
      "Mean (SD)", "CV (%)", "Geometric Mean", "CV % Geometric Mean", "Median",
      "Min - Max", "ARM A", "", "268", "1.551e+00 (3.385e-01)", "21.8",
      "1.513e+00", "23.0", "1.547e+00", "7.021e-01 - 2.464e+00", "", "134",
      "4.918e-02 (9.611e-03)", "19.5", "4.818e-02", "21.0", "4.909e-02",
      "2.488e-02 - 7.511e-02", "", "268", "1.571e+01 (3.348e+00)", "21.3",
      "1.534e+01", "22.2", "1.577e+01", "8.147e+00 - 2.452e+01", "", "134",
      "4.873e-03 (9.654e-04)", "19.8", "4.772e-03", "21.2", "4.967e-03",
      "2.385e-03 - 7.258e-03", "ARM C", "", "264", "1.535e+00 (2.978e-01)",
      "19.4", "1.505e+00", "20.3", "1.547e+00", "8.502e-01 - 2.208e+00", "",
      "132", "5.024e-02 (1.050e-02)", "20.9", "4.913e-02", "21.9", "4.984e-02",
      "2.505e-02 - 8.560e-02", "", "264", "1.609e+01 (3.103e+00)", "19.3",
      "1.578e+01", "20.2", "1.604e+01", "8.503e+00 - 2.443e+01", "", "132",
      "5.110e-03 (9.339e-04)", "18.3", "5.019e-03", "19.7", "5.151e-03",
      "2.356e-03 - 7.407e-03"
    ),
    .Dim = c(33L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT04 Drug Y
testthat::test_that("PKPT04 is produced correctly for Drug Y", {
  adpp1 <- adpp_urine %>%
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp1)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "Ae (mg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "CLR (L/hr)", "n",
      "Mean (SD)", "CV (%)", "Geometric Mean", "CV % Geometric Mean",
      "Median", "Min - Max", "Fe (%)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max",
      "RENALCLD (L/hr/mg)", "n", "Mean (SD)", "CV (%)", "Geometric Mean",
      "CV % Geometric Mean", "Median", "Min - Max", "ARM C", "", "264",
      "1.598e+00 (3.154e-01)", "19.7", "1.565e+00", "21.4", "1.603e+00",
      "8.574e-01 - 2.257e+00", "", "132", "4.966e-02 (1.009e-02)", "20.3",
      "4.857e-02", "22.0", "4.912e-02", "1.839e-02 - 7.761e-02", "", "264",
      "1.583e+01 (3.077e+00)", "19.4", "1.552e+01", "20.2", "1.570e+01",
      "8.311e+00 - 2.378e+01", "", "132", "5.093e-03 (1.032e-03)", "20.3",
      "4.985e-03", "21.4", "5.017e-03", "2.356e-03 - 7.939e-03"
    ),
    .Dim = c(33L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT06 Drug X
testthat::test_that("PKPT06 is produced correctly for Drug X", {
  adpp0 <- adpp_norm_dose %>%
    filter(PPCAT == "Plasma Drug X") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp0)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "RENALCLD (L/hr/mg)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max", "ARM A",
      "", "134", "4.873e-03 (9.654e-04)", "19.8", "4.772e-03", "21.2",
      "4.967e-03", "2.385e-03 - 7.258e-03", "ARM C", "", "132",
      "5.110e-03 (9.339e-04)", "18.3", "5.019e-03", "19.7", "5.151e-03",
      "2.356e-03 - 7.407e-03"
    ),
    .Dim = c(9L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

# PKPT06 Drug Y
testthat::test_that("PKPT06 is produced correctly for Drug Y", {
  adpp1 <- adpp_norm_dose %>%
    filter(PPCAT == "Plasma Drug Y") %>%
    h_pkparam_sort() %>%
    mutate(PKPARAM = factor(paste0(TLG_DISPLAY, " (", AVALU, ")")))

  result <- build_table(l, df = adpp1)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "PK Parameter", "RENALCLD (L/hr/mg)", "n", "Mean (SD)", "CV (%)",
      "Geometric Mean", "CV % Geometric Mean", "Median", "Min - Max", "ARM C",
      "", "132", "5.093e-03 (1.032e-03)", "20.3", "4.985e-03", "21.4",
      "5.017e-03", "2.356e-03 - 7.939e-03"
    ),
    .Dim = c(9L, 2L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
