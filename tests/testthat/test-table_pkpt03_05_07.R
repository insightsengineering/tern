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

# Helper function
threesigfmt <- function(x, ...) {
  as.character(signif(x, 3))
}

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
    vars = "AVAL",
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
      mean = threesigfmt,
      sd = threesigfmt,
      cv = "xx.x",
      median = threesigfmt,
      geom_mean = threesigfmt,
      geom_cv = "xx.x",
      min = threesigfmt,
      max = threesigfmt
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
  expected_matrix <- structure(
    c(
    "Treatment Arm", "  PK Parameter", "ARM A", "AUCinf obs (day*ug/mL)", " ",
    "CL obs (ml/day/kg)", " ", "Cmax (ug/mL)", " ", "ARM C",
    "AUCinf obs (day*ug/mL)", " ", "CL obs (ml/day/kg)", " ", "Cmax (ug/mL)",
    " ", "", "n", "", "", "134", "", "134", "", "134", "", "", "132", "",
    "132", "", "132", "", "Mean", "", "", "203", "", "5.04", "", "30.2", "",
    "", "195", "", "5.01", "", "30", "", "SD", "", "", "37.7", "", "1.04", "",
    "6.24", "", "", "37.8", "", "0.985", "", "5.46", "", "CV (%)", "", "",
    "18.6", "", "20.6", "", "20.6", "", "", "19.4", "", "19.7", "", "18.2",
    "", "Geometric Mean", "", "", "199", "", "4.93", "", "29.6", "", "",
    "192", "", "4.91", "", "29.5", "", "CV % Geometric Mean", "", "", "18.7",
    "", "22.4", "", "21.0", "", "", "20.1", "", "21.1", "", "18.9", "",
    "Median", "", "", "197", "", "5.08", "", "29.9", "", "", "196", "",
    "4.97", "", "29.8", "", "Minimum", "", "", "125", "", "2.25", "", "17.5",
    "", "", "103", "", "2.1", "", "15.9", "", "Maximum", "", "", "311", "",
    "7.39", "", "48.7", "", "", "315", "", "7.49", "", "47.6"
  ),
  .Dim = c(16L, 10L)
  )
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
  expected_matrix <- structure(
    c(
    "Treatment Arm", "  PK Parameter", "ARM C", "AUCinf obs (day*ug/mL)", " ",
    "CL obs (ml/day/kg)", " ", "Cmax (ug/mL)", " ", "", "n", "", "", "132", "",
    "132", "", "132", "", "Mean", "", "", "199", "", "4.96", "", "29.9", "",
    "SD", "", "", "37.9", "", "0.895", "", "5.55", "", "CV (%)", "", "",
    "19.1", "", "18.1", "", "18.6", "", "Geometric Mean", "", "", "195", "",
    "4.87", "", "29.4", "", "CV % Geometric Mean", "", "", "18.9", "", "18.7",
    "", "20.1", "", "Median", "", "", "195", "", "4.94", "", "29.7", "",
    "Minimum", "", "", "126", "", "2.99", "", "14.1", "", "Maximum", "", "",
    "318", "", "7.21", "", "43.4"
  ),
  .Dim = c(9L, 10L)
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

  expected_matrix <- structure(
    c(
    "Treatment Arm", "  PK Parameter", "ARM A", "Ae (mg)", " ", "CLR (L/hr)",
    " ", "Fe (%)", " ", "RENALCLD (L/hr/mg)", " ", "ARM C", "Ae (mg)", " ",
    "CLR (L/hr)", " ", "Fe (%)", " ", "RENALCLD (L/hr/mg)", " ", "", "n", "",
    "", "268", "", "134", "", "268", "", "134", "", "", "264", "", "132", "",
    "264", "", "132", "", "Mean", "", "", "1.55", "", "0.0492", "", "15.7", "",
    "0.00487", "", "", "1.54", "", "0.0502", "", "16.1", "", "0.00511", "",
    "SD", "", "", "0.338", "", "0.00961", "", "3.35", "", "0.000965", "",
    "", "0.298", "", "0.0105", "", "3.1", "", "0.000934", "", "CV (%)", "",
    "", "21.8", "", "19.5", "", "21.3", "", "19.8", "", "", "19.4", "",
    "20.9", "", "19.3", "", "18.3", "", "Geometric Mean", "", "", "1.51",
    "", "0.0482", "", "15.3", "", "0.00477", "", "", "1.51", "", "0.0491",
    "", "15.8", "", "0.00502", "", "CV % Geometric Mean", "", "", "23.0",
    "", "21.0", "", "22.2", "", "21.2", "", "", "20.3", "", "21.9", "", "20.2",
    "", "19.7", "", "Median", "", "", "1.55", "", "0.0491", "", "15.8", "",
    "0.00497", "", "", "1.55", "", "0.0498", "", "16", "", "0.00515", "",
    "Minimum", "", "", "0.702", "", "0.0249", "", "8.15", "", "0.00238", "",
    "", "0.85", "", "0.0251", "", "8.5", "", "0.00236", "", "Maximum", "", "",
    "2.46", "", "0.0751", "", "24.5", "", "0.00726", "", "", "2.21", "",
    "0.0856", "", "24.4", "", "0.00741"
  ), .Dim = c(20L, 10L)
  )

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

  expected_matrix <- structure(
    c(
    "Treatment Arm", "  PK Parameter", "ARM C", "Ae (mg)", " ", "CLR (L/hr)",
    " ", "Fe (%)", " ", "RENALCLD (L/hr/mg)", " ", "", "n", "", "", "264", "",
    "132", "", "264", "", "132", "", "Mean", "", "", "1.6", "", "0.0497",
    "", "15.8", "", "0.00509", "", "SD", "", "", "0.315", "", "0.0101", "",
    "3.08", "", "0.00103", "", "CV (%)", "", "", "19.7", "", "20.3", "",
    "19.4", "", "20.3", "", "Geometric Mean", "", "", "1.56", "", "0.0486",
    "", "15.5", "", "0.00499", "", "CV % Geometric Mean", "", "", "21.4",
    "", "22.0", "", "20.2", "", "21.4", "", "Median", "", "", "1.6", "",
    "0.0491", "", "15.7", "", "0.00502", "", "Minimum", "", "", "0.857",
    "", "0.0184", "", "8.31", "", "0.00236", "", "Maximum", "", "", "2.26",
    "", "0.0776", "", "23.8", "", "0.00794"
  ), .Dim = c(11L, 10L)
)

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

  expected_matrix <- structure(
    c(
    "Treatment Arm", "  PK Parameter", "ARM A", "RENALCLD (L/hr/mg)", " ",
    "ARM C", "RENALCLD (L/hr/mg)", " ", "", "n", "", "", "134", "", "", "132",
    "", "Mean", "", "", "0.00487", "", "", "0.00511", "", "SD", "", "",
    "0.000965", "", "", "0.000934", "", "CV (%)", "", "", "19.8", "", "",
    "18.3", "", "Geometric Mean", "", "", "0.00477", "", "", "0.00502", "",
    "CV % Geometric Mean", "", "", "21.2", "", "", "19.7", "", "Median", "",
    "", "0.00497", "", "", "0.00515", "", "Minimum", "", "", "0.00238", "",
    "", "0.00236", "", "Maximum", "", "", "0.00726", "", "", "0.00741"
  ), .Dim = c(8L, 10L)
  )

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

  expected_matrix <- structure(
    c(
    "Treatment Arm", "  PK Parameter", "ARM C", "RENALCLD (L/hr/mg)", " ",
    "", "n", "", "", "132", "", "Mean", "", "", "0.00509", "", "SD", "", "",
    "0.00103", "", "CV (%)", "", "", "20.3", "", "Geometric Mean", "", "",
    "0.00499", "", "CV % Geometric Mean", "", "", "21.4", "", "Median", "",
    "", "0.00502", "", "Minimum", "", "", "0.00236", "", "Maximum", "",
    "", "0.00794"
    ), .Dim = c(5L, 10L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
