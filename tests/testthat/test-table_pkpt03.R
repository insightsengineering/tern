# Data generation
adpp <- adpp_raw
adpp_plasma <- adpp %>% dplyr::filter(PPSPEC == "Plasma", AVISIT == "CYCLE 1 DAY 1")

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
  analyze_vars_in_cols(
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
})
