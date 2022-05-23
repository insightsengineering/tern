library(scda)
library(dplyr)

preprocess_adtte <- function(adtte) {

  # Save variable labels before data processing steps.
  adtte_labels <- formatters::var_labels(adtte)

  adtte_mod <- adtte %>%
    dplyr::filter(
      PARAMCD == "OS",
      ARM %in% c("B: Placebo", "A: Drug X"),
      SEX %in% c("M", "F")
    ) %>%
    dplyr::mutate(
      # Reorder levels of ARM to display reference arm before treatment arm.
      ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
      SEX = droplevels(SEX),
      is_event = CNSR == 0,
      # Convert time to MONTH
      AVAL = day2month(AVAL),
      AVALU = "Months"
    )

  reapply_varlabels(adtte_mod, adtte_labels, AVAL = adtte_labels["AVAL"])
}

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("FSTG02 table variant 1 (Subgroup Analysis of Survival Duration) is produced correctly", {
  anl1 <- adtte %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = anl1
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "median", "hr", "ci"),
      time_unit = anl1$AVALU[1]
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex",
      "F", "M", "Categorical Level Biomarker 2", "LOW", "MEDIUM", "HIGH",
      " ", "Total n", "268", "", "161", "107", "", "95", "93", "80",
      "B: Placebo", "n", "134", "", "82", "52", "", "45", "56", "33",
      "B: Placebo", "Median (Months)", "27.5", "", "28.0", "17.3", "",
      "24.7", "23.7", "27.9", "A: Drug X", "n", "134", "", "79", "55",
      "", "50", "37", "47", "A: Drug X", "Median (Months)", "41.4",
      "", "41.9", "27.9", "", "38.1", "41.7", "35.2", " ", "Hazard Ratio",
      "0.72", "", "0.70", "0.78", "", "0.71", "0.57", "0.98", " ",
      "95% Wald CI", "(0.53, 0.98)", "", "(0.46, 1.05)", "(0.49, 1.26)",
      "", "(0.42, 1.17)", "(0.32, 1.01)", "(0.56, 1.72)"
    ),
    .Dim = c(10L, 8L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})

testthat::test_that("FSTG02 table variant 2 (specifying class variables and options for the treatment variable)", {
  anl2 <- adtte %>%
    preprocess_adtte() %>%
    dplyr::mutate(
      # Recode levels of arm.
      ARM = forcats::fct_recode(
        ARM,
        "Placebo" = "B: Placebo",
        "Drug X" = "A: Drug X"
      ),
      # Reorder levels of `SEX`.
      SEX = forcats::fct_relevel(SEX, "M", "F"),
      # Reorder levels of `STRATA1`` by frequency.
      STRATA1 = forcats::fct_infreq(STRATA1)
    )

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "STRATA1")),
    data = anl2
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "median", "hr", "ci"),
      time_unit = anl2$AVALU[1]
    )
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex",
      "M", "F", "Stratification Factor 1", "C", "B", "A", " ", "Total n",
      "268", "", "107", "161", "", "94", "92", "82", "Placebo", "n",
      "134", "", "52", "82", "", "45", "45", "44", "Placebo", "Median (Months)",
      "27.5", "", "17.3", "28.0", "", "16.3", "27.9", "35.7", "Drug X",
      "n", "134", "", "55", "79", "", "49", "47", "38", "Drug X", "Median (Months)",
      "41.4", "", "27.9", "41.9", "", "54.7", "32.4", "35.2", " ",
      "Hazard Ratio", "0.72", "", "0.78", "0.70", "", "0.53", "0.87",
      "0.86", " ", "95% Wald CI", "(0.53, 0.98)", "", "(0.49, 1.26)",
      "(0.46, 1.05)", "", "(0.31, 0.90)", "(0.53, 1.45)", "(0.48, 1.53)"
    ),
    .Dim = c(10L, 8L)
  )

  # expected_matrix
  testthat::expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})

testthat::test_that("FSTG02 table variant 3 (selecting columns and changing the alpha level)", {
  anl3 <- adtte %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    control = control_coxph(conf_level = 0.9),
    data = anl3
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "hr", "ci")
    )

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex",
      "F", "M", "Categorical Level Biomarker 2", "LOW", "MEDIUM", "HIGH",
      " ", "Total n", "268", "", "161", "107", "", "95", "93", "80",
      " ", "Hazard Ratio", "0.72", "", "0.70", "0.78", "", "0.71",
      "0.57", "0.98", " ", "90% Wald CI", "(0.55, 0.93)", "", "(0.50, 0.98)",
      "(0.53, 1.17)", "", "(0.46, 1.08)", "(0.36, 0.92)", "(0.61, 1.57)"
    ),
    .Dim = c(10L, 4L)
  )

  testthat::expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})

testthat::test_that("FSTG02 table variant 4 (fixed symbol size) is produced correctly", {
  anl4 <- adtte %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2")
    ),
    data = anl4
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "median", "hr", "ci"),
      time_unit = anl4$AVALU[1]
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex",
      "F", "M", "Categorical Level Biomarker 2", "LOW", "MEDIUM", "HIGH",
      " ", "Total n", "268", "", "161", "107", "", "95", "93", "80",
      "B: Placebo", "n", "134", "", "82", "52", "", "45", "56", "33",
      "B: Placebo", "Median (Months)", "27.5", "", "28.0", "17.3", "",
      "24.7", "23.7", "27.9", "A: Drug X", "n", "134", "", "79", "55",
      "", "50", "37", "47", "A: Drug X", "Median (Months)", "41.4",
      "", "41.9", "27.9", "", "38.1", "41.7", "35.2", " ", "Hazard Ratio",
      "0.72", "", "0.70", "0.78", "", "0.71", "0.57", "0.98", " ",
      "95% Wald CI", "(0.53, 0.98)", "", "(0.46, 1.05)", "(0.49, 1.26)",
      "", "(0.42, 1.17)", "(0.32, 1.01)", "(0.56, 1.72)"
    ),
    .Dim = c(10L, 8L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    draw = FALSE
  )
})
