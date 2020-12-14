library(random.cdisc.data)

preprocess_adtte <- function(adtte) {

  # Save variable labels before data processing steps.
  adtte_labels <- var_labels(adtte)

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

test_that("FSTG02 table variant 1 (Subgroup Analysis of Survival Duration) is produced correctly", {

  anl1 <- radtte(cached = TRUE) %>%
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
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Categorical Level Biomarker 2",
      "LOW", "MEDIUM", "HIGH", " ", "Total n", "268", "", "161", "107",
      "", "95", "93", "80", "B: Placebo", "n", "134", "", "82", "52",
      "", "45", "56", "33", "B: Placebo", "Median (Months)", "26.7", "", "22.2",
      "38.8", "", "26.9", "31.9", "15.7", "A: Drug X", "n", "134",
      "", "79", "55", "", "50", "37", "47", "A: Drug X", "Median (Months)",
      "33.2", "", "32.4", "34.9", "", "31.7", "48.4", "31.4", " ",
      "Hazard Ratio", "0.84", "", "0.65", "1.23", "", "1.06", "0.61",
      "0.77", " ", "95% Wald CI", "(0.62, 1.14)", "", "(0.44, 0.97)",
      "(0.76, 1.98)", "", "(0.64, 1.76)", "(0.34, 1.07)", "(0.45, 1.32)"
      ),
    .Dim = c(10L, 8L)
  )
  expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    col_x = 6,
    col_ci = 7,
    forest_header = c(paste0(levels(anl1$ARM)[2:1], rep("\nBetter", 2))),
    vline = 1,
    xlim = c(.1, 10),
    logx = TRUE,
    x_at = c(.1, 1, 10),
    col_symbol_size = 1,
    draw = FALSE
  )

})

test_that("FSTG02 table variant 2 (specifying class variables and options for the treatment variable)", {

  anl2 <- radtte(cached = TRUE) %>%
    preprocess_adtte() %>%
    mutate(
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
      "Baseline Risk Factors", "", "All Patients", "Sex", "M", "F", "Stratification Factor 1",
      "C", "B", "A", " ", "Total n", "268", "", "107", "161", "", "94",
      "92", "82", "Placebo", "n", "134", "", "52", "82", "", "45",
      "45", "44", "Placebo", "Median (Months)", "26.7", "", "38.8", "22.2",
      "", "26.7", "32.4", "26.9", "Drug X", "n", "134", "", "55", "79",
      "", "49", "47", "38", "Drug X", "Median (Months)", "33.2", "", "34.9",
      "32.4", "", "40.8", "31.7", "32.4", " ", "Hazard Ratio", "0.84",
      "", "1.23", "0.65", "", "0.77", "0.98", "0.76", " ", "95% Wald CI",
      "(0.62, 1.14)", "", "(0.76, 1.98)", "(0.44, 0.97)", "", "(0.45, 1.32)",
      "(0.60, 1.59)", "(0.44, 1.32)"),
    .Dim = c(10L, 8L)
  )

  #expected_matrix
  expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    col_x = 6,
    col_ci = 7,
    forest_header = c(paste0(levels(anl2$ARM)[2:1], rep("\nBetter", 2))),
    vline = 1,
    xlim = c(.1, 10),
    logx = TRUE,
    x_at = c(.1, 1, 10),
    col_symbol_size = 1,
    draw = FALSE
  )

})

test_that("FSTG02 table variant 3 (selecting columns and changing the alpha level)", {

  anl3 <- radtte(cached = TRUE) %>%
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
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Categorical Level Biomarker 2",
      "LOW", "MEDIUM", "HIGH", " ", "Total n", "268", "", "161", "107",
      "", "95", "93", "80", " ", "Hazard Ratio", "0.84", "", "0.65",
      "1.23", "", "1.06", "0.61", "0.77", " ", "90% Wald CI", "(0.65, 1.08)",
      "", "(0.47, 0.91)", "(0.83, 1.84)", "", "(0.69, 1.62)", "(0.38, 0.98)",
      "(0.49, 1.21)"),
    .Dim = c(10L, 4L)
    )

  expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    col_x = 2,
    col_ci = 3,
    forest_header = c(paste0(levels(anl3$ARM)[2:1], rep("\nBetter", 2))),
    vline = 1,
    xlim = c(.1, 10),
    logx = TRUE,
    x_at = c(.1, 1, 10),
    col_symbol_size = 1,
    draw = FALSE
  )
})

test_that("FSTG02 table variant 4 (fixed symbol size) is produced correctly", {

  anl4 <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
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
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Categorical Level Biomarker 2",
      "LOW", "MEDIUM", "HIGH", " ", "Total n", "268", "", "161", "107",
      "", "95", "93", "80", "B: Placebo", "n", "134", "", "82", "52",
      "", "45", "56", "33", "B: Placebo", "Median (Months)", "26.7", "", "22.2",
      "38.8", "", "26.9", "31.9", "15.7", "A: Drug X", "n", "134",
      "", "79", "55", "", "50", "37", "47", "A: Drug X", "Median (Months)",
      "33.2", "", "32.4", "34.9", "", "31.7", "48.4", "31.4", " ",
      "Hazard Ratio", "0.84", "", "0.65", "1.23", "", "1.06", "0.61",
      "0.77", " ", "95% Wald CI", "(0.62, 1.14)", "", "(0.44, 0.97)",
      "(0.76, 1.98)", "", "(0.64, 1.76)", "(0.34, 1.07)", "(0.45, 1.32)"
    ),
    .Dim = c(10L, 8L)
  )
  expect_equal(result_matrix, expected_matrix)

  # Add plot.
  g_forest(
    tbl = result,
    col_x = 6,
    col_ci = 7,
    forest_header = c(paste0(levels(anl4$ARM)[2:1], rep("\nBetter", 2))),
    vline = 1,
    xlim = c(.1, 10),
    logx = TRUE,
    x_at = c(.1, 1, 10),
    col_symbol_size = NULL,
    draw = FALSE
  )

})
