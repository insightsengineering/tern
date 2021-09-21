library(scda)
library(dplyr)

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
      AVALU = as.character(AVALU),
      is_event = CNSR == 0
    )

  reapply_varlabels(adtte_mod, adtte_labels, is_event = "Event Flag")
}

adtte <- synthetic_cdisc_data("rcd_2021_05_05")$adtte

# extract_survival_biomarkers ----

test_that("extract_survival_biomarkers functions as expected with valid input and default arguments", {

  adtte_f <- adtte %>%
    preprocess_adtte()

  result <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f
  )
  expected <- data.frame(
    biomarker = c(
      "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1"
    ),
    biomarker_label = c(
      "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1", "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1", "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1"
    ),
    n_tot = c(
      48L, 48L, 31L,
      31L, 17L, 17L, 12L, 12L, 18L, 18L, 18L, 18L
    ),
    n_tot_events = c(
      25L,
      25L, 14L, 14L, 11L, 11L, 4L, 4L, 11L, 11L, 10L, 10L
    ),
    median = c(
      1269.40388857211,
      1269.40388857211, 1724.81777754615, 1724.81777754615, 444.196273007555,
      444.196273007555, 1784.03239974904, 1784.03239974904, 478.509100211499,
      478.509100211499, 1054.27935012728, 1054.27935012728
    ),
    hr = c(
      1.02687761611746,
      0.990844235799699, 1.04626139836209, 1.06924577636498, 1.01135847232281,
      0.746129886511517, 1.19289447104515, 0.835910761807252, 1.00021800888586,
      1.08027836357756, 0.958739040930273, 0.840609949045828
    ),
    lcl = c(
      0.981115999689675,
      0.86617391141802, 0.917579858827001, 0.919092833321873, 0.950374160739,
      0.502886917063905, 0.977818079023576, 0.520128091488273, 0.936670241182532,
      0.938559750894667, 0.802193619739168, 0.614021746513603
    ),
    ucl = c(
      1.07477366470081,
      1.13345863535675, 1.19298925665387, 1.2439293277287, 1.07625607028685,
      1.10702782008335, 1.45527808247425, 1.34341292681542, 1.06807713249921,
      1.24339589642682, 1.14583378125442, 1.15081443034717
    ),
    conf_level = c(
      0.95,
      0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95
    ),
    pval = c(
      0.254157606438, 0.893353037334851, 0.499435396528544,
      0.385831216980903, 0.721892810549689, 0.145712146386295, 0.0820652030819593,
      0.459043683803393, 0.994806856643026, 0.281829406962774, 0.64317447099566,
      0.278616013086157
    ),
    pval_label = c(
      "p-value (Wald)", "p-value (Wald)",
      "p-value (Wald)", "p-value (Wald)", "p-value (Wald)", "p-value (Wald)",
      "p-value (Wald)", "p-value (Wald)", "p-value (Wald)", "p-value (Wald)",
      "p-value (Wald)", "p-value (Wald)"
    ),
    subgroup = c(
      "All Patients",
      "All Patients", "F", "F", "M", "M", "LOW", "LOW", "MEDIUM", "MEDIUM",
      "HIGH", "HIGH"
    ),
    var = c(
      "ALL", "ALL", "SEX", "SEX", "SEX", "SEX",
      "BMRKR2", "BMRKR2", "BMRKR2", "BMRKR2", "BMRKR2", "BMRKR2"
    ),
    var_label = c(
      "All Patients", "All Patients", "Sex", "Sex",
      "Sex", "Sex", "Categorical Level Biomarker 2", "Categorical Level Biomarker 2",
      "Categorical Level Biomarker 2", "Categorical Level Biomarker 2",
      "Categorical Level Biomarker 2", "Categorical Level Biomarker 2"
    ),
    row_type = c(
      "content", "content", "analysis", "analysis",
      "analysis", "analysis", "analysis", "analysis", "analysis",
      "analysis", "analysis", "analysis"
    )
  )
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("extract_survival_biomarkers works as expected with groups_lists", {

  adtte_f <- adtte %>%
    preprocess_adtte()

  result <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )
  sub_result <- subset(
    result,
    var == "BMRKR2",
    select = c(biomarker, subgroup)
  )
  expected <- data.frame(
    biomarker = c(
      "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1"
    ),
    subgroup = c(
      "low", "low", "low/medium", "low/medium", "low/medium/high", "low/medium/high"
    ),
    row.names = 7:12
  )
  expect_identical(sub_result, expected)
})

# tabulate_survival_biomarkers ----

test_that("tabulate_survival_biomarkers works as expected with valid input", {

  adtte_f <- adtte %>%
    preprocess_adtte()

  df <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f
  )

  result <- tabulate_survival_biomarkers(df, time_unit = as.character(adtte_f$AVALU[1]))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    matrix(
      nrow = 19,
      ncol = 7,
      data = c(
        "", "Age", "All Patients", "Sex", "F", "M", "Categorical Level Biomarker 2",
        "LOW", "MEDIUM", "HIGH", "Continous Level Biomarker 1", "All Patients",
        "Sex", "F", "M", "Categorical Level Biomarker 2", "LOW", "MEDIUM",
        "HIGH", "Total n", "", "48", "", "31", "17", "", "12", "18",
        "18", "", "48", "", "31", "17", "", "12", "18", "18", "Total Events",
        "", "25", "", "14", "11", "", "4", "11", "10", "", "25", "",
        "14", "11", "", "4", "11", "10", "Median (DAYS)", "", "1269.4",
        "", "1724.8", "444.2", "", "1784", "478.5", "1054.3", "", "1269.4",
        "", "1724.8", "444.2", "", "1784", "478.5", "1054.3", "Hazard Ratio",
        "", "1.03", "", "1.05", "1.01", "", "1.19", "1.00", "0.96", "",
        "0.99", "", "1.07", "0.75", "", "0.84", "1.08", "0.84", "95% Wald CI",
        "", "(0.98, 1.07)", "", "(0.92, 1.19)", "(0.95, 1.08)", "", "(0.98, 1.46)",
        "(0.94, 1.07)", "(0.80, 1.15)", "", "(0.87, 1.13)", "", "(0.92, 1.24)",
        "(0.50, 1.11)", "", "(0.52, 1.34)", "(0.94, 1.24)", "(0.61, 1.15)",
        "p-value (Wald)", "", "0.2542", "", "0.4994", "0.7219", "", "0.0821",
        "0.9948", "0.6432", "", "0.8934", "", "0.3858", "0.1457", "",
        "0.4590", "0.2818", "0.2786"
      )
    )
  )
  expect_identical(result_matrix, expected_matrix)
  expected_attrs <- list(
    col_x = 4L,
    col_ci = 5L,
    col_symbol_size = 1L,
    forest_header = c("Higher\nBetter", "Lower\nBetter")
  )
  result_attrs <- attributes(result)[names(expected_attrs)]
  expect_identical(result_attrs, expected_attrs)
})

test_that("tabulate_survival_biomarkers functions as expected with NULL subgroups", {

  adtte_f <- adtte %>%
    preprocess_adtte()

  df <- expect_silent(extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1")
    ),
    data = adtte_f
  ))

  result <- tabulate_survival_biomarkers(df, time_unit = as.character(adtte_f$AVALU[1]))
  result_matrix <- to_string_matrix(result)
  expected_first_col <- c("", "Age", "All Patients", "Continous Level Biomarker 1", "All Patients")
  expect_identical(result_matrix[, 1L], expected_first_col)
})
