library(random.cdisc.data)
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

test_that("extract_rsp_subgroups functions as expected with valid input and default arguments", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  expected <- list(
    survtime = data.frame(
      arm = factor(rep(c("B: Placebo", "A: Drug X"), 6), levels = c("B: Placebo", "A: Drug X")),
      n = c(134, 134, 82, 79, 52, 55, 45, 50, 56, 37, 33, 47),
      n_events = c(92, 81, 59, 44, 33, 37, 28, 34, 37, 19, 27, 28),
      median = c(
        813.5769, 1010.2328, 676.0553, 987.2498, 1180.4056, 1062.6402,
        818.6373, 965.8316, 971.3165, 1474.6575, 476.9332, 956.0405
      ),
      subgroup = c(
        "All Patients", "All Patients", "F", "F", "M", "M",
        "LOW", "LOW", "MEDIUM", "MEDIUM", "HIGH", "HIGH"
      ),
      var = c(rep("ALL", 2), rep("SEX", 4), rep("BMRKR2", 6)),
      var_label = c(rep("All Patients", 2), rep("Sex", 4), rep("Categorical Level Biomarker 2", 6)),
      row_type = c(rep("content", 2), rep("analysis", 10)),
      stringsAsFactors = FALSE
    ),
    hr = data.frame(
      arm = rep(" ", 6),
      n_tot = c(268, 161, 107, 95, 93, 80),
      hr = c(0.8412573, 0.6531635, 1.2312049, 1.0592234, 0.6068906, 0.7683790),
      lcl = c(0.6231147, 0.4390645, 0.7644891, 0.6383431, 0.3432599, 0.4489251),
      ucl = c(1.1357683, 0.9716624, 1.9828478, 1.7576037, 1.0729952, 1.3151552),
      conf_level = 0.95,
      pval = c(0.25844564, 0.03429498, 0.39147827, 0.82376106, 0.08300454, 0.33527427),
      pval_label = rep("p-value (log-rank)", 6),
      subgroup = c("All Patients", "F", "M", "LOW", "MEDIUM", "HIGH"),
      var = c("ALL", "SEX", "SEX", "BMRKR2", "BMRKR2", "BMRKR2"),
      var_label = c("All Patients", "Sex", "Sex", rep("Categorical Level Biomarker 2", 3)),
      row_type = c("content", rep("analysis", 5)),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("extract_rsp_subgroups functions as expected with NULL subgroups", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  expected <- list(
    survtime = data.frame(
      arm = factor(c("B: Placebo", "A: Drug X"), levels = c("B: Placebo", "A: Drug X")),
      n = c(134, 134),
      n_events = c(92, 81),
      median = c(813.5769, 1010.2328),
      subgroup = rep("All Patients", 2),
      var = rep("ALL", 2),
      var_label = rep("All Patients", 2),
      row_type = rep("content", 2),
      stringsAsFactors = FALSE
    ),
    hr = data.frame(
      arm = " ",
      n_tot = 268,
      hr = 0.8412573,
      lcl = 0.6231147,
      ucl = 1.1357683,
      conf_level = 0.95,
      pval = 0.25844564,
      pval_label = "p-value (log-rank)",
      subgroup = "All Patients",
      var = "ALL",
      var_label = "All Patients",
      row_type = "content",
      stringsAsFactors = FALSE
    )
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("a_survival_subgroups functions as expected with valid input", {

  df <- data.frame(
    hr = c(0.1234, 0.5678),
    pval = c(0.00001, 1.302309),
    subgroup = c("M", "F"),
    stringsAsFactors = FALSE
  )

  afun <- a_survival_subgroups(.formats = list("hr" = "xx.xx", pval = "x.xxxx | (<0.0001)"))

  result <- basic_table() %>%
    split_cols_by_multivar(c("hr", "pval")) %>%
    analyze_colvars(afun) %>%
    build_table(df)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "M", "F", "hr", "0.12", "0.57", "pval", "<0.0001", "1.3023"),
    .Dim = c(3L, 3L)
  )
  expect_equal(result_matrix, expected_matrix)

})

test_that("tabulate_survival_subgroups functions as expected with valid input", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Categorical Level Biomarker 2",
      "LOW", "MEDIUM", "HIGH", " ", "Total n", "268", "", "161", "107",
      "", "95", "93", "80", "B: Placebo", "n", "134", "", "82", "52",
      "", "45", "56", "33", "B: Placebo", "Median (DAYS)", "813.6",
      "", "676.1", "1180.4", "", "818.6", "971.3", "476.9", "A: Drug X",
      "n", "134", "", "79", "55", "", "50", "37", "47", "A: Drug X",
      "Median (DAYS)", "1010.2", "", "987.2", "1062.6", "", "965.8",
      "1474.7", "956", " ", "Hazard Ratio", "0.84", "", "0.65", "1.23",
      "", "1.06", "0.61", "0.77", " ", "95% Wald CI", "(0.62, 1.14)",
      "", "(0.44, 0.97)", "(0.76, 1.98)", "", "(0.64, 1.76)", "(0.34, 1.07)",
      "(0.45, 1.32)"
    ),
    .Dim = c(10L, 8L)
  )
  expect_equal(result_matrix, expected_matrix)

})

test_that("tabulate_survival_subgroups functions as expected with NULL subgroups", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", " ", "Total n", "268", "B: Placebo",
      "n", "134", "B: Placebo", "Median (DAYS)", "813.6", "A: Drug X",
      "n", "134", "A: Drug X", "Median (DAYS)", "1010.2", " ", "Hazard Ratio",
      "0.84", " ", "95% Wald CI", "(0.62, 1.14)"
    ),
  .Dim = c(3L, 8L)
  )

  expect_equal(result_matrix, expected_matrix)
})

test_that("tabulate_survival_subgroups functions as expected with extreme values in subgroups", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte() %>%
    filter(COUNTRY %in% c("RUS", "GBR")) %>%
    reapply_varlabels(var_labels(radtte(cached = TRUE)))

  df <- expect_warning(extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "COUNTRY"),
    data = adtte
  ))

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Country", "RUS", "GBR",
      " ", "Total n", "20", "", "13", "7", "B: Placebo", "n", "11",
      "", "8", "3", "B: Placebo", "Median (DAYS)", "533.3", "", "375.5",
      "NA", "A: Drug X", "n", "9", "", "5", "4", "A: Drug X", "Median (DAYS)",
      "1541.3", "", "NA", "1541.3", " ", "Hazard Ratio", "0.13", "",
      "0.14", "<0.01", " ", "95% Wald CI", "(0.02, 1.07)", "", "(0.02, 1.20)",
      "(0.00, >999.99)"
    ),
    .Dim = c(6L, 8L)
  )

  expect_equal(result_matrix, expected_matrix)

})

test_that("tabulate_survival_subgroups functions as expected when one arm has 0 records", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte()

  df <- expect_warning(extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "RACE"),
    data = adtte
  ))

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot", "n", "n_events", "median", "hr", "ci", "pval"),
      time_unit = adtte$AVALU[1]
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Race", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      " ", "Total n", "268", "", "135", "59", "53", "19", "1", "1",
      "B: Placebo", "n", "134", "", "67", "28", "26", "11", "1", "1",
      "B: Placebo", "Events", "92", "", "45", "21", "16", "8", "1",
      "1", "B: Placebo", "Median (DAYS)", "813.6", "", "931.7", "676.1",
      "906.4", "1236.5", "4743.5", "280.3", "A: Drug X", "n", "134",
      "", "68", "31", "27", "8", "0", "0", "A: Drug X", "Events", "81",
      "", "45", "17", "14", "5", "NA", "NA", "A: Drug X", "Median (DAYS)",
      "1010.2", "", "987.2", "1274.8", "1062.6", "965.8", "NA", "NA",
      " ", "Hazard Ratio", "0.84", "", "0.96", "0.54", "0.56", "1.59",
      "NA", "NA", " ", "95% Wald CI", "(0.62, 1.14)", "", "(0.63, 1.45)",
      "(0.28, 1.05)", "(0.26, 1.19)", "(0.48, 5.31)", "(NA, NA)", "(NA, NA)",
      " ", "p-value (log-rank)", "0.2584", "", "0.8299", "0.0675",
      "0.1263", "0.4488", "NA", "NA"
    ),
    .Dim = 10:11
  )
  expect_equal(result_matrix, expected_matrix)

})

test_that("d_survival_subgroups_colvars functions as expected with valid input", {

  vars <- c("n", "n_events", "median", "n_tot", "hr", "ci", "pval")

  result <- d_survival_subgroups_colvars(
    vars = vars,
    conf_level = 0.9,
    method = "p-value (log-rank)",
    time_unit = "Months"
  )

  expected <- list(
    vars = c("n", "n_events", "median", "n_tot", "hr", "lcl", "pval"),
    labels = c(
      n = "n",
      n_events = "Events",
      median = "Median (Months)",
      n_tot = "Total n",
      hr = "Hazard Ratio",
      ci = "90% Wald CI",
      pval = "p-value (log-rank)"
    )
  )

  expect_equal(result, expected)

})
