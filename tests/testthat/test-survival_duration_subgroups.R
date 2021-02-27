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
      n_events = c(87L, 79L, 50L, 45L, 37L, 34L, 30L, 31L, 36L, 19L, 21L, 29L),
      median = c(
        837.42801327648, 1260.49053370248, 850.920785514258, 1274.80474338372,
        527.665885794264, 849.297617184933, 751.431436610118, 1160.64578110184,
        722.792588842567, 1269.40388857211, 848.239273340441, 1070.80218764022
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
      hr = c(
        0.71736505115489, 0.697969331159471, 0.783616674201674, 0.705072968604656,
        0.572806884078014, 0.976900177598777
      ),
      lcl = c(
        0.527523110746632, 0.464781196048063, 0.487344418692843, 0.424365474268753,
        0.324419621563317, 0.555200234313668
      ),
      ucl = c(
        0.975526201857014, 1.04815167089682, 1.26000230747263, 1.17146167914251,
        1.01136831633695, 1.71890049393127
      ),
      conf_level = 0.95,
      pval = c(
        0.0334029294775113, 0.0814817359933963, 0.313183467032326,
        0.17526198076925, 0.0517494169527888, 0.935389266684535
      ),
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
      n_events = c(87L, 79L),
      median = c(837.42801327648, 1260.49053370248),
      subgroup = rep("All Patients", 2),
      var = rep("ALL", 2),
      var_label = rep("All Patients", 2),
      row_type = rep("content", 2),
      stringsAsFactors = FALSE
    ),
    hr = data.frame(
      arm = " ",
      n_tot = 268,
      hr = 0.71736505115489,
      lcl = 0.527523110746632,
      ucl = 0.975526201857014,
      conf_level = 0.95,
      pval = 0.0334029294775113,
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
      "Baseline Risk Factors", "", "All Patients", "Sex",
      "F", "M", "Categorical Level Biomarker 2", "LOW", "MEDIUM", "HIGH",
      " ", "Total n", "268", "", "161", "107", "", "95", "93", "80",
      "B: Placebo", "n", "134", "", "82", "52", "", "45", "56", "33",
      "B: Placebo", "Median (DAYS)", "837.4", "", "850.9", "527.7",
      "", "751.4", "722.8", "848.2", "A: Drug X", "n", "134", "", "79",
      "55", "", "50", "37", "47", "A: Drug X", "Median (DAYS)", "1260.5",
      "", "1274.8", "849.3", "", "1160.6", "1269.4", "1070.8", " ",
      "Hazard Ratio", "0.72", "", "0.70", "0.78", "", "0.71", "0.57",
      "0.98", " ", "95% Wald CI", "(0.53, 0.98)", "", "(0.46, 1.05)",
      "(0.49, 1.26)", "", "(0.42, 1.17)", "(0.32, 1.01)", "(0.56, 1.72)"
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
      "Baseline Risk Factors", "", "All Patients", " ",
      "Total n", "268", "B: Placebo", "n", "134", "B: Placebo", "Median (DAYS)",
      "837.4", "A: Drug X", "n", "134", "A: Drug X", "Median (DAYS)",
      "1260.5", " ", "Hazard Ratio", "0.72", " ", "95% Wald CI", "(0.53, 0.98)"
    ),
    .Dim = c(3L, 8L)
  )

  expect_equal(result_matrix, expected_matrix)
})

test_that("tabulate_survival_subgroups functions as expected with extreme values in subgroups", {

  adtte <- radtte(cached = TRUE) %>%
    preprocess_adtte() %>%
    slice(1:30) %>%
    reapply_varlabels(var_labels(radtte(cached = TRUE)))

  df <- expect_warning(extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "REGION1"),
    data = adtte
  ))

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Geographic Region 1",
      "Asia", "North America", "South America", " ", "Total n", "30",
      "", "5", "5", "20", "B: Placebo", "n", "11", "", "2", "2", "7",
      "B: Placebo", "Median (DAYS)", "742.2", "", "176.4", "NA", "837.4",
      "A: Drug X", "n", "19", "", "3", "3", "13", "A: Drug X", "Median (DAYS)",
      "985.4", "", "985.4", "242.7", "1759.9", " ", "Hazard Ratio",
      "0.92", "", "<0.01", ">999.99", "1.10", " ", "95% Wald CI", "(0.31, 2.76)",
      "", "(0.00, >999.99)", "(0.00, >999.99)", "(0.27, 4.43)"
    ),
    .Dim = 7:8
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
      "Baseline Risk Factors", "", "All Patients", "Race",
      "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
      "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", " ",
      "Total n", "268", "", "135", "59", "53", "19", "1", "1", "B: Placebo",
      "n", "134", "", "67", "28", "26", "11", "1", "1", "B: Placebo",
      "Events", "87", "", "40", "20", "19", "6", "1", "1", "B: Placebo",
      "Median (DAYS)", "837.4", "", "906", "751.4", "841.2", "741.9",
      "33.6", "153.5", "A: Drug X", "n", "134", "", "68", "31", "27",
      "8", "0", "0", "A: Drug X", "Events", "79", "", "42", "15", "16",
      "6", "NA", "NA", "A: Drug X", "Median (DAYS)", "1260.5", "",
      "1274.8", "1327.8", "774.7", "849.3", "NA", "NA", " ", "Hazard Ratio",
      "0.72", "", "0.75", "0.63", "0.69", "1.01", "NA", "NA", " ",
      "95% Wald CI", "(0.53, 0.98)", "", "(0.48, 1.17)", "(0.32, 1.25)",
      "(0.35, 1.35)", "(0.28, 3.61)", "(NA, NA)", "(NA, NA)", " ",
      "p-value (log-rank)", "0.0334", "", "0.2012", "0.1832", "0.2745",
      "0.9839", "NA", "NA"
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
