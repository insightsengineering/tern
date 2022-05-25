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
      AVALU = as.character(AVALU),
      is_event = CNSR == 0
    )

  reapply_varlabels(adtte_mod, adtte_labels, is_event = "Event Flag")
}

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("extract_survival_subgroups functions as expected with valid input and default arguments", {
  adtte <- adtte %>%
    preprocess_adtte()

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte
  )

  expected <- list(
    survtime = structure(
      list(
        arm = structure(
          c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
          .Label = c("B: Placebo", "A: Drug X"),
          class = "factor"
        ),
        n = c(134L, 134L, 82L, 79L, 52L, 55L, 45L, 50L, 56L, 37L, 33L, 47L),
        n_events = c(
          87L, 79L, 50L, 45L, 37L, 34L, 30L, 31L, 36L, 19L, 21L, 29L
        ),
        median = c(
          837.42801327648,
          1260.49053370248,
          850.920785514258,
          1274.80474338372,
          527.665885794264,
          849.297617184933,
          751.431436610118,
          1160.64578110184,
          722.792588842567,
          1269.40388857211,
          848.239273340441,
          1070.80218764022
        ),
        subgroup = c(
          "All Patients",
          "All Patients",
          "F",
          "F",
          "M",
          "M",
          "LOW",
          "LOW",
          "MEDIUM",
          "MEDIUM",
          "HIGH",
          "HIGH"
        ),
        var = c(
          "ALL",
          "ALL",
          "SEX",
          "SEX",
          "SEX",
          "SEX",
          "BMRKR2",
          "BMRKR2",
          "BMRKR2",
          "BMRKR2",
          "BMRKR2",
          "BMRKR2"
        ),
        var_label = c(
          "All Patients",
          "All Patients",
          "Sex",
          "Sex",
          "Sex",
          "Sex",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2"
        ),
        row_type = c(
          "content",
          "content",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis"
        )
      ),
      row.names = c(NA, -12L),
      class = "data.frame"
    ),
    hr = structure(
      list(
        arm = c(" ", " ", " ", " ", " ", " "),
        n_tot = c(268L, 161L, 107L, 95L, 93L, 80L),
        n_tot_events = c(166, 95, 71, 61, 55, 50),
        hr = c(
          0.717365051154891,
          0.697969331159471,
          0.783616674201674,
          0.705072968604656,
          0.572806884078014,
          0.976900177598778
        ),
        lcl = c(
          0.527523110746632,
          0.464781196048063,
          0.487344418692844,
          0.424365474268753,
          0.324419621563317,
          0.555200234313668
        ),
        ucl = c(
          0.975526201857015,
          1.04815167089682,
          1.26000230747263,
          1.17146167914251,
          1.01136831633695,
          1.71890049393127
        ),
        conf_level = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95),
        pval = c(
          0.0334029294775114,
          0.0814817359933965,
          0.313183467032327,
          0.17526198076925,
          0.0517494169527886,
          0.935389266684535
        ),
        pval_label = c(
          "p-value (log-rank)",
          "p-value (log-rank)",
          "p-value (log-rank)",
          "p-value (log-rank)",
          "p-value (log-rank)",
          "p-value (log-rank)"
        ),
        subgroup = c("All Patients", "F", "M", "LOW", "MEDIUM", "HIGH"),
        var = c("ALL", "SEX", "SEX", "BMRKR2", "BMRKR2", "BMRKR2"),
        var_label = c(
          "All Patients",
          "Sex",
          "Sex",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2",
          "Categorical Level Biomarker 2"
        ),
        row_type = c(
          "content",
          "analysis",
          "analysis",
          "analysis",
          "analysis",
          "analysis"
        )
      ),
      row.names = c(NA, -6L), class = "data.frame"
    )
  )
  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("extract_survival_subgroups works as expected with groups_lists", {
  adtte <- adtte %>%
    preprocess_adtte()

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adtte,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )

  survtime <- result$survtime
  testthat::expect_setequal(
    survtime[survtime$var == "BMRKR2", "subgroup"],
    c("low", "low/medium", "low/medium/high")
  )

  hr <- result$hr
  testthat::expect_setequal(
    hr[hr$var == "BMRKR2", "subgroup"],
    c("low", "low/medium", "low/medium/high")
  )
})

testthat::test_that("extract_survival_subgroups functions as expected with NULL subgroups", {
  adtte <- adtte %>%
    preprocess_adtte()

  result <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
    data = adtte
  )

  expected <-
    list(
      survtime = structure(
        list(
          arm = structure(
            1:2,
            .Label = c("B: Placebo", "A: Drug X"),
            class = "factor"
          ),
          n = c(134L, 134L),
          n_events = c(87L, 79L),
          median = c(837.42801327648, 1260.49053370248),
          subgroup = c("All Patients", "All Patients"),
          var = c("ALL", "ALL"),
          var_label = c("All Patients", "All Patients"),
          row_type = c("content", "content")
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      ),
      hr = structure(
        list(
          arm = " ",
          n_tot = 268L,
          n_tot_events = 166L,
          hr = 0.717365051154891,
          lcl = 0.527523110746632,
          ucl = 0.975526201857015,
          conf_level = 0.95,
          pval = 0.0334029294775114,
          pval_label = "p-value (log-rank)",
          subgroup = "All Patients",
          var = "ALL",
          var_label = "All Patients",
          row_type = "content"
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )
  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("a_survival_subgroups functions as expected with valid input", {
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
      "", "M", "F", "hr", "0.12", "0.57", "pval", "<0.0001", "1.3023"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_survival_subgroups functions as expected with valid input", {
  adtte <- adtte %>%
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
      " ", "Total Events", "166", "", "95", "71", "", "61", "55", "50",
      "B: Placebo", "Events", "87", "", "50", "37", "", "30", "36",
      "21", "B: Placebo", "Median (DAYS)", "837.4", "", "850.9", "527.7",
      "", "751.4", "722.8", "848.2", "A: Drug X", "Events", "79", "",
      "45", "34", "", "31", "19", "29", "A: Drug X", "Median (DAYS)",
      "1260.5", "", "1274.8", "849.3", "", "1160.6", "1269.4", "1070.8",
      " ", "Hazard Ratio", "0.72", "", "0.70", "0.78", "", "0.71",
      "0.57", "0.98", " ", "95% Wald CI", "(0.53, 0.98)", "", "(0.46, 1.05)",
      "(0.49, 1.26)", "", "(0.42, 1.17)", "(0.32, 1.01)", "(0.56, 1.72)"
    ),
    .Dim = c(10L, 8L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_survival_subgroups functions as expected with NULL subgroups", {
  adtte <- adtte %>%
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
      "Total Events", "166", "B: Placebo", "Events", "87", "B: Placebo",
      "Median (DAYS)", "837.4", "A: Drug X", "Events", "79", "A: Drug X",
      "Median (DAYS)", "1260.5", " ", "Hazard Ratio", "0.72", " ",
      "95% Wald CI", "(0.53, 0.98)"
    ),
    .Dim = c(3L, 8L)
  )

  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_survival_subgroups functions as expected with extreme values in subgroups", {
  adtte <- adtte %>%
    preprocess_adtte() %>%
    dplyr::slice(1:30) %>%
    reapply_varlabels(formatters::var_labels(adtte))

  df <- testthat::expect_warning(extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "REGION1"),
    data = adtte
  ))

  result <- basic_table() %>%
    tabulate_survival_subgroups(df, time_unit = adtte$AVALU[1])

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Geographic Region 1",
      "Asia", "North America", "South America", " ", "Total Events",
      "18", "", "4", "2", "12", "B: Placebo", "Events", "5", "", "2",
      "0", "3", "B: Placebo", "Median (DAYS)", "742.2", "", "176.4",
      "NA", "837.4", "A: Drug X", "Events", "13", "", "2", "2", "9",
      "A: Drug X", "Median (DAYS)", "985.4", "", "985.4", "242.7",
      "1759.9", " ", "Hazard Ratio", "0.92", "", "<0.01", ">999.99",
      "1.10", " ", "95% Wald CI", "(0.31, 2.76)", "", "(0.00, >999.99)",
      "(0.00, >999.99)", "(0.27, 4.43)"
    ),
    .Dim = 7:8
  )

  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_survival_subgroups functions as expected when one arm has 0 records", {
  adtte <- adtte %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "RACE"),
    data = adtte
  )

  result <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("n_tot_events", "n", "n_events", "median", "hr", "ci", "pval"),
      time_unit = adtte$AVALU[1]
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Race",
      "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
      "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", " ",
      "Total Events", "166", "", "82", "35", "35", "12", "1", "1",
      "B: Placebo", "n", "134", "", "67", "28", "26", "11", "1", "1",
      "B: Placebo", "Events", "87", "", "40", "20", "19", "6", "1",
      "1", "B: Placebo", "Median (DAYS)", "837.4", "", "906.0", "751.4",
      "841.2", "741.9", "33.6", "153.5", "A: Drug X", "n", "134", "",
      "68", "31", "27", "8", "0", "0", "A: Drug X", "Events", "79",
      "", "42", "15", "16", "6", "NA", "NA", "A: Drug X", "Median (DAYS)",
      "1260.5", "", "1274.8", "1327.8", "774.7", "849.3", "NA", "NA",
      " ", "Hazard Ratio", "0.72", "", "0.75", "0.63", "0.69", "1.01",
      "NA", "NA", " ", "95% Wald CI", "(0.53, 0.98)", "", "(0.48, 1.17)",
      "(0.32, 1.25)", "(0.35, 1.35)", "(0.28, 3.61)", "(NA, NA)", "(NA, NA)",
      " ", "p-value (log-rank)", "0.0334", "", "0.2012", "0.1832",
      "0.2745", "0.9839", "NA", "NA"
    ),
    .Dim = 10:11
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_survival_subgroups works correctly with both `n_tot` and `n_tot_events` in `vars`", {
  adtte <- adtte %>%
    preprocess_adtte()

  df <- extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = "RACE"),
    data = adtte
  )

  # Both n_tot variables, but no surv time vars.
  result_both <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("hr", "ci", "n_tot", "pval", "n_tot_events"),
      time_unit = adtte$AVALU[1]
    )
  # Check that the column indices attributes are correct.
  expected_cols_both <- list(col_x = 1L, col_ci = 2L, col_symbol_size = 3L)
  result_cols_both <- attributes(result_both)[c("col_x", "col_ci", "col_symbol_size")]
  testthat::expect_identical(result_cols_both, expected_cols_both)

  # Both n_tot variables and also surv time vars, so we have a reordering of the vars in the table.
  result_both_survtime <- basic_table() %>%
    tabulate_survival_subgroups(
      df = df,
      vars = c("hr", "median", "n_events", "ci", "pval", "n_tot_events", "n", "n_tot"),
      time_unit = adtte$AVALU[1]
    )
  # Check that the column indices attributes are correct.
  expected_cols_both_survtime <- list(col_x = 9L, col_ci = 10L, col_symbol_size = 1L)
  result_cols_both_survtime <- attributes(result_both_survtime)[c("col_x", "col_ci", "col_symbol_size")]
  testthat::expect_identical(result_cols_both_survtime, expected_cols_both_survtime)
  # Check header of table.
  result_header_both_survtime <- to_string_matrix(result_both_survtime)[2, ]
  expected_header_both_survtime <- c(
    "", "Total Events", "Total n", "Median (DAYS)", "Events", "n",
    "Median (DAYS)", "Events", "n", "Hazard Ratio", "95% Wald CI",
    "p-value (log-rank)"
  )
  testthat::expect_identical(result_header_both_survtime, expected_header_both_survtime)
})

testthat::test_that("d_survival_subgroups_colvars functions as expected with valid input", {
  vars <- c("n", "n_events", "median", "n_tot_events", "hr", "ci", "pval")

  result <- d_survival_subgroups_colvars(
    vars = vars,
    conf_level = 0.9,
    method = "p-value (log-rank)",
    time_unit = "Months"
  )

  expected <- list(
    vars = c("n", "n_events", "median", "n_tot_events", "hr", "lcl", "pval"),
    labels = c(
      n = "n",
      n_events = "Events",
      median = "Median (Months)",
      n_tot_events = "Total Events",
      hr = "Hazard Ratio",
      ci = "90% Wald CI",
      pval = "p-value (log-rank)"
    )
  )

  testthat::expect_equal(result, expected)
})
