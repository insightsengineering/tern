# Tests DMT01

library(scda)
library(dplyr)
library(rtables)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
advs <- synthetic_cdisc_data("rcd_2022_02_28")$advs


testthat::test_that("DMT01 default and alternative variants are produced correctly", {

  # Change description in variable SEX.
  adsl <- adsl %>%
    dplyr::mutate(SEX = factor(dplyr::case_when(
      SEX == "M" ~ "Male",
      SEX == "F" ~ "Female",
      SEX == "U" ~ "Unknown",
      SEX == "UNDIFFERENTIATED" ~ "Undifferentiated"
    )))

  adsl <- adsl %>%
    dplyr::mutate(AGEGRP = factor(dplyr::case_when(
      dplyr::between(AGE, 18, 40) ~ "18-40",
      dplyr::between(AGE, 41, 64) ~ "41-64",
      AGE > 64 ~ ">=65"
    ),
    levels = c("18-40", "41-64", ">=65")
    ))


  # Obtain SBP, DBP and weight.

  get_param_advs <- function(pname, plabel) {
    ds <- advs %>%
      dplyr::filter(PARAM == plabel & AVISIT == "BASELINE") %>%
      dplyr::select(USUBJID, AVAL)

    colnames(ds) <- c("USUBJID", pname)

    ds
  }

  adsl <- adsl %>%
    dplyr::inner_join(get_param_advs("SBP", "Systolic Blood Pressure")) %>%
    dplyr::inner_join(get_param_advs("DBP", "Diastolic Blood Pressure")) %>%
    dplyr::inner_join(get_param_advs("WGT", "Weight"))

  vars <- c("AGE", "AGEGRP", "SEX", "RACE", "WGT", "SBP", "DBP")
  var_labels <- c(
    "Age (yr)",
    "Age group (yr)",
    "Sex",
    "Race",
    "Weight (kg) at Baseline",
    "Systolic Blood Pressure at Baseline",
    "Diastolic Blood Pressure at Baseline"
  )

  # Default variant for DMT01.
  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars(
      vars = vars,
      var_labels = var_labels
    ) %>%
    build_table(adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Age (yr)", "n", "Mean (SD)", "Median", "Min - Max",
      "Age group (yr)", "n", "18-40", "41-64", ">=65", "Sex", "n",
      "Female", "Male", "Race", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "OTHER", "UNKNOWN", "Weight (kg) at Baseline", "n", "Mean (SD)",
      "Median", "Min - Max", "Systolic Blood Pressure at Baseline",
      "n", "Mean (SD)", "Median", "Min - Max", "Diastolic Blood Pressure at Baseline",
      "n", "Mean (SD)", "Median", "Min - Max", "A: Drug X", "(N=134)",
      "", "134", "33.8 (6.6)", "33.0", "21.0 - 50.0", "", "134", "113 (84.3%)",
      "21 (15.7%)", "0", "", "134", "79 (59%)", "55 (41%)", "",
      "134", "68 (50.7%)", "31 (23.1%)", "27 (20.1%)", "8 (6%)", "0",
      "0", "0", "0", "", "134", "50.7 (7.9)", "49.8",
      "33.3 - 71.9", "", "134", "49.4 (8.5)", "48.3", "24.3 - 71.1",
      "", "134", "48.6 (8.0)", "48.4", "27.7 - 64.6", "B: Placebo", "(N=134)",
      "", "134", "35.4 (7.9)", "35.0", "21.0 - 62.0", "", "134", "103 (76.9%)",
      "31 (23.1%)", "0", "", "134", "82 (61.2%)", "52 (38.8%)",
      "", "134", "67 (50%)", "28 (20.9%)", "26 (19.4%)", "11 (8.2%)",
      "1 (0.7%)", "1 (0.7%)", "0", "0", "", "134", "49.9 (8.3)",
      "50.7", "27.9 - 69.8", "", "134", "50.2 (8.5)", "50.0", "24.4 - 71.1",
      "", "134", "50.4 (7.9)", "50.2", "21.7 - 67.5", "C: Combination",
      "(N=132)", "", "132", "35.4 (7.7)", "35.0", "20.0 - 69.0", "", "132",
      "106 (80.3%)", "25 (18.9%)", "1 (0.8%)", "", "132", "70 (53%)",
      "62 (47%)", "", "132", "73 (55.3%)", "32 (24.2%)", "21 (15.9%)",
      "6 (4.5%)", "0", "0", "0", "0", "", "132",
      "49.4 (7.5)", "49.6", "29.4 - 65.1", "", "132", "48.5 (7.2)",
      "49.2", "26.2 - 63.4", "", "132", "51.1 (7.8)", "50.8", "29.7 - 71.4",
      "All Patients", "(N=400)", "", "400", "34.9 (7.4)", "34.0", "20.0 - 69.0",
      "", "400", "322 (80.5%)", "77 (19.2%)", "1 (0.2%)", "", "400",
      "231 (57.8%)", "169 (42.2%)", "", "400", "208 (52%)", "91 (22.8%)",
      "74 (18.5%)", "25 (6.2%)", "1 (0.2%)", "1 (0.2%)", "0",
      "0", "", "400", "50.0 (7.9)", "50.1", "27.9 - 71.9", "", "400",
      "49.4 (8.1)", "49.2", "24.3 - 71.1", "", "400", "50.0 (8.0)", "50.0",
      "21.7 - 71.4"
    ),
    .Dim = c(41L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)

  # Alternative variant for DMT01 (Optional Subgrouping Analysis).
  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("SEX") %>%
    summarize_vars(vars = vars[4:7], var_labels = var_labels[4:7]) %>%
    summarize_vars(vars = vars[1:3], var_labels = var_labels[1:3], nested = FALSE) %>%
    build_table(adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Female", "Race", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "OTHER", "UNKNOWN", "Weight (kg) at Baseline", "n", "Mean (SD)",
      "Median", "Min - Max", "Systolic Blood Pressure at Baseline",
      "n", "Mean (SD)", "Median", "Min - Max", "Diastolic Blood Pressure at Baseline",
      "n", "Mean (SD)", "Median", "Min - Max", "Male", "Race", "n",
      "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
      "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER",
      "UNKNOWN", "Weight (kg) at Baseline", "n", "Mean (SD)", "Median",
      "Min - Max", "Systolic Blood Pressure at Baseline", "n", "Mean (SD)",
      "Median", "Min - Max", "Diastolic Blood Pressure at Baseline",
      "n", "Mean (SD)", "Median", "Min - Max", "Age (yr)", "n", "Mean (SD)",
      "Median", "Min - Max", "Age group (yr)", "n", "18-40", "41-64",
      ">=65", "Sex", "n", "Female", "Male", "A: Drug X", "(N=134)",
      "", "", "79", "41 (51.9%)", "18 (22.8%)", "17 (21.5%)", "3 (3.8%)",
      "0", "0", "0", "0", "", "79", "50.3 (7.0)",
      "50.1", "35.9 - 69.9", "", "79", "49.7 (7.9)", "48.7", "25.4 - 71.1",
      "", "79", "48.4 (8.8)", "48.2", "27.7 - 64.6", "", "", "55",
      "27 (49.1%)", "13 (23.6%)", "10 (18.2%)", "5 (9.1%)", "0",
      "0", "0", "0", "", "55", "51.3 (8.9)", "49.4",
      "33.3 - 71.9", "", "55", "49.0 (9.3)", "47.9", "24.3 - 69.2", "",
      "55", "49.0 (6.7)", "48.5", "31.5 - 64.0", "", "134", "33.8 (6.6)",
      "33.0", "21.0 - 50.0", "", "134", "113 (84.3%)", "21 (15.7%)", "0",
      "", "134", "79 (59%)", "55 (41%)", "B: Placebo", "(N=134)", "",
      "", "82", "40 (48.8%)", "16 (19.5%)", "18 (22%)", "7 (8.5%)",
      "0", "1 (1.2%)", "0", "0", "", "82", "50.0 (7.8)",
      "50.7", "27.9 - 68.9", "", "82", "49.6 (8.6)", "49.0", "24.4 - 68.8",
      "", "82", "50.5 (7.8)", "49.9", "35.1 - 67.5", "", "", "52",
      "27 (51.9%)", "12 (23.1%)", "8 (15.4%)", "4 (7.7%)", "1 (1.9%)",
      "0", "0", "0", "", "52", "49.7 (9.2)", "50.8",
      "30.5 - 69.8", "", "52", "51.3 (8.4)", "51.2", "34.0 - 71.1", "",
      "52", "50.4 (8.2)", "50.4", "21.7 - 63.3", "", "134", "35.4 (7.9)",
      "35.0", "21.0 - 62.0", "", "134", "103 (76.9%)", "31 (23.1%)", "0",
      "", "134", "82 (61.2%)", "52 (38.8%)", "C: Combination", "(N=132)",
      "", "", "70", "39 (55.7%)", "16 (22.9%)", "11 (15.7%)", "4 (5.7%)",
      "0", "0", "0", "0", "", "70", "49.2 (7.5)",
      "49.6", "29.4 - 63.8", "", "70", "48.1 (7.1)", "49.1", "26.2 - 63.4",
      "", "70", "51.5 (7.8)", "50.6", "29.7 - 67.8", "", "", "62",
      "34 (54.8%)", "16 (25.8%)", "10 (16.1%)", "2 (3.2%)", "0",
      "0", "0", "0", "", "62", "49.6 (7.5)", "49.6",
      "35.5 - 65.1", "", "62", "49.0 (7.3)", "50.3", "30.0 - 61.6", "",
      "62", "50.7 (7.8)", "50.9", "30.8 - 71.4", "", "132", "35.4 (7.7)",
      "35.0", "20.0 - 69.0", "", "132", "106 (80.3%)", "25 (18.9%)", "1 (0.8%)",
      "", "132", "70 (53%)", "62 (47%)", "All Patients", "(N=400)",
      "", "", "231", "120 (51.9%)", "50 (21.6%)", "46 (19.9%)", "14 (6.1%)",
      "0", "1 (0.4%)", "0", "0", "", "231", "49.9 (7.4)",
      "50.3", "27.9 - 69.9", "", "231", "49.1 (7.9)", "49.1", "24.4 - 71.1",
      "", "231", "50.1 (8.2)", "49.9", "27.7 - 67.8", "", "", "169",
      "88 (52.1%)", "41 (24.3%)", "28 (16.6%)", "11 (6.5%)", "1 (0.6%)",
      "0", "0", "0", "", "169", "50.2 (8.5)", "49.6",
      "30.5 - 71.9", "", "169", "49.7 (8.4)", "50.1", "24.3 - 71.1",
      "", "169", "50.0 (7.6)", "50.1", "21.7 - 71.4", "", "400", "34.9 (7.4)",
      "34.0", "20.0 - 69.0", "", "400", "322 (80.5%)", "77 (19.2%)", "1 (0.2%)",
      "", "400", "231 (57.8%)", "169 (42.2%)"
    ),
    .Dim = c(68L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
