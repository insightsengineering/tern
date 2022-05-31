# Test the single variant for VST01

library(scda)
library(rtables)
library(dplyr)

gen_advs <- function() {
  advs <- synthetic_cdisc_data("rcd_2022_02_28")$advs # nolintr
  advs_label <- formatters::var_labels(advs)

  advs <- advs %>%
    dplyr::filter(PARAMCD == "DIABP" & PARAM == "Diastolic Blood Pressure") %>%
    dplyr::mutate(PARAMCD = droplevels(PARAMCD), PARAM = droplevels(PARAM)) # nolint

  # post-baseline
  advs_pb <- advs %>% dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") # nolint

  advs_pb_max <- advs_pb %>%
    dplyr::group_by(PARAM, USUBJID) %>%
    dplyr::arrange(dplyr::desc(AVAL)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AVISIT = "Post-Baseline Maximum")

  advs_pb_min <- advs_pb %>%
    dplyr::group_by(PARAM, USUBJID) %>%
    dplyr::arrange(AVAL) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AVISIT = "Post-Baseline Minimum")

  advs_pb_last <- advs_pb %>%
    dplyr::group_by(PARAM, USUBJID) %>%
    dplyr::arrange(dplyr::desc(AVISITN)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AVISIT = "Post-Baseline Last")

  advs_f <- rbind(
    advs,
    advs_pb_last,
    advs_pb_min,
    advs_pb_max
  )

  formatters::var_labels(advs_f) <- advs_label
  advs_f <- advs_f %>% dplyr::mutate(AVISIT = droplevels(AVISIT)) # nolint
  advs_f
}

testthat::test_that("VST01 default variant is produced correctly", {
  skip_if_too_deep(3)

  advs <- gen_advs()
  advs_baseline <- advs %>% dplyr::filter(ABLFL == "Y") # nolint
  df_adsl <- unique(advs[c("USUBJID", "ARM")])

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value at Visit", "Change from Baseline")
    ) %>%
    summarize_colvars(.labels = c(range = "Min - Max")) %>%
    build_table(df = advs, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "SCREENING", "n", "Mean (SD)", "Median",
      "Min - Max", "BASELINE", "n", "Mean (SD)", "Median", "Min - Max",
      "WEEK 1 DAY 8", "n", "Mean (SD)", "Median", "Min - Max", "WEEK 2 DAY 15",
      "n", "Mean (SD)", "Median", "Min - Max", "WEEK 3 DAY 22", "n",
      "Mean (SD)", "Median", "Min - Max", "WEEK 4 DAY 29", "n", "Mean (SD)",
      "Median", "Min - Max", "WEEK 5 DAY 36", "n", "Mean (SD)", "Median",
      "Min - Max", "Post-Baseline Last", "n", "Mean (SD)", "Median",
      "Min - Max", "Post-Baseline Minimum", "n", "Mean (SD)", "Median",
      "Min - Max", "Post-Baseline Maximum", "n", "Mean (SD)", "Median",
      "Min - Max", "A: Drug X", "Value at Visit", "(N=134)", "", "134",
      "50.0 (7.2)", "49.7", "31.7 - 71.2", "", "134", "48.6 (8.0)", "48.4",
      "27.7 - 64.6", "", "134", "50.3 (7.5)", "50.1", "33.0 - 69.0", "",
      "134", "50.8 (7.8)", "51.4", "31.9 - 70.3", "", "134", "50.7 (7.8)",
      "50.2", "29.1 - 74.9", "", "134", "50.1 (8.1)", "49.3", "29.4 - 71.4",
      "", "134", "50.6 (7.5)", "49.4", "35.1 - 72.2", "", "134", "50.6 (7.5)",
      "49.4", "35.1 - 72.2", "", "134", "41.6 (4.8)", "42.1", "29.1 - 54.2",
      "", "134", "60.0 (4.9)", "59.7", "47.6 - 74.9", "A: Drug X", "Change from Baseline",
      "(N=134)", "", "0", "NA (NA)", "NA", "NA - NA", "", "134", "0.0 (0.0)",
      "0.0", "0.0 - 0.0", "", "134", "1.7 (10.8)", "0.6", "-20.4 - 29.9",
      "", "134", "2.2 (11.8)", "3.1", "-23.9 - 39.2", "", "134", "2.1 (10.9)",
      "2.6", "-25.8 - 30.7", "", "134", "1.5 (10.9)", "1.2", "-30.9 - 24.8",
      "", "134", "2.0 (11.2)", "1.6", "-25.2 - 31.5", "", "134", "2.0 (11.2)",
      "1.6", "-25.2 - 31.5", "", "134", "-7.0 (9.6)", "-5.6", "-30.9 - 13.2",
      "", "134", "11.4 (9.3)", "11.7", "-8.6 - 39.2", "B: Placebo",
      "Value at Visit", "(N=134)", "", "134", "50.8 (8.4)", "50.1",
      "29.3 - 69.2", "", "134", "50.4 (7.9)", "50.2", "21.7 - 67.5",
      "", "134", "49.7 (7.7)", "49.7", "33.7 - 66.5", "", "134", "49.7 (8.4)",
      "50.2", "30.6 - 68.1", "", "134", "49.1 (7.9)", "49.5", "24.5 - 67.1",
      "", "134", "49.6 (7.1)", "49.2", "32.7 - 67.0", "", "134", "48.4 (8.0)",
      "47.7", "30.4 - 70.6", "", "134", "48.4 (8.0)", "47.7", "30.4 - 70.6",
      "", "134", "40.3 (5.3)", "40.3", "24.5 - 55.4", "", "134", "58.5 (4.8)",
      "58.3", "47.8 - 70.6", "B: Placebo", "Change from Baseline",
      "(N=134)", "", "0", "NA (NA)", "NA", "NA - NA", "", "134", "0.0 (0.0)",
      "0.0", "0.0 - 0.0", "", "134", "-0.8 (10.9)", "-1.1", "-25.8 - 28.4",
      "", "134", "-0.7 (12.4)", "-0.7", "-28.4 - 37.0", "", "134", "-1.3 (11.1)",
      "-2.3", "-28.1 - 33.9", "", "134", "-0.8 (10.6)", "-0.8", "-33.2 - 35.9",
      "", "134", "-2.1 (11.7)", "-2.9", "-31.2 - 29.2", "", "134",
      "-2.1 (11.7)", "-2.9", "-31.2 - 29.2", "", "134", "-10.1 (10.2)",
      "-10.7", "-33.2 - 27.7", "", "134", "8.1 (9.5)", "7.5", "-14.0 - 37.0",
      "C: Combination", "Value at Visit", "(N=132)", "", "132", "50.2 (7.6)",
      "49.6", "26.9 - 70.0", "", "132", "51.1 (7.8)", "50.8", "29.7 - 71.4",
      "", "132", "48.9 (7.9)", "47.7", "30.4 - 67.0", "", "132", "50.0 (8.3)",
      "51.0", "24.8 - 65.6", "", "132", "49.9 (7.7)", "50.0", "31.5 - 68.5",
      "", "132", "49.7 (8.0)", "49.8", "28.7 - 68.4", "", "132", "49.1 (7.6)",
      "49.2", "30.6 - 72.9", "", "132", "49.1 (7.6)", "49.2", "30.6 - 72.9",
      "", "132", "40.5 (5.3)", "40.5", "24.8 - 54.9", "", "132", "58.6 (4.8)",
      "59.0", "41.8 - 72.9", "C: Combination", "Change from Baseline",
      "(N=132)", "", "0", "NA (NA)", "NA", "NA - NA", "", "132", "0.0 (0.0)",
      "0.0", "0.0 - 0.0", "", "132", "-2.3 (10.4)", "-2.2", "-22.1 - 22.4",
      "", "132", "-1.1 (11.6)", "-2.3", "-29.2 - 30.7", "", "132",
      "-1.2 (10.9)", "-0.6", "-29.7 - 27.0", "", "132", "-1.4 (11.0)",
      "-0.2", "-35.5 - 28.4", "", "132", "-2.0 (10.7)", "-2.2", "-30.5 - 34.6",
      "", "132", "-2.0 (10.7)", "-2.2", "-30.5 - 34.6", "", "132", "-10.6 (9.1)",
      "-10.9", "-35.5 - 13.7", "", "132", "7.5 (9.1)", "6.8", "-17.3 - 34.6"
    ),
    .Dim = c(53L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
