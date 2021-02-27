# Tests the variants for TTET01.

library(random.cdisc.data)
library(dplyr)

test_that("TTET01 default variant is produced correctly", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Patients with event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients without event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank"),
      table_names = "coxph_unstratified"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      time_point = c(6, 12),
      is_event = "is_event",
      method = "both",
      control = control_surv_timepoint()
    )

  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio",
      "95% CI", "6 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "Difference in Event Free Rate", "95% CI", "p-value (Z-test)",
      "12 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "Difference in Event Free Rate", "95% CI", "p-value (Z-test)",
      "A: Drug X", "(N=134)", "79 (59%)", "15", "14", "18", "17", "15",
      "55 (41%)", "", "41.41", "(27.7, 54.7)", "15.4, 75.2", "0.4 to 154.7",
      "0.3 to 116.4", "", "", "", "", "", "106", "83.83", "(77.49, 90.17)",
      "", "", "", "", "92", "78.03", "(70.82, 85.24)", "", "", "",
      "B: Placebo", "(N=134)", "87 (64.9%)", "14", "20", "16", "15",
      "22", "47 (35.1%)", "", "27.51", "(17.3, 30.2)", "9.5, 54.9",
      "0.9 to 91", "0 to 122.4", "", "0.0334", "1.394", "(1.0251, 1.8957)",
      "", "112", "89.16", "(83.8, 94.53)", "5.33", "(-2.97, 13.64)",
      "0.2080", "", "83", "70.32", "(62.27, 78.37)", "-7.71", "(-18.51, 3.1)",
      "0.1622", "C: Combination", "(N=132)", "116 (87.9%)", "21", "23",
      "24", "27", "21", "16 (12.1%)", "", "11.08", "(9.6, 15.9)", "5.3, 25.2",
      "0.3 to 49.4", "0.1 to 101.6", "", "<0.0001", "2.7532", "(2.0478, 3.7017)",
      "", "92", "73.4", "(65.72, 81.07)", "-10.43", "(-20.38, -0.48)",
      "0.0399", "", "56", "46.39", "(37.59, 55.18)", "-31.64", "(-43.01, -20.26)",
      "<0.0001"
    ),
    .Dim = c(33L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("TTET01 variant: selecting sections to display", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )


  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Patients with event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients without event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank"),
      table_names = "coxph_unstratified"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      is_event = "is_event",
      method = "surv",
      time_point = c(6, 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio",
      "95% CI", "6 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "12 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "A: Drug X", "(N=134)", "79 (59%)", "15", "14", "18",
      "17", "15", "55 (41%)", "", "41.41", "(27.7, 54.7)", "15.4, 75.2",
      "0.4 to 154.7", "0.3 to 116.4", "", "", "", "", "", "106", "83.83",
      "(77.49, 90.17)", "", "92", "78.03", "(70.82, 85.24)", "B: Placebo",
      "(N=134)", "87 (64.9%)", "14", "20", "16", "15", "22", "47 (35.1%)",
      "", "27.51", "(17.3, 30.2)", "9.5, 54.9", "0.9 to 91", "0 to 122.4",
      "", "0.0334", "1.394", "(1.0251, 1.8957)", "", "112", "89.16",
      "(83.8, 94.53)", "", "83", "70.32", "(62.27, 78.37)", "C: Combination",
      "(N=132)", "116 (87.9%)", "21", "23", "24", "27", "21", "16 (12.1%)",
      "", "11.08", "(9.6, 15.9)", "5.3, 25.2", "0.3 to 49.4", "0.1 to 101.6",
      "", "<0.0001", "2.7532", "(2.0478, 3.7017)", "", "92", "73.4",
      "(65.72, 81.07)", "", "56", "46.39", "(37.59, 55.18)"
    ),
    .Dim = c(27L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("TTET01 variant: modifying analysis details like conftype, ties, alpha level", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Patients with event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients without event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      control = control_surv_time(conf_level = 0.9, conf_type = "log-log"),
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank", conf_level = 0.95, ties = "efron"),
      table_names = "coxph_unstratified"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      is_event = "is_event",
      time_point = 12,
      control = control_surv_timepoint(conf_level = 0.9, conf_type = "log-log"),
      table_names_suffix = "_log_log"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      time_point = 12,
      method = "surv_diff",
      control = control_surv_timepoint(conf_level = 0.975),
      table_names_suffix = "_975_pct"
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "90% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio",
      "95% CI", "12 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "90% CI", "Difference in Event Free Rate", "97.5% CI", "p-value (Z-test)",
      "A: Drug X", "(N=134)", "79 (59%)", "15", "14", "18", "17", "15",
      "55 (41%)", "", "41.41", "(27.9, 54.1)", "15.4, 75.2", "0.4 to 154.7",
      "0.3 to 116.4", "", "", "", "", "", "92", "78.03", "(71.24, 83.4)",
      "", "", "", "B: Placebo", "(N=134)", "87 (64.9%)", "14", "20",
      "16", "15", "22", "47 (35.1%)", "", "27.51", "(18.8, 29.9)",
      "9.5, 54.9", "0.9 to 91", "0 to 122.4", "", "0.0334", "1.394",
      "(1.0251, 1.8957)", "", "83", "70.32", "(62.97, 76.49)", "-7.71",
      "(-20.07, 4.65)", "0.1622", "C: Combination", "(N=132)", "116 (87.9%)",
      "21", "23", "24", "27", "21", "16 (12.1%)", "", "11.08", "(9.7, 15.1)",
      "5.3, 25.2", "0.3 to 49.4", "0.1 to 101.6", "", "<0.0001", "2.7532",
      "(2.0478, 3.7017)", "", "56", "46.39", "(38.87, 53.56)", "-31.64",
      "(-44.64, -18.63)", "<0.0001"
    ),
    .Dim = c(26L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("TTET01 variant: with stratified analysis", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Patients with event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients without event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = "Unstratified Analysis",
      table_names = "coxph_unstratified"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = "Stratified Analysis",
      strat = "SEX",
      table_names = "coxph_stratified"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      is_event = "is_event",
      method = "both",
      time_point = 12
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio",
      "95% CI", "Stratified Analysis", "p-value (log-rank)", "Hazard Ratio",
      "95% CI", "12 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "Difference in Event Free Rate", "95% CI", "p-value (Z-test)",
      "A: Drug X", "(N=134)", "79 (59%)", "15", "14", "18", "17", "15",
      "55 (41%)", "", "41.41", "(27.7, 54.7)", "15.4, 75.2", "0.4 to 154.7",
      "0.3 to 116.4", "", "", "", "", "", "", "", "", "", "92", "78.03",
      "(70.82, 85.24)", "", "", "", "B: Placebo", "(N=134)", "87 (64.9%)",
      "14", "20", "16", "15", "22", "47 (35.1%)", "", "27.51", "(17.3, 30.2)",
      "9.5, 54.9", "0.9 to 91", "0 to 122.4", "", "0.0334", "1.394",
      "(1.0251, 1.8957)", "", "0.0478", "1.3644", "(1.0018, 1.8582)",
      "", "83", "70.32", "(62.27, 78.37)", "-7.71", "(-18.51, 3.1)",
      "0.1622", "C: Combination", "(N=132)", "116 (87.9%)", "21", "23",
      "24", "27", "21", "16 (12.1%)", "", "11.08", "(9.6, 15.9)", "5.3, 25.2",
      "0.3 to 49.4", "0.1 to 101.6", "", "<0.0001", "2.7532", "(2.0478, 3.7017)",
      "", "<0.0001", "2.7277", "(2.0171, 3.6886)", "", "56", "46.39",
      "(37.59, 55.18)", "-31.64", "(-43.01, -20.26)", "<0.0001"
    ),
    .Dim = c(30L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("TTET01 variant: modifying time point", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Patients with event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients without event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank"),
      table_names = "coxph_unstratified"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      is_event = "is_event",
      time_point = 6,
      method = "both"
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio",
      "95% CI", "6 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "Difference in Event Free Rate", "95% CI", "p-value (Z-test)",
      "A: Drug X", "(N=134)", "79 (59%)", "15", "14", "18", "17", "15",
      "55 (41%)", "", "41.41", "(27.7, 54.7)", "15.4, 75.2", "0.4 to 154.7",
      "0.3 to 116.4", "", "", "", "", "", "106", "83.83", "(77.49, 90.17)",
      "", "", "", "B: Placebo", "(N=134)", "87 (64.9%)", "14", "20",
      "16", "15", "22", "47 (35.1%)", "", "27.51", "(17.3, 30.2)",
      "9.5, 54.9", "0.9 to 91", "0 to 122.4", "", "0.0334", "1.394",
      "(1.0251, 1.8957)", "", "112", "89.16", "(83.8, 94.53)", "5.33",
      "(-2.97, 13.64)", "0.2080", "C: Combination", "(N=132)", "116 (87.9%)",
      "21", "23", "24", "27", "21", "16 (12.1%)", "", "11.08", "(9.6, 15.9)",
      "5.3, 25.2", "0.3 to 49.4", "0.1 to 101.6", "", "<0.0001", "2.7532",
      "(2.0478, 3.7017)", "", "92", "73.4", "(65.72, 81.07)", "-10.43",
      "(-20.38, -0.48)", "0.0399"
    ),
    .Dim = c(26L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("TTET01 variant: requesting more than one p-value", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Patients with event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients without event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      table_names = "time_to_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank"),
      .stats = "pvalue",
      table_names = "coxph_unstratified"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      show_labels = "hidden",
      control = control_coxph(pval_method = "wald"),
      .stats = "pvalue",
      .indent_mods = c(pvalue = 2L),
      table_names = "coxph_wald_pvalue"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      show_labels = "hidden",
      control = control_coxph(pval_method = "likelihood"),
      .indent_mods = c(pvalue = 2L, hr = 2L, hr_ci = 4L),
      table_names = "coxph_likelihood_pvalue"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      is_event = "is_event",
      time_point = 12,
      method = "both"
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "p-value (wald)",
      "p-value (likelihood)", "Hazard Ratio", "95% CI", "12 Months",
      "Patients remaining at risk", "Event Free Rate (%)", "95% CI",
      "Difference in Event Free Rate", "95% CI", "p-value (Z-test)",
      "A: Drug X", "(N=134)", "79 (59%)", "15", "14", "18", "17", "15",
      "55 (41%)", "", "41.41", "(27.7, 54.7)", "15.4, 75.2", "0.4 to 154.7",
      "0.3 to 116.4", "", "", "", "", "", "", "", "92", "78.03", "(70.82, 85.24)",
      "", "", "", "B: Placebo", "(N=134)", "87 (64.9%)", "14", "20",
      "16", "15", "22", "47 (35.1%)", "", "27.51", "(17.3, 30.2)",
      "9.5, 54.9", "0.9 to 91", "0 to 122.4", "", "0.0334", "0.0342",
      "0.0341", "1.394", "(1.0251, 1.8957)", "", "83", "70.32", "(62.27, 78.37)",
      "-7.71", "(-18.51, 3.1)", "0.1622", "C: Combination", "(N=132)",
      "116 (87.9%)", "21", "23", "24", "27", "21", "16 (12.1%)", "",
      "11.08", "(9.6, 15.9)", "5.3, 25.2", "0.3 to 49.4", "0.1 to 101.6",
      "", "<0.0001", "<0.0001", "<0.0001", "2.7532", "(2.0478, 3.7017)",
      "", "56", "46.39", "(37.59, 55.18)", "-31.64", "(-43.01, -20.26)",
      "<0.0001"
    ),
    .Dim = c(28L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
