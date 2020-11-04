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
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank")
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "6 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 6)
    ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 6)
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "12 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )

  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)",
      "Range (event)", "Unstratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "6 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "95% CI", "Difference in Event Free Rate",
      "95% CI", "p-value (Z-test)", "12 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "95% CI", "Difference in Event Free Rate",
      "95% CI", "p-value (Z-test)", "A: Drug X", "(N=134)",
      "81 (60.4%)", "12", "12", "23", "19", "15", "53 (39.6%)", "",
      "33.19", "(31.3, 41.9)", "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3",
      "", "", "", "", "", "107", "89.74", "(84.44, 95.03)", "", "",
      "", "", "93", "81.84", "(74.94, 88.74)", "", "", "", "B: Placebo",
      "(N=134)", "92 (68.7%)", "18", "15", "11", "23", "25", "42 (31.3%)",
      "", "26.73", "(20, 36.1)", "12.6, 48.5", "2.8 to 130.7", "0.3 to 155.8",
      "", "0.2584", "1.1887", "(0.8805, 1.6048)", "", "114", "89.33",
      "(84.05, 94.62)", "-0.4", "(-7.89, 7.08)", "0.9155", "", "93",
      "77.23", "(69.93, 84.54)", "-4.61", "(-14.65, 5.44)", "0.3686",
      "C: Combination", "(N=132)", "106 (80.3%)", "25", "15", "23",
      "22", "21", "26 (19.7%)", "", "14.82", "(10.9, 22.6)", "6, 33.3",
      "1.1 to 81.9", "0.2 to 72", "", "<0.0001", "2.1107", "(1.5689, 2.8397)",
      "", "92", "75.02", "(67.51, 82.53)", "-14.72", "(-23.91, -5.53)",
      "0.0017", "", "64", "55.91", "(47.14, 64.68)", "-25.93", "(-37.09, -14.78)",
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
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank")
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "6 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 6)
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "12 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)",
      "Range (event)", "Unstratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "6 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "95% CI", "12 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "95% CI", "A: Drug X", "(N=134)", "81 (60.4%)",
      "12", "12", "23", "19", "15", "53 (39.6%)", "", "33.19", "(31.3, 41.9)",
      "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3", "", "", "", "",
      "", "107", "89.74", "(84.44, 95.03)", "", "93", "81.84", "(74.94, 88.74)",
      "B: Placebo", "(N=134)", "92 (68.7%)", "18", "15", "11", "23",
      "25", "42 (31.3%)", "", "26.73", "(20, 36.1)", "12.6, 48.5",
      "2.8 to 130.7", "0.3 to 155.8", "", "0.2584", "1.1887", "(0.8805, 1.6048)",
      "", "114", "89.33", "(84.05, 94.62)", "", "93", "77.23", "(69.93, 84.54)",
      "C: Combination", "(N=132)", "106 (80.3%)", "25", "15", "23",
      "22", "21", "26 (19.7%)", "", "14.82", "(10.9, 22.6)", "6, 33.3",
      "1.1 to 81.9", "0.2 to 72", "", "<0.0001", "2.1107", "(1.5689, 2.8397)",
      "", "92", "75.02", "(67.51, 82.53)", "", "64", "55.91", "(47.14, 64.68)"
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
      control = control_surv_time(conf_level = 0.9, conf_type = "log-log")
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank", conf_level = 0.95, ties = "efron")
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "12 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12, conf_level = 0.9, conf_type = "log-log")
    ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12),
      conf_level = 0.975
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "90% CI", "25% and 75%-ile", "Range (censored)",
      "Range (event)", "Unstratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "12 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "90% CI", "Difference in Event Free Rate",
      "97.5% CI", "p-value (Z-test)", "A: Drug X", "(N=134)",
      "81 (60.4%)", "12", "12", "23", "19", "15", "53 (39.6%)", "",
      "33.19", "(31.3, 40.8)", "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3",
      "", "", "", "", "", "93", "81.84", "(75.19, 86.87)", "", "",
      "", "B: Placebo", "(N=134)", "92 (68.7%)", "18", "15", "11",
      "23", "25", "42 (31.3%)", "", "26.73", "(21, 34.8)", "12.6, 48.5",
      "2.8 to 130.7", "0.3 to 155.8", "", "0.2584", "1.1887", "(0.8805, 1.6048)",
      "", "93", "77.23", "(70.38, 82.7)", "-4.61", "(-16.1, 6.88)",
      "0.3686", "C: Combination", "(N=132)", "106 (80.3%)", "25", "15",
      "23", "22", "21", "26 (19.7%)", "", "14.82", "(11.1, 21.3)",
      "6, 33.3", "1.1 to 81.9", "0.2 to 72", "", "<0.0001", "2.1107",
      "(1.5689, 2.8397)", "", "64", "55.91", "(48.23, 62.9)", "-25.93",
      "(-38.7, -13.17)", "<0.0001"
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
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = "Unstratified Analysis"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = "Stratified Analysis",
      strat = "SEX"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "12 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)",
      "Range (event)", "Unstratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "Stratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "12 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "95% CI", "Difference in Event Free Rate",
      "95% CI", "p-value (Z-test)", "A: Drug X", "(N=134)",
      "81 (60.4%)", "12", "12", "23", "19", "15", "53 (39.6%)", "",
      "33.19", "(31.3, 41.9)", "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3",
      "", "", "", "", "", "", "", "", "", "93", "81.84", "(74.94, 88.74)",
      "", "", "", "B: Placebo", "(N=134)", "92 (68.7%)", "18", "15",
      "11", "23", "25", "42 (31.3%)", "", "26.73", "(20, 36.1)", "12.6, 48.5",
      "2.8 to 130.7", "0.3 to 155.8", "", "0.2584", "1.1887", "(0.8805, 1.6048)",
      "", "0.2789", "1.1825", "(0.8727, 1.6021)", "", "93", "77.23",
      "(69.93, 84.54)", "-4.61", "(-14.65, 5.44)", "0.3686", "C: Combination",
      "(N=132)", "106 (80.3%)", "25", "15", "23", "22", "21", "26 (19.7%)",
      "", "14.82", "(10.9, 22.6)", "6, 33.3", "1.1 to 81.9", "0.2 to 72",
      "", "<0.0001", "2.1107", "(1.5689, 2.8397)", "", "<0.0001", "2.0803",
      "(1.5453, 2.8004)", "", "64", "55.91", "(47.14, 64.68)", "-25.93",
      "(-37.09, -14.78)", "<0.0001"
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
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank")
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "6 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 6)
    ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 6)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)",
      "Range (event)", "Unstratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "6 Months", "Patients remaining at risk",
      "Event Free Rate (%)", "95% CI", "Difference in Event Free Rate",
      "95% CI", "p-value (Z-test)", "A: Drug X", "(N=134)",
      "81 (60.4%)", "12", "12", "23", "19", "15", "53 (39.6%)", "",
      "33.19", "(31.3, 41.9)", "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3",
      "", "", "", "", "", "107", "89.74", "(84.44, 95.03)", "", "",
      "", "B: Placebo", "(N=134)", "92 (68.7%)", "18", "15", "11",
      "23", "25", "42 (31.3%)", "", "26.73", "(20, 36.1)", "12.6, 48.5",
      "2.8 to 130.7", "0.3 to 155.8", "", "0.2584", "1.1887", "(0.8805, 1.6048)",
      "", "114", "89.33", "(84.05, 94.62)", "-0.4", "(-7.89, 7.08)",
      "0.9155", "C: Combination", "(N=132)", "106 (80.3%)", "25", "15",
      "23", "22", "21", "26 (19.7%)", "", "14.82", "(10.9, 22.6)",
      "6, 33.3", "1.1 to 81.9", "0.2 to 72", "", "<0.0001", "2.1107",
      "(1.5689, 2.8397)", "", "92", "75.02", "(67.51, 82.53)", "-14.72",
      "(-23.91, -5.53)", "0.0017"
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
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank"),
      .stats = "pvalue"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      show_labels = "hidden",
      control = control_coxph(pval_method = "wald"),
      .stats = "pvalue",
      .indent_mods = c(pvalue = 2L)
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      show_labels = "hidden",
      control = control_coxph(pval_method = "likelihood"),
      .indent_mods = c(pvalue = 2L, hr = 2L, hr_ci = 4L)
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "12 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      show_labels = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Patients with event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Patients without event (%)", "Time to Event (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)",
      "Range (event)", "Unstratified Analysis", "p-value (log-rank)",
      "p-value (wald)", "p-value (likelihood)", "Hazard Ratio",
      "95% CI", "12 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "Difference in Event Free Rate", "95% CI",
      "p-value (Z-test)", "A: Drug X", "(N=134)", "81 (60.4%)",
      "12", "12", "23", "19", "15", "53 (39.6%)", "", "33.19", "(31.3, 41.9)",
      "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3", "", "", "", "",
      "", "", "", "93", "81.84", "(74.94, 88.74)", "", "", "", "B: Placebo",
      "(N=134)", "92 (68.7%)", "18", "15", "11", "23", "25", "42 (31.3%)",
      "", "26.73", "(20, 36.1)", "12.6, 48.5", "2.8 to 130.7", "0.3 to 155.8",
      "", "0.2584", "0.2590", "0.2585", "1.1887", "(0.8805, 1.6048)",
      "", "93", "77.23", "(69.93, 84.54)", "-4.61", "(-14.65, 5.44)",
      "0.3686", "C: Combination", "(N=132)", "106 (80.3%)", "25", "15",
      "23", "22", "21", "26 (19.7%)", "", "14.82", "(10.9, 22.6)",
      "6, 33.3", "1.1 to 81.9", "0.2 to 72", "", "<0.0001", "<0.0001",
      "<0.0001", "2.1107", "(1.5689, 2.8397)", "", "64", "55.91", "(47.14, 64.68)",
      "-25.93", "(-37.09, -14.78)", "<0.0001"
    ),
    .Dim = c(28L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
