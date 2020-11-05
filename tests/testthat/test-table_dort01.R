# Test variants of DORT01

library(random.cdisc.data)
library(dplyr)

test_that("DORT01 variant 1 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adtte <- radtte(cached = TRUE)

  adtte_f <- adtte %>%
    filter(PARAMCD == "CRSD" & BMEASIFL == "Y") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Responders with subsequent event (%)",
          is_event == FALSE ~ "Responders without subsequent event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Responders with subsequent event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Duration of response (Months)",
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
      var_labels = "Months duration",
      is_event = "is_event",
      time_point = c(6, 12)
    ) %>%
    build_table(adtte_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders with subsequent event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Responders without subsequent event (%)",
      "Duration of response (Months)", "Median", "95% CI", "25% and 75%-ile",
      "Range (censored)", "Range (event)", "Unstratified Analysis",
      "p-value (log-rank)", "Hazard Ratio", "95% CI", "6 Months duration",
      "Patients remaining at risk", "Event Free Rate (%)", "95% CI",
      "12 Months duration", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "A: Drug X", "(N=68)", "42 (61.8%)", "12", "5", "10",
      "7", "8", "26 (38.2%)", "", "16.1", "(13.1, 29.2)", "8.8, 33",
      "0.4 to 41.9", "0.2 to 52.6", "", "", "", "", "", "48", "81.61",
      "(72.18, 91.04)", "", "34", "68.39", "(56.43, 80.36)", "B: Placebo",
      "(N=73)", "49 (67.1%)", "7", "14", "11", "6", "11", "24 (32.9%)",
      "", "12.46", "(6.9, 20.9)", "4.3, 26.8", "0.1 to 82.6", "0.2 to 75.1",
      "", "0.2160", "1.3004", "(0.8567, 1.974)", "", "43", "71.19",
      "(60.48, 81.91)", "", "27", "51.58", "(38.98, 64.17)", "C: Combination",
      "(N=62)", "45 (72.6%)", "8", "13", "8", "9", "7", "17 (27.4%)",
      "", "8.19", "(6.4, 9.4)", "3.6, 16.6", "1.1 to 37.3", "0.2 to 35.7",
      "", "0.0006", "2.1163", "(1.3659, 3.2791)", "", "31", "63.33",
      "(50.7, 75.96)", "", "15", "33.86", "(20.69, 47.02)"
    ),
    .Dim = c(27L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("DORT01 variant 2 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adtte <- radtte(cached = TRUE)

  adtte_f <- adtte %>%
    filter(PARAMCD == "CRSD" & BMEASIFL == "Y") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Responders with subsequent event (%)",
          is_event == FALSE ~ "Responders without subsequent event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Responders with subsequent event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Duration of response (Months)",
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank")
    ) %>%
    build_table(adtte_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders with subsequent event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Responders without subsequent event (%)",
      "Duration of response (Months)", "Median", "95% CI", "25% and 75%-ile",
      "Range (censored)", "Range (event)", "Unstratified Analysis",
      "p-value (log-rank)", "Hazard Ratio", "95% CI", "A: Drug X",
      "(N=68)", "42 (61.8%)", "12", "5", "10", "7", "8", "26 (38.2%)",
      "", "16.1", "(13.1, 29.2)", "8.8, 33", "0.4 to 41.9", "0.2 to 52.6",
      "", "", "", "", "B: Placebo", "(N=73)", "49 (67.1%)", "7", "14",
      "11", "6", "11", "24 (32.9%)", "", "12.46", "(6.9, 20.9)", "4.3, 26.8",
      "0.1 to 82.6", "0.2 to 75.1", "", "0.2160", "1.3004", "(0.8567, 1.974)",
      "C: Combination", "(N=62)", "45 (72.6%)", "8", "13", "8", "9",
      "7", "17 (27.4%)", "", "8.19", "(6.4, 9.4)", "3.6, 16.6", "1.1 to 37.3",
      "0.2 to 35.7", "", "0.0006", "2.1163", "(1.3659, 3.2791)"
    ),
    .Dim = c(19L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("DORT01 variant 3 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adtte <- radtte(cached = TRUE)

  adtte_f <- adtte %>%
    filter(PARAMCD == "CRSD" & BMEASIFL == "Y") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        case_when(
          is_event == TRUE ~ "Responders with subsequent event (%)",
          is_event == FALSE ~ "Responders without subsequent event (%)"
        )
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    split_rows_by("EVNT1", split_fun = keep_split_levels("Responders with subsequent event (%)")) %>%
    summarize_row_groups() %>%
    summarize_vars(
      vars = "EVNTDESC",
      .stats = "count"
    ) %>%
    summarize_vars(
      "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Duration of response (Months)",
      is_event = "is_event"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank", conf_level = 0.90)
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months duration",
      is_event = "is_event",
      time_point = c(6, 12),
      control = control_surv_timepoint(conf_level = 0.90)
    ) %>%
    build_table(adtte_f)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders with subsequent event (%)", "Adverse Event",
      "Death", "Disease Progression", "Last Date Known To Be Alive",
      "Last Tumor Assessment", "Responders without subsequent event (%)",
      "Duration of response (Months)", "Median", "95% CI", "25% and 75%-ile",
      "Range (censored)", "Range (event)", "Unstratified Analysis",
      "p-value (log-rank)", "Hazard Ratio", "90% CI", "6 Months duration",
      "Patients remaining at risk", "Event Free Rate (%)", "90% CI",
      "12 Months duration", "Patients remaining at risk", "Event Free Rate (%)",
      "90% CI", "A: Drug X", "(N=68)", "42 (61.8%)", "12", "5", "10",
      "7", "8", "26 (38.2%)", "", "16.1", "(13.1, 29.2)", "8.8, 33",
      "0.4 to 41.9", "0.2 to 52.6", "", "", "", "", "", "48", "81.61",
      "(73.7, 89.52)", "", "34", "68.39", "(58.35, 78.43)", "B: Placebo",
      "(N=73)", "49 (67.1%)", "7", "14", "11", "6", "11", "24 (32.9%)",
      "", "12.46", "(6.9, 20.9)", "4.3, 26.8", "0.1 to 82.6", "0.2 to 75.1",
      "", "0.2160", "1.3004", "(0.9162, 1.8459)", "", "43", "71.19",
      "(62.2, 80.19)", "", "27", "51.58", "(41.01, 62.15)", "C: Combination",
      "(N=62)", "45 (72.6%)", "8", "13", "8", "9", "7", "17 (27.4%)",
      "", "8.19", "(6.4, 9.4)", "3.6, 16.6", "1.1 to 37.3", "0.2 to 35.7",
      "", "0.0006", "2.1163", "(1.4655, 3.0562)", "", "31", "63.33",
      "(52.73, 73.93)", "", "15", "33.86", "(22.81, 44.9)"
    ),
    .Dim = c(27L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
