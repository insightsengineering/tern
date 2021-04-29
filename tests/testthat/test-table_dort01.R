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
        ),
        levels = c("Responders with subsequent event (%)", "Responders without subsequent event (%)")
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
      is_event = "is_event",
      table_names = "duration_response"
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
      "95% CI", "A: Drug X", "(N=68)", "33 (48.5%)", "9", "5", "7",
      "7", "5", "35 (51.5%)", "", "23.8", "(17.9, 39.6)", "9.3, 44.3",
      "1.6 to 64.5", "0.3 to 44.3", "", "", "", "", "", "51", "83.63",
      "(74.78, 92.48)", "", "28", "63.41", "(50.48, 76.34)", "B: Placebo",
      "(N=73)", "51 (69.9%)", "14", "13", "5", "12", "7", "22 (30.1%)",
      "", "11.1", "(8.3, 14.6)", "6.2, 20.5", "0.1 to 43.8", "0.6 to 61.9",
      "", "0.0029", "1.94", "(1.24, 3.02)", "", "48", "76.14",
      "(65.92, 86.35)", "", "25", "43.82", "(31.28, 56.36)", "C: Combination",
      "(N=62)", "53 (85.5%)", "11", "12", "11", "12", "7", "9 (14.5%)",
      "", "6.6", "(4.4, 9.2)", "3, 15.7", "0.2 to 39.6", "0.2 to 53.8",
      "", "<0.0001", "2.99", "(1.92, 4.67)", "", "28", "50.57",
      "(37.68, 63.46)", "", "18", "33.72", "(21.28, 46.15)"
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
        ),
        levels = c("Responders with subsequent event (%)", "Responders without subsequent event (%)")
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
      is_event = "is_event",
      table_names = "duration_response"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank"),
      table_names = "coxph_unstratified"
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
      "(N=68)", "33 (48.5%)", "9", "5", "7", "7", "5", "35 (51.5%)",
      "", "23.8", "(17.9, 39.6)", "9.3, 44.3", "1.6 to 64.5", "0.3 to 44.3",
      "", "", "", "", "B: Placebo", "(N=73)", "51 (69.9%)", "14", "13",
      "5", "12", "7", "22 (30.1%)", "", "11.1", "(8.3, 14.6)", "6.2, 20.5",
      "0.1 to 43.8", "0.6 to 61.9", "", "0.0029", "1.94", "(1.24, 3.02)",
      "C: Combination", "(N=62)", "53 (85.5%)", "11", "12", "11", "12",
      "7", "9 (14.5%)", "", "6.6", "(4.4, 9.2)", "3, 15.7", "0.2 to 39.6",
      "0.2 to 53.8", "", "<0.0001", "2.99", "(1.92, 4.67)"
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
        ),
        levels = c("Responders with subsequent event (%)", "Responders without subsequent event (%)")
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
      is_event = "is_event",
      table_names = "duration_response"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "log-rank", conf_level = 0.90),
      table_names = "coxph_unstratified"
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
      "90% CI", "A: Drug X", "(N=68)", "33 (48.5%)", "9", "5", "7",
      "7", "5", "35 (51.5%)", "", "23.8", "(17.9, 39.6)", "9.3, 44.3",
      "1.6 to 64.5", "0.3 to 44.3", "", "", "", "", "", "51", "83.63",
      "(76.2, 91.06)", "", "28", "63.41", "(52.56, 74.26)", "B: Placebo",
      "(N=73)", "51 (69.9%)", "14", "13", "5", "12", "7", "22 (30.1%)",
      "", "11.1", "(8.3, 14.6)", "6.2, 20.5", "0.1 to 43.8", "0.6 to 61.9",
      "", "0.0029", "1.94", "(1.34, 2.81)", "", "48", "76.14",
      "(67.56, 84.71)", "", "25", "43.82", "(33.3, 54.34)", "C: Combination",
      "(N=62)", "53 (85.5%)", "11", "12", "11", "12", "7", "9 (14.5%)",
      "", "6.6", "(4.4, 9.2)", "3, 15.7", "0.2 to 39.6", "0.2 to 53.8",
      "", "<0.0001", "2.99", "(2.06, 4.35)", "", "28", "50.57",
      "(39.76, 61.39)", "", "18", "33.72", "(23.28, 44.15)"
    ),
    .Dim = c(27L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
