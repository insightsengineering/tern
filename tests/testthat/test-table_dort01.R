# Test variants of DORT01

library(scda)
library(dplyr)

preproc_adtte <- function(adtte) {
  anl <- adtte %>%
    dplyr::filter(PARAMCD == "CRSD" & BMEASIFL == "Y") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        dplyr::case_when(
          is_event == TRUE ~ "Responders with subsequent event (%)",
          is_event == FALSE ~ "Responders without subsequent event (%)"
        ),
        levels = c("Responders with subsequent event (%)", "Responders without subsequent event (%)")
      ),
      EVNTDESC = factor(EVNTDESC)
    )

  anl
}

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("DORT01 variant 1 is produced correctly", {
  adtte <- adtte %>%
    preproc_adtte()

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    count_values(
      vars = "USUBJID",
      values = unique(adtte$USUBJID),
      .labels = "Responders",
      .stats = "count"
    ) %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders with subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      show_labels = "hidden",
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Responders with subsequent event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 2L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Duration of response (Months)",
      is_event = "is_event",
      table_names = "duration_response"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months duration",
      is_event = "is_event",
      time_point = 12
    ) %>%
    build_table(adtte, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders", "Responders with subsequent event (%)",
      "Earliest contributing event", "Death", "Disease Progression",
      "Responders without subsequent event (%)", "Duration of response (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "12 Months duration", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "A: Drug X", "(N=134)", "68", "33 (48.5%)", "", "14", "19",
      "35 (51.5%)", "", "23.8", "(17.9, 39.6)", "9.3, 44.3", "1.6 to 64.5",
      "0.3 to 44.3", "", "28", "63.41", "(50.48, 76.34)", "B: Placebo", "(N=134)",
      "73", "51 (69.9%)", "", "20", "31", "22 (30.1%)", "", "11.1", "(8.3, 14.6)",
      "6.2, 20.5", "0.1 to 43.8", "0.6 to 61.9", "", "25", "43.82", "(31.28, 56.36)",
      "C: Combination", "(N=132)", "62", "53 (85.5%)", "", "25", "28", "9 (14.5%)",
      "", "6.6", "(4.4, 9.2)", "3.0, 15.7", "0.2 to 39.6", "0.2 to 53.8", "",
      "18", "33.72", "(21.28, 46.15)"
    ),
    .Dim = c(18L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("DORT01 variant 2 (selecting sectons) is produced correctly", {
  adtte <- adtte %>%
    preproc_adtte()

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    count_values(
      vars = "USUBJID",
      values = unique(adtte$USUBJID),
      .labels = "Responders",
      .stats = "count"
    ) %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders with subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      show_labels = "hidden",
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Responders with subsequent event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 2L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
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
    build_table(adtte, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders", "Responders with subsequent event (%)",
      "Earliest contributing event", "Death", "Disease Progression",
      "Responders without subsequent event (%)", "Duration of response (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", "95% CI",
      "A: Drug X", "(N=134)", "68", "33 (48.5%)", "", "14", "19", "35 (51.5%)",
      "", "23.8", "(17.9, 39.6)", "9.3, 44.3", "1.6 to 64.5", "0.3 to 44.3", "",
      "", "", "", "B: Placebo", "(N=134)", "73", "51 (69.9%)", "", "20", "31",
      "22 (30.1%)", "", "11.1", "(8.3, 14.6)", "6.2, 20.5", "0.1 to 43.8",
      "0.6 to 61.9", "", "0.0029", "1.94", "(1.24, 3.02)", "C: Combination",
      "(N=132)", "62", "53 (85.5%)", "", "25", "28", "9 (14.5%)", "", "6.6",
      "(4.4, 9.2)", "3.0, 15.7", "0.2 to 39.6", "0.2 to 53.8", "", "<0.0001",
      "2.99", "(1.92, 4.67)"
    ),
    .Dim = c(18L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("DORT01 variant 3 (modifying conftype and alpha level) is produced correctly", {
  adtte <- adtte %>%
    preproc_adtte()

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    count_values(
      vars = "USUBJID",
      values = unique(adtte$USUBJID),
      .labels = "Responders",
      .stats = "count"
    ) %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders with subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      show_labels = "hidden",
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Responders with subsequent event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 2L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Duration of response (Months)",
      is_event = "is_event",
      control = control_surv_time(conf_level = 0.90, conf_type = "log-log")
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months duration",
      is_event = "is_event",
      time_point = 12,
      control = control_surv_timepoint(conf_level = 0.975)
    ) %>%
    build_table(adtte, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders", "Responders with subsequent event (%)",
      "Earliest contributing event", "Death", "Disease Progression",
      "Responders without subsequent event (%)", "Duration of response (Months)",
      "Median", "90% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "12 Months duration", "Patients remaining at risk", "Event Free Rate (%)",
      "97.5% CI", "A: Drug X", "(N=134)", "68", "33 (48.5%)", "", "14", "19",
      "35 (51.5%)", "", "23.8", "(17.9, 30.2)", "9.3, 44.3", "1.6 to 64.5",
      "0.3 to 44.3", "", "28", "63.41", "(48.63, 78.19)", "B: Placebo", "(N=134)",
      "73", "51 (69.9%)", "", "20", "31", "22 (30.1%)", "", "11.1", "(9.1, 14.0)",
      "6.2, 20.5", "0.1 to 43.8", "0.6 to 61.9", "", "25", "43.82", "(29.48, 58.16)",
      "C: Combination", "(N=132)", "62", "53 (85.5%)", "", "25", "28", "9 (14.5%)",
      "", "6.6", "(4.4, 8.0)", "3.0, 15.7", "0.2 to 39.6", "0.2 to 53.8", "",
      "18", "33.72", "(19.49, 47.94)"
    ),
    .Dim = c(18L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("DORT01 variant 4 (modifying time point for the “xx duration”) is produced correctly", {
  adtte <- adtte %>%
    preproc_adtte()

  result <- basic_table() %>%
    split_cols_by(var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    count_values(
      vars = "USUBJID",
      values = unique(adtte$USUBJID),
      .labels = "Responders",
      .stats = "count"
    ) %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders with subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      show_labels = "hidden",
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Responders with subsequent event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 2L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Responders without subsequent event (%)"),
      .indent_mods = c(count_fraction = 1L),
      nested = FALSE,
      show_labels = "hidden"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Duration of response (Months)",
      is_event = "is_event"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months duration",
      is_event = "is_event",
      time_point = 6
    ) %>%
    build_table(adtte, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders", "Responders with subsequent event (%)",
      "Earliest contributing event", "Death", "Disease Progression",
      "Responders without subsequent event (%)", "Duration of response (Months)",
      "Median", "95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "6 Months duration", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "A: Drug X", "(N=134)", "68", "33 (48.5%)", "", "14", "19",
      "35 (51.5%)", "", "23.8", "(17.9, 39.6)", "9.3, 44.3", "1.6 to 64.5",
      "0.3 to 44.3", "", "51", "83.63", "(74.78, 92.48)", "B: Placebo", "(N=134)",
      "73", "51 (69.9%)", "", "20", "31", "22 (30.1%)", "", "11.1", "(8.3, 14.6)",
      "6.2, 20.5", "0.1 to 43.8", "0.6 to 61.9", "", "48", "76.14", "(65.92, 86.35)",
      "C: Combination", "(N=132)", "62", "53 (85.5%)", "", "25", "28", "9 (14.5%)",
      "", "6.6", "(4.4, 9.2)", "3.0, 15.7", "0.2 to 39.6", "0.2 to 53.8", "", "28",
      "50.57", "(37.68, 63.46)"
    ),
    .Dim = c(18L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
