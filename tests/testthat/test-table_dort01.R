# Test variants of DORT01

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

adsl <- adsl_raw
adtte_local <- adtte_raw %>%
  preproc_adtte()

testthat::test_that("DORT01 variant 1 is produced correctly", {
  adtte <- adtte_local

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

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("DORT01 variant 2 (selecting sectons) is produced correctly", {
  adtte <- adtte_local

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

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("DORT01 variant 3 (modifying conftype and alpha level) is produced correctly", {
  adtte <- adtte_local

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

  res <- expect_silent(result)
  expect_snapshot(res)
})

testthat::test_that("DORT01 variant 4 (modifying time point for the “xx duration”) is produced correctly", {
  adtte <- adtte_local

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

  res <- expect_silent(result)
  expect_snapshot(res)
})
