# Tests the variants for TTET01.

preproc_adtte <- function(adtte) {
  anl <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0,
      is_not_event = CNSR == 1,
      EVNT1 = factor(
        dplyr::case_when(
          is_event == TRUE ~ "Patients with event (%)",
          is_event == FALSE ~ "Patients without event (%)"
        ),
        levels = c("Patients with event (%)", "Patients without event (%)")
      ),
      EVNTDESC = factor(EVNTDESC)
    )
  anl
}

adsl <- adsl_raw
adtte <- adtte_raw

testthat::test_that("TTET01 default variant is produced correctly", {
  adtte <- adtte %>%
    preproc_adtte()

  l <- basic_table() %>%
    split_cols_by("ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Patients with event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 1L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
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

  result <- build_table(l, df = adtte, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("TTET01 variant 2: selecting sections to display", {
  adtte <- adtte %>%
    preproc_adtte()

  l <- basic_table() %>%
    split_cols_by("ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
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
      time_point = c(12)
    )
  result <- build_table(l, df = adtte, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("TTET01 variant 3: modifying analysis details like conftype, ties, alpha level", {
  adtte <- adtte %>%
    preproc_adtte()

  l <- basic_table() %>%
    split_cols_by("ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Patients with event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 1L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
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
  result <- build_table(l, df = adtte, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("TTET01 variant 4: with stratified analysis", {
  adtte <- adtte %>%
    preproc_adtte()

  l <- basic_table() %>%
    split_cols_by("ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Patients with event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 1L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
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
  result <- build_table(l, df = adtte, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("TTET01 variant 5: modifying time point", {
  adtte <- adtte %>%
    preproc_adtte()

  l <- basic_table() %>%
    split_cols_by("ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Patients with event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 1L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
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
  result <- build_table(l, df = adtte, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("TTET01 variant 6: requesting more than one p-value", {
  adtte <- adtte %>%
    preproc_adtte()

  l <- basic_table() %>%
    split_cols_by("ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
    ) %>%
    split_rows_by(
      "EVNT1",
      split_label = "Earliest contributing event",
      split_fun = keep_split_levels("Patients with event (%)"),
      label_pos = "visible",
      child_labels = "hidden",
      indent_mod = 1L,
    ) %>%
    split_rows_by("EVNTDESC", split_fun = drop_split_levels) %>%
    summarize_row_groups(format = "xx") %>%
    summarize_vars(
      vars = "is_not_event",
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
  result <- build_table(l, df = adtte, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
