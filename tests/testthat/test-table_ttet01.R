# Tests the variants for TTET01.

library(random.cdisc.data)
library(dplyr)

test_that("TTET01 default variant is produced correctly", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
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
      show_label = "hidden",
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
      show_label = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Time to Event (Months)", "Median", "  95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "6 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "  Difference in Event Free Rate", "    95% CI", "    p-value (Z-test)",
      "12 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "  Difference in Event Free Rate", "    95% CI", "    p-value (Z-test)",
      "A: Drug X", "(N=134)", "", "96.86", "(74, 125.3)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2", rep("", 5),
      "124", "96.92", "(93.95, 99.89)", rep("", 4), "109", "89.04", "(83.61, 94.46)", rep("", 3),
      "B: Placebo", "(N=134)", "", "92.31", "(71.2, 102.7)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "", "0.8512", "1.0287", "(0.7652, 1.3831)", "",
      "126", "95.47", "(91.93, 99.01)", "-1.45", "(-6.07, 3.17)", "0.5386", "", "119", "91.62",
      "(86.88, 96.36)", "2.59", "(-4.62, 9.79)", "0.4818",
      "C: Combination", "(N=132)", "", "79.23", "(55.8, 107.4)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1",
      "", "0.0918", "1.2955", "(0.958, 1.7519)", "",
      "120", "94.53", "(90.59, 98.47)", "-2.39", "(-7.33, 2.54)", "0.3422",
      "", "109", "88.89", "(83.4, 94.38)", "-0.14", "(-7.86, 7.57)", "0.9707"
    ),
    .Dim = c(26L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("TTET01 variant: selecting sections to display", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
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
      "", "", "Time to Event (Months)", "Median", "  95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "6 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "12 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "A: Drug X", "(N=134)", "", "96.86", "(74, 125.3)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2", rep("", 5),
      "124", "96.92", "(93.95, 99.89)", "", "109", "89.04", "(83.61, 94.46)",
      "B: Placebo", "(N=134)", "", "92.31", "(71.2, 102.7)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "", "0.8512", "1.0287", "(0.7652, 1.3831)", "",
      "126", "95.47", "(91.93, 99.01)",  "", "119", "91.62", "(86.88, 96.36)",
      "C: Combination", "(N=132)", "", "79.23", "(55.8, 107.4)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1",
      "", "0.0918", "1.2955", "(0.958, 1.7519)", "",
      "120", "94.53", "(90.59, 98.47)", "", "109", "88.89", "(83.4, 94.38)"
    ),
    .Dim = c(20L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("TTET01 variant: modifying analysis details like conftype, ties, alpha level", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
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
      show_label = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12),
      conf_level = 0.975
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Time to Event (Months)", "Median", "  90% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "12 Months", "Patients remaining at risk", "Event Free Rate (%)", "  90% CI",
      "  Difference in Event Free Rate", "    97.5% CI", "    p-value (Z-test)",
      "A: Drug X", "(N=134)", "", "96.86", "(75.5, 124.9)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2", rep("", 5),
      "109", "89.04", "(83.5, 92.8)", rep("", 3),
      "B: Placebo", "(N=134)", "", "92.31", "(71.7, 101.9)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "", "0.8512", "1.0287", "(0.7652, 1.3831)", "", "119", "91.62", "(86.61, 94.81)",
      "2.59", "(-5.65, 10.82)", "0.4818",
      "C: Combination", "(N=132)", "", "79.23", "(57.4, 104.3)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1",
      "", "0.0918", "1.2955", "(0.958, 1.7519)", "", "109", "88.89", "(83.29, 92.7)", "-0.14", "(-8.97, 8.68)", "0.9707"
    ),
    .Dim = c(19L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("TTET01 variant: with stratified analysis", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
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
      show_label = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Time to Event (Months)", "Median", "  95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "Stratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "12 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "  Difference in Event Free Rate", "    95% CI", "    p-value (Z-test)",
      "A: Drug X", "(N=134)", "", "96.86", "(74, 125.3)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2", rep("", 9),
      "109", "89.04", "(83.61, 94.46)", rep("", 3),
      "B: Placebo", "(N=134)", "", "92.31", "(71.2, 102.7)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "", "0.8512", "1.0287", "(0.7652, 1.3831)", "", "0.8589", "1.0273", "(0.7632, 1.3829)",
      "", "119", "91.62", "(86.88, 96.36)", "2.59", "(-4.62, 9.79)", "0.4818",
      "C: Combination", "(N=132)", "", "79.23", "(55.8, 107.4)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1",
      "", "0.0918", "1.2955", "(0.958, 1.7519)", "", "0.1222", "1.2792", "(0.9355, 1.7493)",
      "", "109", "88.89", "(83.4, 94.38)", "-0.14", "(-7.86, 7.57)", "0.9707"
    ),
    .Dim = c(23L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("TTET01 variant: modifying time point", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
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
      show_label = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 6)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Time to Event (Months)", "Median", "  95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "6 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "  Difference in Event Free Rate", "    95% CI", "    p-value (Z-test)",
      "A: Drug X", "(N=134)", "", "96.86", "(74, 125.3)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2", rep("", 5),
      "124", "96.92", "(93.95, 99.89)", rep("", 3),
      "B: Placebo", "(N=134)", "", "92.31", "(71.2, 102.7)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "", "0.8512", "1.0287", "(0.7652, 1.3831)", "",
      "126", "95.47", "(91.93, 99.01)", "-1.45", "(-6.07, 3.17)", "0.5386",
      "C: Combination", "(N=132)", "", "79.23", "(55.8, 107.4)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1",
      "", "0.0918", "1.2955", "(0.958, 1.7519)", "",
      "120", "94.53", "(90.59, 98.47)", "-2.39", "(-7.33, 2.54)", "0.3422"
    ),
    .Dim = c(19L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("TTET01 variant: requesting more than one p-value", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  l <- split_cols_by(lyt = NULL, var = "ARM", ref_group = "A: Drug X") %>%
    add_colcounts() %>%
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
      show_label = "hidden",
      control = control_coxph(pval_method = "wald"),
      .stats = "pvalue",
      .indent_mods = c(pvalue = 2L)
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      show_label = "hidden",
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
      show_label = "hidden",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 12)
    )
  result <- build_table(l, adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Time to Event (Months)", "Median", "  95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "Unstratified Analysis", "p-value (log-rank)", "  p-value (wald)", "  p-value (likelihood)",
      "  Hazard Ratio", "    95% CI",
      "12 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "  Difference in Event Free Rate", "    95% CI", "    p-value (Z-test)",
      "A: Drug X", "(N=134)", "", "96.86", "(74, 125.3)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2", rep("", 7),
      "109", "89.04", "(83.61, 94.46)", rep("", 3),
      "B: Placebo", "(N=134)", "", "92.31", "(71.2, 102.7)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "", "0.8512", "0.8512", "0.8511", "1.0287", "(0.7652, 1.3831)", "", "119", "91.62",
      "(86.88, 96.36)", "2.59", "(-4.62, 9.79)", "0.4818",
      "C: Combination", "(N=132)", "", "79.23", "(55.8, 107.4)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1",
      "", "0.0918", "0.0927", "0.0925", "1.2955", "(0.958, 1.7519)", "",
      "109", "88.89", "(83.4, 94.38)", "-0.14", "(-7.86, 7.57)", "0.9707"
    ),
    .Dim = c(21L, 4L),
    .Dimnames = list(NULL, c("", "A: Drug X", "B: Placebo", "C: Combination"))
  )
  expect_identical(result_matrix, expected_matrix)
})
