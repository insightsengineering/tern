library(random.cdisc.data)
library(dplyr)

test_that("s_surv_timepoint works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- s_surv_timepoint(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event"
  )
  expected <- list(
    pt_at_risk = with_label(126, "Patients remaining at risk"),
    event_free_rate = with_label(95.47124, "Event Free Rate (%)"),
    rate_se = with_label(1.806604, "Standard Error of Event Free Rate"),
    rate_ci = with_label(c(91.93036, 99.01212), "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.0000001)

})


test_that("s_surv_timepoint works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- s_surv_timepoint(
    adtte_f %>% dplyr::filter(ARMCD == "ARM C"),
    .var = "AVAL",
    is_event = "is_event",
    control = control_surv_timepoint(
      conf_level = 0.99, conf_type = "log", time_point = 7
    )
  )
  expected <- list(
    pt_at_risk = with_label(118, "Patients remaining at risk"),
    event_free_rate = with_label(93.74203, "Event Free Rate (%)"),
    rate_se = with_label(2.142505, "Standard Error of Event Free Rate"),
    rate_ci = with_label(c(88.38261, 99.42644), "99% CI")
  )
  expect_equal(result, expected, tolerance = 0.0000001)

})

test_that("surv_timepoint works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
    var = "ARMCD",
    ref_group = "ARM A"
  ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "6 Months",
      is_event = "is_event"
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "6 Months", "Patients remaining at risk", "Event Free Rate (%)", "  95% CI",
      "ARM A", "", "124", "96.92", "(93.95, 99.89)",
      "ARM B", "", "126", "95.47", "(91.93, 99.01)",
      "ARM C", "", "120", "94.53", "(90.59, 98.47)"
    ),
    .Dim = 5:4
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("surv_timepoint works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
    var = "ARMCD",
    ref_group = "ARM A"
  ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "8 Months",
      is_event = "is_event",
      control = control_surv_timepoint(conf_level = 0.9, conf_type = "log-log", time_point = 8)
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "8 Months", "Patients remaining at risk", "Event Free Rate (%)", "  90% CI",
      "ARM A", "", "117", "92.23", "(87.27, 95.3)",
      "ARM B", "", "121", "93.16", "(88.46, 95.99)",
      "ARM C", "", "117", "93.74", "(89.08, 96.45)"
    ),
    .Dim = 5:4
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("s_surv_timepoint_diff works with default arguments for comparison group", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_surv_timepoint_diff(
    df = df,
    .var = "AVAL",
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    is_event = "is_event"
  )
  expected <- list(
    rate_diff = with_label(1.450188, "Difference in Event Free Rate"),
    rate_diff_ci = with_label(c(-3.171972, 6.072348), "95% CI"),
    ztest_pval = with_label(0.5385993, "p-value (Z-test)")
  )
  expect_equal(result, expected, tolerance = 0.000001)
})


test_that("s_surv_timepoint_diff works with customized arguments for comparison arm", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_surv_timepoint_diff(
    df = df,
    .var = "AVAL",
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    is_event = "is_event",
    control = control_surv_timepoint(time_point = 8),
    conf_level = 0.9
  )
  expected <- list(
    rate_diff = with_label(-0.9363941, "Difference in Event Free Rate"),
    rate_diff_ci = with_label(c(-6.246414, 4.373626), "90% CI"),
    ztest_pval = with_label(0.7717694, "p-value (Z-test)")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
})

test_that("surv_timepoint_diff works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
    var = "ARMCD",
    ref_group = "ARM A"
  ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      var_labels = "6 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 9)
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "6 Months", "  Difference in Event Free Rate", "    95% CI", "    p-value (Z-test)",
      "ARM A", "", "", "", "",
      "ARM B", "", "2.51", "(-4.12, 9.15)", "0.4578",
      "ARM C", "", "2.29", "(-4.43, 9.01)", "0.5038"
    ),
    .Dim = 5:4
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("surv_timepoint_diff works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
    var = "ARMCD",
    ref_group = "ARM A"
  ) %>%
    surv_timepoint_diff(
      vars = "AVAL",
      var_labels = "9 Months",
      is_event = "is_event",
      control = control_surv_timepoint(time_point = 9),
      conf_level = 0.99
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "9 Months", "  Difference in Event Free Rate", "    99% CI", "    p-value (Z-test)",
      "ARM A", "", "", "", "",
      "ARM B", "", "2.51", "(-6.2, 11.23)", "0.4578",
      "ARM C", "", "2.29", "(-6.54, 11.13)", "0.5038"
    ),
    .Dim = 5:4
  )
  expect_identical(result_matrix, expected_matrix)
})
