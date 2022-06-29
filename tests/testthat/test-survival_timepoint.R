library(scda)
library(dplyr)

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("s_surv_timepoint works with default arguments", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_surv_timepoint(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    time_point = 6,
    is_event = "is_event"
  )
  expected <- list(
    pt_at_risk = formatters::with_label(112, label = "Patients remaining at risk"),
    event_free_rate = formatters::with_label(89.1626763122249, label = "Event Free Rate (%)"),
    rate_se = formatters::with_label(2.73767719947927, label = "Standard Error of Event Free Rate"),
    rate_ci = formatters::with_label(c(83.7969275999491, 94.5284250245007), label = "95% CI")
  )
  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("s_surv_timepoint works with customized arguments", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_surv_timepoint(
    adtte_f %>% dplyr::filter(ARMCD == "ARM C"),
    .var = "AVAL",
    is_event = "is_event",
    time_point = 7,
    control = control_surv_timepoint(
      conf_level = 0.99, conf_type = "log"
    )
  )
  expected <- list(
    pt_at_risk = formatters::with_label(83, label = "Patients remaining at risk"),
    event_free_rate = formatters::with_label(66.9772864371622, label = "Event Free Rate (%)"),
    rate_se = formatters::with_label(4.17892667633869, label = "Standard Error of Event Free Rate"),
    rate_ci = formatters::with_label(c(57.0335300760197, 78.6547298143104), label = "99% CI")
  )
  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("s_surv_timepoint also works when there are 0 patients at risk", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    ) %>%
    # Only take patients from Arm A who have less than 6 months time point,
    # such that no patients are at risk anymore beyond 6 months.
    dplyr::filter(ARMCD == "ARM A", AVAL <= 6)

  result <- testthat::expect_silent(s_surv_timepoint(
    adtte_f,
    .var = "AVAL",
    time_point = 6,
    is_event = "is_event"
  ))
  expected <- list(
    pt_at_risk = formatters::with_label(NA_real_, "Patients remaining at risk"),
    event_free_rate = formatters::with_label(NA_real_, "Event Free Rate (%)"),
    rate_se = formatters::with_label(NA_real_, "Standard Error of Event Free Rate"),
    rate_ci = formatters::with_label(c(NA_real_, NA_real_), "95% CI")
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("surv_timepoint works with default arguments", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      time_point = 6,
      is_event = "is_event"
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "6 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "95% CI", "ARM A", "", "106", "83.83", "(77.49, 90.17)", "ARM B",
      "", "112", "89.16", "(83.80, 94.53)", "ARM C", "", "92", "73.40",
      "(65.72, 81.07)"
    ),
    .Dim = 5:4
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("surv_timepoint works with customized arguments", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      time_point = 8,
      is_event = "is_event",
      control = control_surv_timepoint(conf_level = 0.9, conf_type = "log-log")
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "8 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "90% CI", "ARM A", "", "98", "81.36", "(74.90, 86.30)", "ARM B",
      "", "100", "81.92", "(75.47, 86.82)", "ARM C", "", "77", "62.92",
      "(55.40, 69.53)"
    ),
    .Dim = 5:4
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("s_surv_timepoint_diff works with default arguments for comparison group", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_surv_timepoint_diff(
    df = df,
    .var = "AVAL",
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    is_event = "is_event",
    time_point = 6,
    control = control_surv_timepoint()
  )
  expected <- list(
    rate_diff = formatters::with_label(-5.33344544636732, label = "Difference in Event Free Rate"),
    rate_diff_ci = formatters::with_label(c(-13.6362446145953, 2.96935372186064), label = "95% CI"),
    ztest_pval = formatters::with_label(0.208024379170235, label = "p-value (Z-test)")
  )
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})


testthat::test_that("s_surv_timepoint_diff works with customized arguments for comparison arm", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_surv_timepoint_diff(
    df = df,
    .var = "AVAL",
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    time_point = 8,
    is_event = "is_event",
    control = control_surv_timepoint(conf_level = 0.9)
  )
  expected <- list(
    rate_diff = formatters::with_label(-0.562624959121464, label = "Difference in Event Free Rate"),
    rate_diff_ci = formatters::with_label(c(-8.53787765104722, 7.41262773280429), label = "90% CI"),
    ztest_pval = formatters::with_label(0.907622094366106, label = "p-value (Z-test)")
  )
  testthat::expect_equal(result, expected, tolerance = 0.0000001)
})

testthat::test_that("surv_timepoint for survival diff works with default arguments", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      time_point = 9,
      is_event = "is_event",
      method = "surv_diff"
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "9 Months", "Difference in Event Free Rate",
      "95% CI", "p-value (Z-test)", "ARM A", "", "", "", "", "ARM B",
      "", "-4.34", "(-14.48, 5.79)", "0.4012", "ARM C", "", "-20.05",
      "(-31.02, -9.09)", "0.0003"
    ),
    .Dim = 5:4
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("surv_timepoint for survival diff works with customized arguments", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    surv_timepoint(
      vars = "AVAL",
      var_labels = "Months",
      time_point = 9,
      is_event = "is_event",
      method = "surv_diff",
      control = control_surv_timepoint(conf_level = 0.99)
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "9 Months", "Difference in Event Free Rate",
      "99% CI", "p-value (Z-test)", "ARM A", "", "", "", "", "ARM B",
      "", "-4.34", "(-17.66, 8.98)", "0.4012", "ARM C", "", "-20.05",
      "(-34.46, -5.65)", "0.0003"
    ),
    .Dim = 5:4
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("surv_timepoint no warning when multipled layers generated", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )
  testthat::expect_silent(
    basic_table() %>%
      split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
      add_colcounts() %>%
      surv_timepoint(
        vars = "AVAL",
        var_labels = "Months",
        is_event = "is_event",
        time_point = 9,
        method = "both"
      ) %>%
      build_table(df = adtte_f)
  )
})
