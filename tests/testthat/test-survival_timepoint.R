library(random.cdisc.data)
library(dplyr)

test_that("s_surv_timepoint works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_surv_timepoint(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event"
  )
  expected <- list(
    pt_at_risk = with_label(114, "Patients remaining at risk"),
    event_free_rate = with_label(89.33188, "Event Free Rate (%)"),
    rate_se = with_label(2.69695, "Standard Error of Event Free Rate"),
    rate_ci = with_label(c(84.04595, 94.61780), "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
})


test_that("s_surv_timepoint works with customized arguments", {
  adtte <- radtte(cached = TRUE)
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
    control = control_surv_timepoint(
      conf_level = 0.99, conf_type = "log", time_point = 7
    )
  )
  expected <- list(
    pt_at_risk = with_label(85, "Patients remaining at risk"),
    event_free_rate = with_label(69.31139, "Event Free Rate (%)"),
    rate_se = with_label(4.102859, "Standard Error of Event Free Rate"),
    rate_ci = with_label(c(59.50938, 80.72791), "99% CI")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
})

test_that("surv_timepoint works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

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
      "", "6 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "  95% CI", "ARM A", "", "107", "89.74", "(84.44, 95.03)", "ARM B",
      "", "114", "89.33", "(84.05, 94.62)", "ARM C", "", "92", "75.02",
      "(67.51, 82.53)"
    ),
    .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("surv_timepoint works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

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
      "", "8 Months", "Patients remaining at risk", "Event Free Rate (%)",
      "  90% CI", "ARM A", "", "103", "88.87", "(83.25, 92.69)", "ARM B",
      "", "107", "85.36", "(79.37, 89.73)", "ARM C", "", "79", "66.01",
      "(58.55, 72.45)"
    ),
    .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("s_surv_timepoint_diff works with default arguments for comparison group", {
  adtte <- radtte(cached = TRUE)
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
    is_event = "is_event"
  )
  expected <- list(
    rate_diff = with_label(0.4049929, "Difference in Event Free Rate"),
    rate_diff_ci = with_label(c(-7.077293, 7.887279), "95% CI"),
    ztest_pval = with_label(0.9155135, "p-value (Z-test)")
  )
  expect_equal(result, expected, tolerance = 0.000001)
})


test_that("s_surv_timepoint_diff works with customized arguments for comparison arm", {
  adtte <- radtte(cached = TRUE)
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
    control = control_surv_timepoint(time_point = 8),
    conf_level = 0.9
  )
  expected <- list(
    rate_diff = with_label(3.509382, "Difference in Event Free Rate"),
    rate_diff_ci = with_label(c(-3.380986, 10.399750), "90% CI"),
    ztest_pval = with_label(0.40217, "p-value (Z-test)")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
})

test_that("surv_timepoint_diff works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

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
      "", "6 Months", "  Difference in Event Free Rate",
      "    95% CI", "    p-value (Z-test)", "ARM A", "", "", "", "",
      "ARM B", "", "-2.64", "(-10.99, 5.71)", "0.5357", "ARM C", "",
      "-22.83", "(-32.94, -12.72)", "<0.0001"
    ),
    .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("surv_timepoint_diff works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

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
      "", "9 Months", "  Difference in Event Free Rate",
      "    99% CI", "    p-value (Z-test)", "ARM A", "", "", "", "",
      "ARM B", "", "-2.64", "(-13.61, 8.33)", "0.5357", "ARM C", "",
      "-22.83", "(-36.11, -9.54)", "<0.0001"
    ),
    .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)
})
