testthat::test_that("s_surv_timepoint works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_surv_timepoint works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_surv_timepoint also works when there are 0 patients at risk", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_timepoint works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_timepoint works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_surv_timepoint_diff works with default arguments for comparison group", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_surv_timepoint_diff works with customized arguments for comparison arm", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_timepoint for survival diff works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_timepoint for survival diff works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_timepoint works with method = both", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )
  result <- testthat::expect_silent(
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
  testthat::expect_snapshot(result)
})
