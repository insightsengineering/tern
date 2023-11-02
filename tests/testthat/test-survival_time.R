testthat::test_that("s_surv_time works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_surv_time(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_surv_time works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_surv_time(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event",
    control = control_surv_time(
      conf_level = 0.99, conf_type = "log-log", quantiles = c(0.2, 0.8)
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_surv_time works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- a_surv_time(
    adtte_f,
    .df_row = df,
    .var = "AVAL",
    is_event = "is_event"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("a_surv_time works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    ) %>%
    dplyr::filter(ARMCD == "ARM B")

  result <- a_surv_time(
    adtte_f,
    .var = "AVAL",
    is_event = "is_event",
    control = control_surv_time(
      conf_level = 0.99, conf_type = "log-log", quantiles = c(0.2, 0.8)
    ),
    .df_row = adtte_f,
    .stats = c("median_ci", "quantiles", "range"),
    .formats = c(median_ci = "auto", quantiles = "xx.xx / xx.xx"),
    .labels = c(median_ci = "median conf int"),
    .indent_mods = c(median_ci = 3L)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_time works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD"
    ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Survival Time (Months)",
      is_event = "is_event"
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_time works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD") %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Survival Time (Months)",
      is_event = "is_event",
      .stats = get_stats("surv_time"),
      control = control_surv_time(conf_level = 0.9, conf_type = "log", quantiles = c(0.4, 0.6))
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("surv_time works with referential footnotes", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )
  adtte_f$is_event[adtte_f$AVAL == min(adtte_f[adtte_f$ARMCD == "ARM A", ]$AVAL) & adtte_f$ARMCD == "ARM A"] <- FALSE
  adtte_f$is_event[adtte_f$AVAL == min(adtte_f[adtte_f$ARMCD == "ARM B", ]$AVAL) & adtte_f$ARMCD == "ARM B"] <- FALSE
  adtte_f$is_event[adtte_f$AVAL == max(adtte_f[adtte_f$ARMCD == "ARM B", ]$AVAL) & adtte_f$ARMCD == "ARM B"] <- FALSE
  adtte_f$is_event[adtte_f$AVAL == max(adtte_f[adtte_f$ARMCD == "ARM C", ]$AVAL) & adtte_f$ARMCD == "ARM C"] <- FALSE

  result <- basic_table() %>%
    split_cols_by(var = "ARMCD") %>%
    add_overall_col(label = "All") %>%
    surv_time(
      vars = "AVAL",
      is_event = "is_event"
    ) %>%
    build_table(df = adtte_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
