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
    is_event = "is_event",
    control = control_surv_time(),
    ref_fn_censor = TRUE
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
    ref_fn_censor = TRUE,
    .stats = c("median_ci", "quantiles", "range"),
    .formats = c(median_ci = "xx.xx / xx.xx", quantiles = "xx.xx / xx.xx"),
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
      .formats = list(median_ci = "auto"),
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

testthat::test_that("a_surv_time works when `is_event` only has TRUE observations", {
  anl <- tibble::tribble(
    ~AVAL, ~ARM, ~is_event,
    1, "A", TRUE,
    2, "A", TRUE
  )

  testthat::expect_silent(
    tern::a_surv_time(
      anl,
      .var = "AVAL",
      is_event = "is_event",
      control = control_surv_time(
        conf_level = 0.99, conf_type = "log-log", quantiles = c(0.2, 0.8)
      ),
      ref_fn_censor = TRUE
    )
  )
})

testthat::test_that("a_surv_time works when `is_event` only has FALSE observations", {
  anl <- tibble::tribble(
    ~AVAL, ~ARM, ~is_event,
    1, "A", FALSE,
    2, "A", FALSE
  )

  testthat::expect_silent(
    tern::a_surv_time(
      anl,
      .var = "AVAL",
      is_event = "is_event",
      control = control_surv_time(
        conf_level = 0.99, conf_type = "log-log", quantiles = c(0.2, 0.8)
      ),
      ref_fn_censor = TRUE
    )
  )
})

testthat::test_that("s_surv_time includes range_with_cens_info with no censoring at boundaries (0, 0)", {
  # All observations are events — no censoring at either boundary
  anl <- tibble::tribble(
    ~AVAL, ~is_event,
    2,     TRUE,
    5,     TRUE,
    8,     TRUE
  )
  result <- s_surv_time(anl, .var = "AVAL", is_event = "is_event")
  testthat::expect_named(result, c(
    "median", "median_ci", "quantiles", "range_censor", "range_event",
    "range", "median_ci_3d", "quantiles_lower", "quantiles_upper", "range_with_cens_info"
  ))
  rwci <- result$range_with_cens_info
  testthat::expect_equal(rwci[1:2], c(2, 8))
  testthat::expect_equal(as.numeric(rwci[3:4]), c(0, 0))
})

testthat::test_that("s_surv_time range_with_cens_info flags upper censored (0, 1)", {
  # Censored observation has the largest value — upper boundary is censored
  anl <- tibble::tribble(
    ~AVAL, ~is_event,
    2,     TRUE,
    5,     TRUE,
    10,    FALSE   # censored at max
  )
  result <- s_surv_time(anl, .var = "AVAL", is_event = "is_event")
  rwci <- result$range_with_cens_info
  testthat::expect_equal(as.numeric(rwci[3]), 0)  # lower not censored
  testthat::expect_equal(as.numeric(rwci[4]), 1)  # upper censored
})

testthat::test_that("s_surv_time range_with_cens_info flags lower censored (1, 0)", {
  # Censored observation has the smallest value — lower boundary is censored
  anl <- tibble::tribble(
    ~AVAL, ~is_event,
    1,     FALSE,  # censored at min
    5,     TRUE,
    8,     TRUE
  )
  result <- s_surv_time(anl, .var = "AVAL", is_event = "is_event")
  rwci <- result$range_with_cens_info
  testthat::expect_equal(as.numeric(rwci[3]), 1)  # lower censored
  testthat::expect_equal(as.numeric(rwci[4]), 0)  # upper not censored
})

testthat::test_that("s_surv_time range_with_cens_info flags both bounds censored when all censored (1, 1)", {
  # All observations are censored
  anl <- tibble::tribble(
    ~AVAL, ~is_event,
    2,     FALSE,
    5,     FALSE,
    8,     FALSE
  )
  result <- s_surv_time(anl, .var = "AVAL", is_event = "is_event")
  rwci <- result$range_with_cens_info
  testthat::expect_equal(as.numeric(rwci[3:4]), c(1, 1))
})

testthat::test_that("format_range_cens produces correct strings", {
  testthat::expect_equal(format_range_cens(c(1.2, 8.5, 0, 0)), "1.2 to 8.5")
  testthat::expect_equal(format_range_cens(c(1.2, 8.5, 1, 0)), "1.2+ to 8.5")
  testthat::expect_equal(format_range_cens(c(1.2, 8.5, 0, 1)), "1.2 to 8.5+")
  testthat::expect_equal(format_range_cens(c(1.2, 8.5, 1, 1)), "1.2+ to 8.5+")
})
