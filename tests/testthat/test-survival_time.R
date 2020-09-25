library(random.cdisc.data)
library(dplyr)

test_that("s_surv_time works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- s_surv_time(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event"
  )
  expected <- list(
    median = with_label(92.31187, "Median"),
    median_ci = with_label(c(71.23956, 102.68315), "95% CI"),
    quantiles = with_label(c(32.59752, 174.70006), "25% and 75%-ile"),
    range_censor = with_label(c(0.3897209, 386.0054528), "Range (censored)"),
    range_event = with_label(c(1.773525, 395.893285), "Range (event)"),
    range = with_label(c(0.3897209, 395.8932852), "Range")
  )
  expect_equal(result, expected, tolerance = 0.0000001)

})

test_that("s_surv_time works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- s_surv_time(
    adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event",
    control = control_surv_time(
      conf_level = 0.99, conf_type = "log-log", quantiles = c(0.2, 0.8)
    )
  )
  expected <- list(
    median = with_label(92.31187, "Median"),
    median_ci = with_label(c(63.26015, 117.43863), "99% CI"),
    quantiles = with_label(c(30.94608, 216.76314), "20% and 80%-ile"),
    range_censor = with_label(c(0.3897209, 386.0054528), "Range (censored)"),
    range_event = with_label(c(1.773525, 395.893285), "Range (event)"),
    range = with_label(c(0.3897209, 395.8932852), "Range")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
})

test_that("surv_time works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
    var = "ARMCD"
  ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Survival Time (Months)",
      is_event = "is_event"
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Survival Time (Months)", "Median", "  95% CI", "25% and 75%-ile", "Range (censored)", "Range (event)",
      "ARM A", "", "96.86", "(74, 125.3)", "37.5, 184.6", "0.5 to 235.8", "1.7 to 479.2",
      "ARM B", "", "92.31", "(71.2, 102.7)", "32.6, 174.7", "0.4 to 386", "1.8 to 395.9",
      "ARM C", "", "79.23", "(55.8, 107.4)", "26.3, 155.7", "1.9 to 140.5", "2.7 to 326.1"
    ),
    .Dim = c(7L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("surv_time works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
    var = "ARMCD"
  ) %>%
    surv_time(
      vars = "AVAL",
      var_labels = "Survival Time (Months)",
      is_event = "is_event",
      control = control_surv_time(conf_level = 0.9, conf_type = "log", quantiles = c(0.4, 0.6))
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Survival Time (Months)", "Median", "  90% CI", "40% and 60%-ile", "Range (censored)", "Range (event)",
      "ARM A", "", "96.86", "(75.5, 125.3)", "73.5, 125.3", "0.5 to 235.8", "1.7 to 479.2",
      "ARM B", "", "92.31", "(73.1, 102.7)", "69.4, 102.9", "0.4 to 386", "1.8 to 395.9",
      "ARM C", "", "79.23", "(57.4, 107.4)", "54.7, 117.5", "1.9 to 140.5", "2.7 to 326.1"
    ),
    .Dim = c(7L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
