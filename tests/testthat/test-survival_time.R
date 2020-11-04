library(random.cdisc.data)
library(dplyr)

test_that("s_surv_time works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
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
  expected <- list(
    median = with_label(26.72943, "Median"),
    median_ci = with_label(c(20.00754, 36.14036), "95% CI"),
    quantiles = with_label(c(12.55214, 48.49524), "25% and 75%-ile"),
    range_censor = with_label(c(2.813063, 130.655977), "Range (censored)"),
    range_event = with_label(c(0.3034908, 155.8429981), "Range (event)"),
    range = with_label(c(0.3034908, 155.8429981), "Range")
  )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_surv_time works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
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
  expected <- list(
    median = with_label(26.72943, "Median"),
    median_ci = with_label(c(17.33605, 38.75602), "99% CI"),
    quantiles = with_label(c(11.14121, 53.29873), "20% and 80%-ile"),
    range_censor = with_label(c(2.813063, 130.655977), "Range (censored)"),
    range_event = with_label(c(0.3034908, 155.8429981), "Range (event)"),
    range = with_label(c(0.3034908, 155.8429981), "Range")
  )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("surv_time works with default arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

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
      "", "Survival Time (Months)", "Median", "95% CI",
      "25% and 75%-ile", "Range (censored)", "Range (event)", "ARM A",
      "", "33.19", "(31.3, 41.9)", "14.7, 57.8", "0.2 to 109.1", "0.4 to 151.3",
      "ARM B", "", "26.73", "(20, 36.1)", "12.6, 48.5", "2.8 to 130.7",
      "0.3 to 155.8", "ARM C", "", "14.82", "(10.9, 22.6)", "6, 33.3",
      "1.1 to 81.9", "0.2 to 72"
    ),
    .Dim = c(7L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("surv_time works with customized arguments", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = day2month(AVAL),
      is_event = CNSR == 0
    )

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
      "", "Survival Time (Months)", "Median", "90% CI",
      "40% and 60%-ile", "Range (censored)", "Range (event)", "ARM A",
      "", "33.19", "(31.4, 41.9)", "27.9, 41.9", "0.2 to 109.1", "0.4 to 151.3",
      "ARM B", "", "26.73", "(22.2, 34.8)", "19.4, 37.3", "2.8 to 130.7",
      "0.3 to 155.8", "ARM C", "", "14.82", "(11.6, 22.5)", "10.3, 23",
      "1.1 to 81.9", "0.2 to 72"
    ),
    .Dim = c(7L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
