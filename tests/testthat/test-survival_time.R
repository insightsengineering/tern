library(scda)
library(dplyr)

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("s_surv_time works with default arguments", {
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
    median = formatters::with_label(27.5130353437858, label = "Median"),
    median_ci = formatters::with_label(c(17.336045529175, 30.2169178803838), label = "95% CI"),
    quantiles = formatters::with_label(c(9.49817068050519, 54.8903290286298), label = "25% and 75%-ile"),
    range_censor = formatters::with_label(c(0.864396308007549, 91.0033832489784), label = "Range (censored)"),
    range_event = formatters::with_label(c(0.0104370446088212, 122.357382022329), label = "Range (event)"),
    range = formatters::with_label(c(0.0104370446088212, 122.357382022329), label = "Range")
  )
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})

testthat::test_that("s_surv_time works with customized arguments", {
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
    median = formatters::with_label(27.5130353437858, label = "Median"),
    median_ci = formatters::with_label(c(15.7210381999671, 35.7398274871358), label = "99% CI"),
    quantiles = formatters::with_label(c(8.49943279711687, 61.6015017964328), label = "20% and 80%-ile"),
    range_censor = formatters::with_label(c(0.864396308007549, 91.0033832489784), label = "Range (censored)"),
    range_event = formatters::with_label(c(0.0104370446088212, 122.357382022329), label = "Range (event)"),
    range = formatters::with_label(c(0.0104370446088212, 122.357382022329), label = "Range")
  )
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})

testthat::test_that("surv_time works with default arguments", {
  adtte_f <- adtte %>%
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Survival Time (Months)", "Median", "95% CI",
      "25% and 75%-ile", "Range (censored)", "Range (event)", "ARM A",
      "", "41.4", "(27.7, 54.7)", "15.4, 75.2", "0.4 to 154.7", "0.3 to 116.4",
      "ARM B", "", "27.5", "(17.3, 30.2)", "9.5, 54.9", "0.9 to 91.0",
      "0.0 to 122.4", "ARM C", "", "11.1", "(9.6, 15.9)", "5.3, 25.2",
      "0.3 to 49.4", "0.1 to 101.6"
    ),
    .Dim = c(7L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("surv_time works with customized arguments", {
  adtte_f <- adtte %>%
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
      control = control_surv_time(conf_level = 0.9, conf_type = "log", quantiles = c(0.4, 0.6))
    ) %>%
    build_table(df = adtte_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Survival Time (Months)", "Median", "90% CI",
      "40% and 60%-ile", "Range (censored)", "Range (event)", "ARM A",
      "", "41.4", "(32.4, 54.1)", "27.7, 54.7", "0.4 to 154.7", "0.3 to 116.4",
      "ARM B", "", "27.5", "(20.0, 30.2)", "16.3, 32.4", "0.9 to 91.0",
      "0.0 to 122.4", "ARM C", "", "11.1", "(9.7, 15.6)", "9.2, 15.9",
      "0.3 to 49.4", "0.1 to 101.6"
    ),
    .Dim = c(7L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
