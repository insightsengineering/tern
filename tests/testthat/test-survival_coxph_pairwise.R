library(scda)
library(dplyr)

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("s_coxph_pairwise works with default arguments and no stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strat = NULL
  )

  expected <- list(
    pvalue = formatters::with_label(0.03340293, "p-value (log-rank)"),
    hr = formatters::with_label(0.7173651, "Hazard Ratio"),
    hr_ci = formatters::with_label(c(0.5275231, 0.9755262), "95% CI"),
    n_tot = formatters::with_label(268L, "Total n"),
    n_tot_events = formatters::with_label(166L, "Total events")
  )
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})

testthat::test_that("s_coxph_pairwise works with customized arguments and no stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strat = NULL,
    control = control_coxph(pval_method = "wald", ties = "breslow", conf_level = 0.9)
  )

  expected <- list(
    pvalue = formatters::with_label(0.03417907, "p-value (wald)"),
    hr = formatters::with_label(0.7173651, "Hazard Ratio"),
    hr_ci = formatters::with_label(c(0.5542485, 0.9284871), "90% CI"),
    n_tot = formatters::with_label(268L, "Total n"),
    n_tot_events = formatters::with_label(166L, "Total events")
  )
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})

testthat::test_that("s_coxph_pairwise works with default arguments and stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strat = c("SEX", "RACE")
  )

  expected <- list(
    pvalue = formatters::with_label(0.03398278, "p-value (log-rank)"),
    hr = formatters::with_label(0.7057806, "Hazard Ratio"),
    hr_ci = formatters::with_label(c(0.5106793, 0.9754189), "95% CI"),
    n_tot = formatters::with_label(268L, "Total n"),
    n_tot_events = formatters::with_label(166L, "Total events")
  )
  testthat::expect_equal(result, expected, tolerance = 0.00001)
})

testthat::test_that("s_coxph_pairwise works with customized arguments and stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  df <- adtte_f %>% dplyr::filter(ARMCD == "ARM A")
  df_ref <- adtte_f %>% dplyr::filter(ARMCD == "ARM B")

  result <- s_coxph_pairwise(
    df = df,
    .ref_group = df_ref,
    .in_ref_col = FALSE,
    .var = "AVAL",
    is_event = "is_event",
    strat = c("SEX", "RACE"),
    control = control_coxph(pval_method = "wald", ties = "breslow", conf_level = 0.9)
  )

  expected <- list(
    pvalue = formatters::with_label(0.03479608, "p-value (wald)"),
    hr = formatters::with_label(0.7057806, "Hazard Ratio"),
    hr_ci = formatters::with_label(c(0.5379481, 0.9259745), "90% CI"),
    n_tot = formatters::with_label(268L, "Total n"),
    n_tot_events = formatters::with_label(166L, "Total events")
  )
  testthat::expect_equal(result, expected, tolerance = 0.00001)
})


testthat::test_that("coxph_pairwise works with default arguments and no stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      strat = NULL
    ) %>%
    build_table(df = adtte_f)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Unstratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "ARM A", "", "", "", "", "ARM B", "",
      "0.0334", "1.39", "(1.03, 1.90)", "ARM C", "", "<0.0001",
      "2.75", "(2.05, 3.70)"
    ),
    .Dim = 5:4
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("coxph_pairwise works with customized arguments and no stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Unstratified Analysis"),
      control = control_coxph(pval_method = "likelihood", conf_level = 0.99),
      strat = NULL
    ) %>%
    build_table(df = adtte_f)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Unstratified Analysis", "p-value (likelihood)",
      "Hazard Ratio", "99% CI", "ARM A", "", "", "", "", "ARM B", "",
      "0.0341", "1.39", "(0.93, 2.09)", "ARM C", "", "<0.0001",
      "2.75", "(1.87, 4.06)"
    ),
    .Dim = 5:4
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("coxph_pairwise works with default arguments and stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Stratified Analysis"),
      strat = "SEX"
    ) %>%
    build_table(df = adtte_f)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Stratified Analysis", "p-value (log-rank)",
      "Hazard Ratio", "95% CI", "ARM A", "", "", "", "", "ARM B", "",
      "0.0478", "1.36", "(1.00, 1.86)", "ARM C", "", "<0.0001",
      "2.73", "(2.02, 3.69)"
    ),
    .Dim = 5:4
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("coxph_pairwise works with customized arguments and stratification factors", {
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- basic_table() %>%
    split_cols_by(
      var = "ARMCD",
      ref_group = "ARM A"
    ) %>%
    coxph_pairwise(
      vars = "AVAL",
      is_event = "is_event",
      var_labels = c("Stratified Analysis"),
      control = control_coxph(pval_method = "likelihood", conf_level = 0.99),
      strat = c("SEX", "RACE"),
      .stats = c("hr", "hr_ci"),
      .formats = c(hr = "xx.xxx", hr_ci = "(xx.xxx, xx.xxx)")
    ) %>%
    build_table(df = adtte_f)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Stratified Analysis", "Hazard Ratio", "99% CI",
      "ARM A", "", "", "", "ARM B", "", "1.417", "(0.926, 2.168)",
      "ARM C", "", "2.775", "(1.827, 4.216)"
    ),
    .Dim = c(4L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
