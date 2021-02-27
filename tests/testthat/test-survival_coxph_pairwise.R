library(random.cdisc.data)
library(dplyr)

test_that("s_coxph_pairwise works with default arguments and no stratification factors", {
  adtte <- radtte(cached = TRUE)
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
    pvalue = with_label(0.03340293, "p-value (log-rank)"),
    hr = with_label(0.7173651, "Hazard Ratio"),
    hr_ci = with_label(c(0.5275231, 0.9755262), "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_coxph_pairwise works with customized arguments and no stratification factors", {
  adtte <- radtte(cached = TRUE)
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
    pvalue = with_label(0.03417907, "p-value (wald)"),
    hr = with_label(0.7173651, "Hazard Ratio"),
    hr_ci = with_label(c(0.5542485, 0.9284871), "90% CI")
  )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_coxph_pairwise works with default arguments and stratification factors", {
  adtte <- radtte(cached = TRUE)
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
    pvalue = with_label(0.03398278, "p-value (log-rank)"),
    hr = with_label(0.7057806, "Hazard Ratio"),
    hr_ci = with_label(c(0.5106793, 0.9754189), "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.00001)
})

test_that("s_coxph_pairwise works with customized arguments and stratification factors", {
  adtte <- radtte(cached = TRUE)
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
    pvalue = with_label(0.03479608, "p-value (wald)"),
    hr = with_label(0.7057806, "Hazard Ratio"),
    hr_ci = with_label(c(0.5379481, 0.9259745), "90% CI")
  )
  expect_equal(result, expected, tolerance = 0.00001)
})


test_that("coxph_pairwise works with default arguments and no stratification factors", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
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
      "0.0334", "1.394", "(1.0251, 1.8957)", "ARM C", "", "<0.0001",
      "2.7532", "(2.0478, 3.7017)"
    ),
    .Dim = 5:4
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("coxph_pairwise works with customized arguments and no stratification factors", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
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
      "0.0341", "1.394", "(0.9307, 2.0879)", "ARM C", "", "<0.0001",
      "2.7532", "(1.8659, 4.0625)"
    ),
    .Dim = 5:4
  )
  expect_identical(result_matrix, expected_matrix)
})


test_that("coxph_pairwise works with default arguments and stratification factors", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
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
      "0.0478", "1.3644", "(1.0018, 1.8582)", "ARM C", "", "<0.0001",
      "2.7277", "(2.0171, 3.6886)"
    ),
    .Dim = 5:4
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("coxph_pairwise works with customized arguments and stratification factors", {
  adtte <- radtte(cached = TRUE)
  adtte_f <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  result <- split_cols_by(
    lyt = NULL,
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

  expect_identical(result_matrix, expected_matrix)
})
