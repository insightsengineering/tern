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
    pvalue = with_label(0.2584456, "p-value (log-rank)"),
    hr = with_label(0.8412573, "Hazard Ratio"),
    hr_ci = with_label(c(0.6231147, 1.1357683), "95% CI")
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
    pvalue = with_label(0.2590296, "p-value (wald)"),
    hr = with_label(0.8412573, "Hazard Ratio"),
    hr_ci = with_label(c(0.6539229, 1.0822589), "90% CI")
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
    pvalue = with_label(0.1649184, "p-value (log-rank)"),
    hr = with_label(0.7995183, "Hazard Ratio"),
    hr_ci = with_label(c(0.5826759, 1.0970585), "95% CI")
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
    pvalue = with_label(0.1657147, "p-value (wald)"),
    hr = with_label(0.7995183, "Hazard Ratio"),
    hr_ci = with_label(c(0.6130806, 1.0426518), "90% CI")
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
      "Hazard Ratio", "95% CI", "ARM A", "", "", "", "", "ARM B",
      "", "0.2584", "1.1887", "(0.8805, 1.6048)", "ARM C", "", "<0.0001",
      "2.1107", "(1.5689, 2.8397)"
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
      "Hazard Ratio", "99% CI", "ARM A", "", "", "", "", "ARM B",
      "", "0.2585", "1.1887", "(0.8012, 1.7636)", "ARM C", "", "<0.0001",
      "2.1107", "(1.4293, 3.1171)"
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
      "Hazard Ratio", "95% CI", "ARM A", "", "", "", "", "ARM B",
      "", "0.2789", "1.1825", "(0.8727, 1.6021)", "ARM C", "", "<0.0001",
      "2.0803", "(1.5453, 2.8004)"
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
      "ARM A", "", "", "", "ARM B", "", "1.251", "(0.825, 1.896)",
      "ARM C", "", "2.17", "(1.443, 3.262)"
    ),
    .Dim = c(4L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
