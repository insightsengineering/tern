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
    pvalue = with_label(0.8511568, "p-value (log-rank)"),
    hr = with_label(0.9720615, "Hazard Ratio"),
    hr_ci = with_label(c(0.7230167, 1.3068904), "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
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
    pvalue = with_label(0.8511615, "p-value (wald)"),
    hr = with_label(0.9720615, "Hazard Ratio"),
    hr_ci = with_label(c(0.7582545, 1.2461562), "90% CI")
  )
  expect_equal(result, expected, tolerance = 0.0000001)
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
    pvalue = with_label(0.9142032, "p-value (log-rank)"),
    hr = with_label(1.01703, "Hazard Ratio"),
    hr_ci = with_label(c(0.7480246, 1.3827760), "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.000001)
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
    pvalue = with_label(0.9142041, "p-value (wald)"),
    hr = with_label(1.01703, "Hazard Ratio"),
    hr_ci = with_label(c(0.7858976, 1.3161390), "90% CI")
  )
  expect_equal(result, expected, tolerance = 0.000001)
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
      "", "Unstratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "ARM A", "", "", "", "",
      "ARM B", "", "0.8512", "1.0287", "(0.7652, 1.3831)",
      "ARM C", "", "0.0918", "1.2955", "(0.958, 1.7519)"
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
      "", "Unstratified Analysis", "p-value (likelihood)", "Hazard Ratio", " 99% CI",
      "ARM A", "", "", "", "",
      "ARM B", "", "0.8511", "1.0287", "(0.6972, 1.5179)",
      "ARM C", "", "0.0925", "1.2955", "(0.8713, 1.9262)"
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
      "", "Stratified Analysis", "p-value (log-rank)", "Hazard Ratio", " 95% CI",
      "ARM A", "", "", "", "",
      "ARM B", "", "0.8589", "1.0273", "(0.7632, 1.3829)",
      "ARM C", "", "0.1222", "1.2792", "(0.9355, 1.7493)"
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
      "", "Stratified Analysis", "Hazard Ratio", " 99% CI",
      "ARM A", "", "", "",
      "ARM B", "", "0.983", "(0.657, 1.472)",
      "ARM C", "", "1.273", "(0.833, 1.944)"
    ),
    .Dim = c(4L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
