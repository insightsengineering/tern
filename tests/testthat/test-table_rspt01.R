# Preparation of the test case.
library(dplyr)
library(scda)

adrs <- synthetic_cdisc_data("rcd_2022_02_28")$adrs

# Select a response parameter endpoint.
adrs <- adrs %>% dplyr::filter(PARAMCD == "INVET")

# Recode multinomial response with standard oncology labels.
levels(adrs$AVALC) <- d_onco_rsp_label(levels(adrs$AVALC))

# Response variable.
adrs <- adrs %>%
  dplyr::select(ARMCD, AVALC, SEX, STRATA1, STRATA2) %>%
  dplyr::mutate(
    is_rsp = AVALC %in% c("Complete Response (CR)", "Partial Response (PR)"),
    STRATA1 = factor(STRATA1)
  )

testthat::test_that("RSPT01: 1. Best Overall Response", {
  l <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    estimate_proportion(
      vars = "is_rsp",
      table_names = "prop_est"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      var_labels = "Unstratified Analysis",
      table_names = "prop_diff"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "schouten",
      table_names = "test_diff"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      table_names = "est_or"
    ) %>%
    estimate_multinomial_response(var = "AVALC")

  result <- build_table(l, adrs)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Responders", "95% CI (Wald, with correction)",
      "Unstratified Analysis", "Difference in Response rate (%)", "95% CI (Wald, with correction)",
      "p-value (Chi-Squared Test with Schouten Correction)", "Odds Ratio (95% CI)",
      "Complete Response (CR)", "95% CI (Wald, with correction)", "Partial Response (PR)",
      "95% CI (Wald, with correction)", "Stable Disease (SD)", "95% CI (Wald, with correction)",
      "Progressive Disease (PD)", "95% CI (Wald, with correction)",
      "Not Evaluable (NE)", "95% CI (Wald, with correction)", "ARM A",
      "(N=134)", "100 (74.6%)", "(66.9, 82.4)", "", "", "", "", "",
      "60 (44.8%)", "(35.98, 53.57)", "40 (29.9%)", "(21.73, 37.97)",
      "9 (6.7%)", "(2.11, 11.33)", "24 (17.9%)", "(11.05, 24.78)",
      "1 (0.7%)", "(0.00, 2.58)", "ARM B", "(N=134)", "84 (62.7%)", "(54.1, 71.2)",
      "", "-11.9", "(-23.7, -0.2)", "0.0416", "0.57 (0.34 - 0.96)",
      "47 (35.1%)", "(26.62, 43.53)", "37 (27.6%)", "(19.67, 35.55)",
      "22 (16.4%)", "(9.77, 23.06)", "16 (11.9%)", "(6.08, 17.80)",
      "12 (9.0%)", "(3.75, 14.16)", "ARM C", "(N=132)", "81 (61.4%)",
      "(52.7, 70.0)", "", "-13.3", "(-25.1, -1.4)", "0.0245", "0.54 (0.32 - 0.91)",
      "57 (43.2%)", "(34.35, 52.01)", "24 (18.2%)", "(11.22, 25.14)",
      "13 (9.8%)", "(4.39, 15.31)", "33 (25.0%)", "(17.23, 32.77)", "5 (3.8%)",
      "(0.15, 7.42)"
    ),
    .Dim = c(19L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("RSPT01: 2. Best Overall Response (selecting sections to display)", {
  l <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    estimate_proportion(
      vars = "is_rsp",
      table_names = "prop_est"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      var_labels = "Unstratified Analysis",
      table_names = "prop_diff"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "schouten",
      table_names = "test_diff"
    ) %>%
    estimate_multinomial_response(var = "AVALC")

  result <- build_table(l, adrs)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Responders", "95% CI (Wald, with correction)",
      "Unstratified Analysis", "Difference in Response rate (%)", "95% CI (Wald, with correction)",
      "p-value (Chi-Squared Test with Schouten Correction)", "Complete Response (CR)",
      "95% CI (Wald, with correction)", "Partial Response (PR)", "95% CI (Wald, with correction)",
      "Stable Disease (SD)", "95% CI (Wald, with correction)", "Progressive Disease (PD)",
      "95% CI (Wald, with correction)", "Not Evaluable (NE)", "95% CI (Wald, with correction)",
      "ARM A", "(N=134)", "100 (74.6%)", "(66.9, 82.4)", "", "", "",
      "", "60 (44.8%)", "(35.98, 53.57)", "40 (29.9%)", "(21.73, 37.97)",
      "9 (6.7%)", "(2.11, 11.33)", "24 (17.9%)", "(11.05, 24.78)",
      "1 (0.7%)", "(0.00, 2.58)", "ARM B", "(N=134)", "84 (62.7%)", "(54.1, 71.2)",
      "", "-11.9", "(-23.7, -0.2)", "0.0416", "47 (35.1%)", "(26.62, 43.53)",
      "37 (27.6%)", "(19.67, 35.55)", "22 (16.4%)", "(9.77, 23.06)",
      "16 (11.9%)", "(6.08, 17.80)", "12 (9.0%)", "(3.75, 14.16)", "ARM C",
      "(N=132)", "81 (61.4%)", "(52.7, 70.0)", "", "-13.3", "(-25.1, -1.4)",
      "0.0245", "57 (43.2%)", "(34.35, 52.01)", "24 (18.2%)", "(11.22, 25.14)",
      "13 (9.8%)", "(4.39, 15.31)", "33 (25.0%)", "(17.23, 32.77)", "5 (3.8%)",
      "(0.15, 7.42)"
    ),
    .Dim = c(18L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("RSPT01: 3. Best Overall Response (modifying settings)", {
  conf_level <- 0.90
  method_prop <- "clopper-pearson"
  l <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    estimate_proportion(
      vars = "is_rsp",
      method = method_prop,
      conf_level = conf_level,
      table_names = "prop_est"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      var_labels = "Unstratified Analysis",
      method = "ha",
      conf_level = conf_level,
      table_names = "prop_diff"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "fisher",
      table_names = "test_diff"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      conf_level = conf_level,
      table_names = "est_or"
    ) %>%
    estimate_multinomial_response(
      var = "AVALC",
      method = method_prop,
      conf_level = conf_level
    )

  result <- build_table(l, adrs)
  result <- result[-nrow(result), ]

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Responders", "90% CI (Clopper-Pearson)",
      "Unstratified Analysis", "Difference in Response rate (%)", "90% CI (Anderson-Hauck)",
      "p-value (Fisher's Exact Test)", "Odds Ratio (90% CI)", "Complete Response (CR)",
      "90% CI (Clopper-Pearson)", "Partial Response (PR)", "90% CI (Clopper-Pearson)",
      "Stable Disease (SD)", "90% CI (Clopper-Pearson)", "Progressive Disease (PD)",
      "90% CI (Clopper-Pearson)", "Not Evaluable (NE)", "ARM A", "(N=134)",
      "100 (74.6%)", "(67.7, 80.7)", "", "", "", "", "", "60 (44.8%)",
      "(37.48, 52.25)", "40 (29.9%)", "(23.36, 37.02)", "9 (6.7%)",
      "(3.55, 11.43)", "24 (17.9%)", "(12.67, 24.25)", "1 (0.7%)",
      "ARM B", "(N=134)", "84 (62.7%)", "(55.3, 69.7)", "", "-11.9",
      "(-21.6, -2.3)", "0.0479", "0.57 (0.37 - 0.89)", "47 (35.1%)",
      "(28.22, 42.43)", "37 (27.6%)", "(21.31, 34.67)", "22 (16.4%)",
      "(11.38, 22.61)", "16 (11.9%)", "(7.63, 17.57)", "12 (9.0%)", "ARM C",
      "(N=132)", "81 (61.4%)", "(53.9, 68.5)", "", "-13.3", "(-23.0, -3.5)",
      "0.0253", "0.54 (0.35 - 0.84)", "57 (43.2%)", "(35.88, 50.71)",
      "24 (18.2%)", "(12.87, 24.61)", "13 (9.8%)", "(5.92, 15.20)",
      "33 (25.0%)", "(18.90, 31.97)", "5 (3.8%)"
    ),
    .Dim = c(18L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("RSPT01: 4. Best Overall Response (with stratified analysis)", {
  l <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    estimate_proportion(
      vars = "is_rsp",
      table_names = "prop_est"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      var_labels = "Unstratified Analysis",
      table_names = "prop_diff"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "schouten",
      table_names = "test_diff"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      table_names = "est_or"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      variables = list(strata = "STRATA1"),
      method = "cmh",
      var_labels = "Stratified Analysis",
      table_names = "prop_diff_strat"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "cmh",
      variables = list(strata = "STRATA1"),
      table_names = "test_diff_strat"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      variables = list(arm = "ARMCD", strata = "STRATA1"),
      table_names = "est_or_strat"
    ) %>%
    estimate_multinomial_response(var = "AVALC")

  result <- build_table(l, adrs)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Responders", "95% CI (Wald, with correction)",
      "Unstratified Analysis", "Difference in Response rate (%)", "95% CI (Wald, with correction)",
      "p-value (Chi-Squared Test with Schouten Correction)", "Odds Ratio (95% CI)",
      "Stratified Analysis", "Difference in Response rate (%)", "95% CI (CMH, without correction)",
      "p-value (Cochran-Mantel-Haenszel Test)", "Odds Ratio (95% CI)",
      "Complete Response (CR)", "95% CI (Wald, with correction)", "Partial Response (PR)",
      "95% CI (Wald, with correction)", "Stable Disease (SD)", "95% CI (Wald, with correction)",
      "Progressive Disease (PD)", "95% CI (Wald, with correction)",
      "Not Evaluable (NE)", "95% CI (Wald, with correction)", "ARM A",
      "(N=134)", "100 (74.6%)", "(66.9, 82.4)", "", "", "", "", "",
      "", "", "", "", "", "60 (44.8%)", "(35.98, 53.57)", "40 (29.9%)",
      "(21.73, 37.97)", "9 (6.7%)", "(2.11, 11.33)", "24 (17.9%)",
      "(11.05, 24.78)", "1 (0.7%)", "(0.00, 2.58)", "ARM B", "(N=134)",
      "84 (62.7%)", "(54.1, 71.2)", "", "-11.9", "(-23.7, -0.2)", "0.0416",
      "0.57 (0.34 - 0.96)", "", "-11.9", "(-22.7, -1.0)", "0.0366", "0.57 (0.34 - 0.96)",
      "47 (35.1%)", "(26.62, 43.53)", "37 (27.6%)", "(19.67, 35.55)",
      "22 (16.4%)", "(9.77, 23.06)", "16 (11.9%)", "(6.08, 17.80)",
      "12 (9.0%)", "(3.75, 14.16)", "ARM C", "(N=132)", "81 (61.4%)",
      "(52.7, 70.0)", "", "-13.3", "(-25.1, -1.4)", "0.0245", "0.54 (0.32 - 0.91)",
      "", "-13.5", "(-24.5, -2.5)", "0.0180", "0.54 (0.32 - 0.90)",
      "57 (43.2%)", "(34.35, 52.01)", "24 (18.2%)", "(11.22, 25.14)",
      "13 (9.8%)", "(4.39, 15.31)", "33 (25.0%)", "(17.23, 32.77)", "5 (3.8%)",
      "(0.15, 7.42)"
    ),
    .Dim = c(24L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("RSPT01: 5. Best Overall Response (modifying the definition of overall response)", {
  l <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    estimate_proportion(
      vars = "is_rsp",
      table_names = "prop_est"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      var_labels = "Unstratified Analysis",
      table_names = "prop_diff"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "schouten",
      table_names = "test_diff"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      table_names = "est_or"
    ) %>%
    estimate_multinomial_response(var = "AVALC")

  adrs$is_rsp <- adrs$AVALC == "Complete Response (CR)"
  result <- build_table(l, adrs)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Responders", "95% CI (Wald, with correction)",
      "Unstratified Analysis", "Difference in Response rate (%)", "95% CI (Wald, with correction)",
      "p-value (Chi-Squared Test with Schouten Correction)", "Odds Ratio (95% CI)",
      "Complete Response (CR)", "95% CI (Wald, with correction)", "Partial Response (PR)",
      "95% CI (Wald, with correction)", "Stable Disease (SD)", "95% CI (Wald, with correction)",
      "Progressive Disease (PD)", "95% CI (Wald, with correction)",
      "Not Evaluable (NE)", "95% CI (Wald, with correction)", "ARM A",
      "(N=134)", "60 (44.8%)", "(36.0, 53.6)", "", "", "", "", "", "60 (44.8%)",
      "(35.98, 53.57)", "40 (29.9%)", "(21.73, 37.97)", "9 (6.7%)",
      "(2.11, 11.33)", "24 (17.9%)", "(11.05, 24.78)", "1 (0.7%)",
      "(0.00, 2.58)", "ARM B", "(N=134)", "47 (35.1%)", "(26.6, 43.5)",
      "", "-9.7", "(-22.1, 2.7)", "0.1197", "0.67 (0.41 - 1.09)", "47 (35.1%)",
      "(26.62, 43.53)", "37 (27.6%)", "(19.67, 35.55)", "22 (16.4%)",
      "(9.77, 23.06)", "16 (11.9%)", "(6.08, 17.80)", "12 (9.0%)", "(3.75, 14.16)",
      "ARM C", "(N=132)", "57 (43.2%)", "(34.4, 52.0)", "", "-1.6", "(-14.3, 11.1)",
      "0.8413", "0.94 (0.58 - 1.52)", "57 (43.2%)", "(34.35, 52.01)",
      "24 (18.2%)", "(11.22, 25.14)", "13 (9.8%)", "(4.39, 15.31)",
      "33 (25.0%)", "(17.23, 32.77)", "5 (3.8%)", "(0.15, 7.42)"
    ),
    .Dim = c(19L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("RSPT01: 6. Best Overall Response (define new sections to display)", {

  # Define study-specific response category "No Progression".
  adrs <- adrs %>%
    dplyr::mutate(
      AVALC_NEW = dplyr::case_when(
        AVALC %in% c(
          "Complete Response (CR)", "Partial Response (PR)",
          "Stable Disease (SD)"
        ) ~ "No Progression",
        TRUE ~ as.character(AVALC)
      )
    ) %>%
    dplyr::mutate(
      AVALC_NEW = factor(
        AVALC_NEW,
        levels = c("No Progression", "Progressive Disease (PD)", "Not Evaluable (NE)")
      )
    )

  l <- basic_table() %>%
    split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
    add_colcounts() %>%
    estimate_proportion(
      vars = "is_rsp",
      table_names = "prop_est"
    ) %>%
    estimate_proportion_diff(
      vars = "is_rsp",
      show_labels = "visible",
      var_labels = "Unstratified Analysis",
      table_names = "prop_diff"
    ) %>%
    test_proportion_diff(
      vars = "is_rsp",
      method = "schouten",
      table_names = "test_diff"
    ) %>%
    estimate_odds_ratio(
      vars = "is_rsp",
      table_names = "est_or"
    ) %>%
    estimate_multinomial_response(var = "AVALC_NEW")

  result <- build_table(l, adrs)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Responders", "95% CI (Wald, with correction)",
      "Unstratified Analysis", "Difference in Response rate (%)", "95% CI (Wald, with correction)",
      "p-value (Chi-Squared Test with Schouten Correction)", "Odds Ratio (95% CI)",
      "No Progression", "95% CI (Wald, with correction)", "Progressive Disease (PD)",
      "95% CI (Wald, with correction)", "Not Evaluable (NE)", "95% CI (Wald, with correction)",
      "ARM A", "(N=134)", "100 (74.6%)", "(66.9, 82.4)", "", "", "",
      "", "", "109 (81.3%)", "(74.37, 88.31)", "24 (17.9%)", "(11.05, 24.78)",
      "1 (0.7%)", "(0.00, 2.58)", "ARM B", "(N=134)", "84 (62.7%)", "(54.1, 71.2)",
      "", "-11.9", "(-23.7, -0.2)", "0.0416", "0.57 (0.34 - 0.96)",
      "106 (79.1%)", "(71.85, 86.36)", "16 (11.9%)", "(6.08, 17.80)",
      "12 (9.0%)", "(3.75, 14.16)", "ARM C", "(N=132)", "81 (61.4%)",
      "(52.7, 70.0)", "", "-13.3", "(-25.1, -1.4)", "0.0245", "0.54 (0.32 - 0.91)",
      "94 (71.2%)", "(63.11, 79.31)", "33 (25.0%)", "(17.23, 32.77)",
      "5 (3.8%)", "(0.15, 7.42)"
    ),
    .Dim = c(15L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
