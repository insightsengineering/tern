# Preparation of the test case.
adrs <- adrs_raw

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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
})
