library(random.cdisc.data)

preprocess_adrs <- function(adrs, n_records = 20) {

  adrs_labels <- var_labels(adrs)
  adrs <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
    dplyr::slice(1:n_records) %>%
    droplevels() %>%
    dplyr::mutate(
      # Reorder levels of factor to make the placebo group the reference arm.
      ARM = forcats::fct_relevel(ARM, "B: Placebo"),
      rsp = AVALC == "CR"
    )
  var_labels(adrs) <- c(adrs_labels, "Response")

  adrs
}

test_that("h_proportion_df functions as expected with valid input and default arguments", {

  # Typical case.
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  expected <- data.frame(
    arm = factor(c("B", "A"), levels = c("B", "A")),
    n = c(2, 4),
    n_rsp = c(1, 1),
    prop = c(0.5, 0.25),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)

  # Edge case: 0 responses in one group.
  rsp <- c(TRUE, FALSE, FALSE, FALSE)
  arm <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  expected <- data.frame(
    arm = factor(c("A", "B"), levels = c("A", "B")),
    n = c(2, 2),
    n_rsp = c(1, 0),
    prop = c(0.5, 0),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)

})

test_that("h_proportion_df fails with wrong input", {

  expect_error(h_proportion_df(
    rsp = c(TRUE, FALSE, NA),
    arm = factor(c("A", "B", "A"), levels = c("B", "A"))
  ))

})

test_that("h_proportion_subgroups_df functions as expected with valid input and default arguments", {

  adrs <- radrs(cached = TRUE) %>%
    preprocess_adrs()

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- data.frame(
    arm = factor(rep(c("B: Placebo", "A: Drug X"), 4), levels = c("B: Placebo", "A: Drug X")),
    n = c(3, 5, 4, 8, 2, 9, 5, 4),
    n_rsp = c(1, 5, 3, 6, 2, 8, 2, 3),
    prop = c(0.3333333, 1, 0.75, 0.75, 1, 0.8888889, 0.4, 0.75),
    subgroup = c("F", "F", "M", "M", "S1", "S1", "S2", "S2"),
    var = c(rep("SEX", 4), rep("STRATA2", 4)),
    var_label = c(rep("Sex", 4), rep("Stratification Factor 2", 4)),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_odds_ratio_df functions as expected with valid input and default arguments", {

  result <- h_odds_ratio_df(
    c(TRUE, FALSE, FALSE, TRUE),
    arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 4,
    or = 1,
    lcl = 0.01984252,
    ucl = 50.39681,
    conf_level = 0.95,
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_odds_ratio_df functions as expected with valid input and non-default arguments", {

  adrs <- radrs(cached = TRUE) %>%
    preprocess_adrs(n_records = 100)

  result <- h_odds_ratio_df(
    rsp = adrs$rsp,
    arm = adrs$ARM,
    conf_level = 0.9,
    method = "chisq"
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 100,
    or = 2.538461,
    lcl = 0.9745661,
    ucl = 6.611955,
    conf_level = 0.9,
    pval = 0.1017069,
    pval_label = "p-value (Chi-Squared Test)",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("h_odds_ratio_subgroups_df functions as expected with valid input and default arguments", {

  adrs <- radrs(cached = TRUE) %>%
    preprocess_adrs(n_records = 100)

  result <- h_odds_ratio_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- data.frame(
    arm = rep(" ", 4),
    n_tot = c(56L, 44L, 48L, 52L),
    or = c(4.363636, 1.235294, 2.083333, 3.043478),
    lcl = c(0.8347243, 0.2204647, 0.4110081, 0.5663254),
    ucl = c(22.811510, 6.921525, 10.560077, 16.355893),
    conf_level = 0.95,
    subgroup = c("F", "M", "S1", "S2"),
    var = c("SEX", "SEX", "STRATA2", "STRATA2"),
    var_label = rep(c("Sex", "Stratification Factor 2"), each = 2),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("extract_rsp_subgroups functions as expected with valid input and default arguments", {

  adrs <- radrs(cached = TRUE) %>%
    preprocess_adrs(n_records = 100)

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- list(
    prop = data.frame(
      arm = factor(rep(c("B: Placebo", "A: Drug X"), 4), levels = c("B: Placebo", "A: Drug X")),
      n = c(30, 26, 20, 24, 20, 28, 30, 22),
      n_rsp = c(22, 24, 17, 21, 16, 25, 23, 20),
      prop = c(0.7333333, 0.9230769, 0.85, 0.875, 0.8, 0.8928571, 0.7666667, 0.9090909),
      subgroup = c("F", "F", "M", "M", "S1", "S1", "S2", "S2"),
      var = c(rep("SEX", 4), rep("STRATA2", 4)),
      var_label = c(rep("Sex", 4), rep("Stratification Factor 2", 4)),
      stringsAsFactors = FALSE
    ),
    or = data.frame(
      arm = rep(" ", 4),
      n_tot = c(56L, 44L, 48L, 52L),
      or = c(4.363636, 1.235294, 2.083333, 3.043478),
      lcl = c(0.8347243, 0.2204647, 0.4110081, 0.5663254),
      ucl = c(22.811510, 6.921525, 10.560077, 16.355893),
      conf_level = 0.95,
      subgroup = c("F", "M", "S1", "S2"),
      var = c("SEX", "SEX", "STRATA2", "STRATA2"),
      var_label = rep(c("Sex", "Stratification Factor 2"), each = 2),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(result, expected, tol = 0.000001)

})

test_that("a_response_subgroups functions as expected with valid input", {

  df <- data.frame(
    prop = c(0.1234, 0.5678),
    pval = c(0.00001, 0.983758),
    subgroup = c("M", "F"),
    stringsAsFactors = FALSE
  )

  afun <- a_response_subgroups(.formats = c(prop = "xx.xx", pval = "x.xxxx | (<0.0001)"))

  result <- basic_table() %>%
    split_cols_by_multivar(c("prop", "pval")) %>%
    analyze_colvars(afun) %>%
    build_table(df)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "M", "F", "prop", "0.12", "0.57", "pval", "<0.0001",
      "0.9838"),
    .Dim = c(3L, 3L)
    )
  expect_equal(result_matrix, expected_matrix)

})

test_that("tabulate_rsp_subgroups functions as expected with valid input", {

  adrs <- radrs(cached = TRUE) %>%
    preprocess_adrs(n_records = 200)

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs,
    method = "chisq",
    conf_level = 0.95
  )
  df_prop <- df$prop
  df_or <- df$or

  # Response table.
  result <- basic_table() %>%
    tabulate_rsp_subgroups(vars = c("n", "prop")) %>%
    build_table(df_prop)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Sex", "F", "M", "Stratification Factor 2",
      "S1", "S2", "B: Placebo", "n", "", "62", "38", "", "48", "52",
      "B: Placebo", "Response (%)", "", "64.5%", "81.6%", "", "70.8%",
      "71.2%", "A: Drug X", "n", "", "58", "42", "", "57", "43", "A: Drug X",
      "Response (%)", "", "91.4%", "88.1%", "", "89.5%", "90.7%"),
    .Dim = c(8L, 5L)
  )
  expect_equal(result_matrix, expected_matrix)

  # Odds rato table with non-default inputs.
  result <- basic_table() %>%
    tabulate_rsp_subgroups(vars = c("n_tot", "or", "ci", "pval"), method = "chisq", conf_level = 0.95) %>%
    build_table(df_or)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Sex", "F", "M", "Stratification Factor 2",
      "S1", "S2", " ", "Total n", "", "120", "80", "", "105", "95",
      " ", "Odds Ratio", "", "5.83", "1.67", "", "3.5", "3.95", " ",
      "95% CI", "", "(2.03, 16.73)", "(0.48, 5.79)", "", "(1.22, 10)",
      "(1.2, 13.01)", " ", "p-value (Chi-Squared Test)", "", "0.0004",
      "0.4150", "", "0.0154", "0.0178"),
    .Dim = c(8L, 5L)
    )
  expect_equal(result_matrix, expected_matrix)

})

test_that("d_rsp_subgroups_colvars functions as expected with valid input", {

  vars <- c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval")

  result <- d_rsp_subgroups_colvars(
    vars = vars,
    conf_level = 0.9,
    method = "chisq"
  )

  expected <- list(
    vars = c("n", "n_rsp", "prop", "n_tot", "or", "lcl", "pval"),
    labels = c(
      n = "n",
      n_rsp = "Responder n",
      prop = "Response (%)",
      n_tot = "Total n",
      or = "Odds Ratio",
      ci = "90% CI",
      pval = "p-value (Chi-Squared Test)"
    )
  )

  expect_equal(result, expected)

})
