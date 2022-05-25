library(scda)

preprocess_adrs <- function(adrs, n_records = 20) {
  adrs_labels <- formatters::var_labels(adrs)
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
  formatters::var_labels(adrs) <- c(adrs_labels, "Response")

  adrs
}

adrs <- synthetic_cdisc_data("rcd_2022_02_28")$adrs

adrs_100 <- adrs %>%
  preprocess_adrs(n_records = 100)

adrs_200 <- adrs %>%
  preprocess_adrs(n_records = 200)

testthat::test_that("extract_rsp_subgroups functions as expected with valid input and default arguments", {
  adrs <- adrs_100

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- list(
    prop = data.frame(
      arm = factor(rep(c("B: Placebo", "A: Drug X"), 5), levels = c("B: Placebo", "A: Drug X")),
      n = c(50, 50, 30, 26, 20, 24, 20, 28, 30, 22),
      n_rsp = c(39, 45, 22, 24, 17, 21, 16, 25, 23, 20),
      prop = c(0.78, 0.9, 0.7333333, 0.9230769, 0.85, 0.875, 0.8, 0.8928571, 0.7666667, 0.9090909),
      subgroup = c("All Patients", "All Patients", "F", "F", "M", "M", "S1", "S1", "S2", "S2"),
      var = c(rep("ALL", 2), rep("SEX", 4), rep("STRATA2", 4)),
      var_label = c(rep("All Patients", 2), rep("Sex", 4), rep("Stratification Factor 2", 4)),
      row_type = c(rep("content", 2), rep("analysis", 8)),
      stringsAsFactors = FALSE
    ),
    or = data.frame(
      arm = rep(" ", 5),
      n_tot = c(100L, 56L, 44L, 48L, 52L),
      or = c(2.538461, 4.363636, 1.235294, 2.083333, 3.043478),
      lcl = c(0.8112651, 0.8347243, 0.2204647, 0.4110081, 0.5663254),
      ucl = c(7.942886, 22.811510, 6.921525, 10.560077, 16.355893),
      conf_level = 0.95,
      subgroup = c("All Patients", "F", "M", "S1", "S2"),
      var = c("ALL", "SEX", "SEX", "STRATA2", "STRATA2"),
      var_label = c("All Patients", rep(c("Sex", "Stratification Factor 2"), each = 2)),
      row_type = c("content", rep("analysis", 4)),
      stringsAsFactors = FALSE
    )
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("extract_rsp_subgroups functions as expected with NULL subgroups", {
  adrs <- adrs_100

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs
  )

  expected <- list(
    prop = data.frame(
      arm = factor(c("B: Placebo", "A: Drug X"), levels = c("B: Placebo", "A: Drug X")),
      n = c(50, 50),
      n_rsp = c(39, 45),
      prop = c(0.78, 0.9),
      subgroup = c("All Patients", "All Patients"),
      var = rep("ALL", 2),
      var_label = rep("All Patients", 2),
      row_type = rep("content", 2),
      stringsAsFactors = FALSE
    ),
    or = data.frame(
      arm = " ",
      n_tot = 100L,
      or = 2.538461,
      lcl = 0.8112651,
      ucl = 7.942886,
      conf_level = 0.95,
      subgroup = "All Patients",
      var = "ALL",
      var_label = "All Patients",
      row_type = "content",
      stringsAsFactors = FALSE
    )
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("extract_rsp_subgroups works as expected with groups_lists", {
  adrs <- adrs_100

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
    data = adrs,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )

  prop <- result$prop
  testthat::expect_setequal(
    prop[prop$var == "BMRKR2", "subgroup"],
    c("low", "low/medium", "low/medium/high")
  )

  or <- result$or
  testthat::expect_setequal(
    or[or$var == "BMRKR2", "subgroup"],
    c("low", "low/medium", "low/medium/high")
  )
})

testthat::test_that("extract_rsp_subgroups functions as expected with strata", {
  adrs <- adrs_100

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2"), strat = c("STRATA1")),
    data = adrs,
    conf_level = 0.9,
    method = "cmh",
    label_all = "ALL"
  )

  expected <- list(
    prop = data.frame(
      arm = factor(rep(c("B: Placebo", "A: Drug X"), 5), levels = c("B: Placebo", "A: Drug X")),
      n = c(50, 50, 30, 26, 20, 24, 20, 28, 30, 22),
      n_rsp = c(39, 45, 22, 24, 17, 21, 16, 25, 23, 20),
      prop = c(0.78, 0.9, 0.7333333, 0.9230769, 0.85, 0.875, 0.8, 0.8928571, 0.7666667, 0.9090909),
      subgroup = c("ALL", "ALL", "F", "F", "M", "M", "S1", "S1", "S2", "S2"),
      var = c(rep("ALL", 2), rep("SEX", 4), rep("STRATA2", 4)),
      var_label = c(rep("ALL", 2), rep("Sex", 4), rep("Stratification Factor 2", 4)),
      row_type = c(rep("content", 2), rep("analysis", 8)),
      stringsAsFactors = FALSE
    ),
    or = data.frame(
      arm = rep(" ", 5),
      n_tot = c(100L, 56L, 44L, 48L, 52L),
      or = c(2.44435836096141, 3.93615491524354, 1.33764497396648, 1.76534154255098, 2.96199676942766),
      lcl = c(0.943961742331993, 1.00223952484456, 0.314998956584877, 0.432590127386342, 0.729767321547954),
      ucl = c(6.32958681359417, 15.458695384418, 5.68031747081565, 7.20411901373099, 12.022222156358),
      conf_level = c(0.9, 0.9, 0.9, 0.9, 0.9),
      pval = c(0.11439763791237, 0.0817702804665442, 0.740127116433065, 0.503305076219993, 0.187331184135787),
      pval_label = "p-value (Cochran-Mantel-Haenszel Test)",
      subgroup = c("ALL", "F", "M", "S1", "S2"),
      var = c("ALL", "SEX", "SEX", "STRATA2", "STRATA2"),
      var_label = c("ALL", rep(c("Sex", "Stratification Factor 2"), each = 2)),
      row_type = c("content", rep("analysis", 4)),
      stringsAsFactors = FALSE
    )
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("a_response_subgroups functions as expected with valid input", {
  df <- data.frame(
    prop = c(0.1234, 0.5678),
    pval = c(0.00001, 0.983758),
    subgroup = c("M", "F"),
    stringsAsFactors = FALSE
  )

  afun <- a_response_subgroups(.formats = list(prop = "xx.xx", pval = "x.xxxx | (<0.0001)"))

  result <- basic_table() %>%
    split_cols_by_multivar(c("prop", "pval")) %>%
    analyze_colvars(afun) %>%
    build_table(df)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "M", "F", "prop", "0.12", "0.57", "pval", "<0.0001",
      "0.9838"
    ),
    .Dim = c(3L, 3L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_rsp_subgroups functions as expected with valid input", {
  adrs <- adrs_200

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs,
    conf_level = 0.95,
    method = "chisq"
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("n", "prop", "n_tot", "or", "ci", "pval")
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Sex", "F", "M", "Stratification Factor 2",
      "S1", "S2", " ", "Total n", "200", "", "120", "80", "", "105",
      "95", "B: Placebo", "n", "100", "", "62", "38", "", "48", "52",
      "B: Placebo", "Response (%)", "71.0%", "", "64.5%", "81.6%", "",
      "70.8%", "71.2%", "A: Drug X", "n", "100", "", "58", "42", "",
      "57", "43", "A: Drug X", "Response (%)", "90.0%", "", "91.4%",
      "88.1%", "", "89.5%", "90.7%", " ", "Odds Ratio", "3.68", "",
      "5.83", "1.67", "", "3.50", "3.95", " ", "95% CI", "(1.68, 8.04)",
      "", "(2.03, 16.73)", "(0.48, 5.79)", "", "(1.22, 10.00)", "(1.20, 13.01)",
      " ", "p-value (Chi-Squared Test)", "0.0007", "", "0.0004", "0.4150",
      "", "0.0154", "0.0178"
    ),
    .Dim = c(9L, 9L)
  )

  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_rsp_subgroups correctly calculates column indices", {
  adrs <- adrs_200

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs,
    conf_level = 0.95,
    method = "chisq"
  )

  # Case with both OR and response table parts.
  result_both <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("n", "prop", "or", "ci", "pval", "n_tot")
    )
  result_both_cols <- attributes(result_both)[c("col_x", "col_ci", "col_symbol_size")]
  expected_both_cols <- list(col_x = 6L, col_ci = 7L, col_symbol_size = 1L)
  testthat::expect_identical(result_both_cols, expected_both_cols)

  # Case with just OR results.
  result_or <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("or", "n_tot", "ci")
    )
  result_or_cols <- attributes(result_or)[c("col_x", "col_ci", "col_symbol_size")]
  expected_or_cols <- list(col_x = 1L, col_ci = 3L, col_symbol_size = 2L)
  testthat::expect_identical(result_or_cols, expected_or_cols)
})

testthat::test_that("tabulate_rsp_subgroups functions as expected with valid input extreme values in OR table", {
  var1 <- data.frame(
    rsp = c(rep(TRUE, 30), rep(FALSE, 20)),
    arm = factor(c(rep("REF", 30), rep("COMP", 20)), levels = c("REF", "COMP")),
    var1 = factor("subgroup1", levels = c("subgroup1", "subgroup2")),
    stringsAsFactors = FALSE
  )

  var2 <- data.frame(
    rsp = c(rep(TRUE, 3), rep(FALSE, 7), rep(TRUE, 2), rep(FALSE, 0)),
    arm = factor(c(rep("REF", 10), rep("COMP", 2)), levels = c("REF", "COMP")),
    var1 = factor("subgroup2", levels = c("subgroup1", "subgroup2")),
    stringsAsFactors = FALSE
  )

  adrs <- rbind(var1, var2)

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "arm", subgroups = "var1"),
    data = adrs,
    conf_level = 0.95
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(df)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "var1", "subgroup1", "subgroup2",
      " ", "Total n", "62", "", "50", "12", "REF", "n", "40", "", "30",
      "10", "REF", "Response (%)", "82.5%", "", "100.0%", "30.0%", "COMP",
      "n", "22", "", "20", "2", "COMP", "Response (%)", "9.1%", "",
      "0.0%", "100.0%", " ", "Odds Ratio", "0.02", "", "<0.01", ">999.99",
      " ", "95% CI", "(<0.01, 0.11)", "", "(0.00, >999.99)", "(0.00, >999.99)"
    ),
    .Dim = c(6L, 8L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_rsp_subgroups functions as expected with NULL subgroups", {
  adrs <- adrs_200

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs,
    method = "chisq",
    conf_level = 0.95
  )

  result <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("n_tot", "n", "prop", "or", "ci", "pval")
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", " ", "Total n", "200", "B: Placebo",
      "n", "100", "B: Placebo", "Response (%)", "71.0%", "A: Drug X",
      "n", "100", "A: Drug X", "Response (%)", "90.0%", " ", "Odds Ratio",
      "3.68", " ", "95% CI", "(1.68, 8.04)", " ", "p-value (Chi-Squared Test)",
      "0.0007"
    ),
    .Dim = c(3L, 9L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("tabulate_rsp_subgroups functions as expected when 0 obs in one arm", {
  adrs <- adrs_200

  df <- testthat::expect_warning(extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = "RACE"),
    data = adrs,
    method = "chisq",
    conf_level = 0.95
  ))

  result <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("n_tot", "n", "prop", "or", "ci", "pval")
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "Baseline Risk Factors", "", "All Patients", "Race", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      " ", "Total n", "200", "", "106", "49", "34", "10", "1", "B: Placebo",
      "n", "100", "", "50", "23", "19", "7", "1", "B: Placebo", "Response (%)",
      "71.0%", "", "66.0%", "65.2%", "78.9%", "100.0%", "100.0%", "A: Drug X",
      "n", "100", "", "56", "26", "15", "3", "0", "A: Drug X", "Response (%)",
      "90.0%", "", "87.5%", "92.3%", "100.0%", "66.7%", "NA%", " ", "Odds Ratio",
      "3.68", "", "3.61", "6.40", ">999.99", "<0.01", "NA", " ", "95% CI",
      "(1.68, 8.04)", "", "(1.35, 9.65)", "(1.19, 34.29)", "(0.00, >999.99)",
      "(0.00, >999.99)", "(NA, NA)", " ", "p-value (Chi-Squared Test)",
      "0.0007", "", "0.0083", "0.0189", "0.0585", "0.1074", "NA"
    ),
    .Dim = c(9L, 9L)
  )
  testthat::expect_equal(result_matrix, expected_matrix)
})

testthat::test_that("d_rsp_subgroups_colvars functions as expected with valid input", {
  vars <- c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval")

  result <- d_rsp_subgroups_colvars(
    vars = vars,
    conf_level = 0.9,
    method = "p-value (Chi-Squared Test)"
  )

  expected <- list(
    vars = c("n", "n_rsp", "prop", "n_tot", "or", "lcl", "pval"),
    labels = c(
      n = "n",
      n_rsp = "Responders",
      prop = "Response (%)",
      n_tot = "Total n",
      or = "Odds Ratio",
      ci = "90% CI",
      pval = "p-value (Chi-Squared Test)"
    )
  )

  testthat::expect_equal(result, expected)
})
