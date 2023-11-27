# Local data pre-processing
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

adrs_100 <- tern_ex_adrs %>% preprocess_adrs(n_records = 100)
adrs_200 <- tern_ex_adrs %>% preprocess_adrs(n_records = 200)

testthat::test_that("extract_rsp_subgroups functions as expected with valid input and default arguments", {
  adrs <- adrs_100

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("extract_rsp_subgroups functions as expected with NULL subgroups", {
  adrs <- adrs_100

  result <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
  res <- testthat::expect_silent(prop[prop$var == "BMRKR2", "subgroup"])
  testthat::expect_snapshot(res)

  or <- result$or
  res <- testthat::expect_silent(or[or$var == "BMRKR2", "subgroup"])
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result_both_cols)
  testthat::expect_snapshot(res)

  # Case with just OR results.
  result_or <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("or", "n_tot", "ci")
    )
  result_or_cols <- attributes(result_or)[c("col_x", "col_ci", "col_symbol_size")]

  res <- testthat::expect_silent(result_or_cols)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_rsp_subgroups functions as expected when 0 obs in one arm", {
  adrs <- adrs_200

  suppressWarnings(testthat::expect_warning(df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = "RACE"),
    data = adrs,
    method = "chisq",
    conf_level = 0.95
  )))

  result <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("n_tot", "n", "prop", "or", "ci", "pval")
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("d_rsp_subgroups_colvars functions as expected with valid input", {
  vars <- c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval")

  result <- d_rsp_subgroups_colvars(
    vars = vars,
    conf_level = 0.9,
    method = "p-value (Chi-Squared Test)"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_rsp_subgroups na_str argument works as expected", {
  adrs <- adrs_200

  df <- extract_rsp_subgroups(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs,
    conf_level = 0.95,
    method = "chisq"
  )
  df$or$or[2:5] <- NA

  result <- basic_table() %>%
    tabulate_rsp_subgroups(
      df = df,
      vars = c("n", "prop", "n_tot", "or", "ci", "pval"),
      na_str = "<No data>"
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
