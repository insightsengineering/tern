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

adrs_20 <- preprocess_adrs(adrs, 20)
adrs_100 <- preprocess_adrs(adrs, 100)

testthat::test_that("h_proportion_df functions as expected with valid input and default arguments", {
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

  testthat::expect_equal(result, expected)
})

testthat::test_that("h_proportion_df functions as expected when 0 responses in one group", {
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

  testthat::expect_equal(result, expected)
})

testthat::test_that("h_proportion_df fails with wrong input", {
  testthat::expect_error(h_proportion_df(
    rsp = c(TRUE, FALSE, Inf),
    arm = factor(c("A", "B", "A"), levels = c("B", "A"))
  ))
})

testthat::test_that("h_proportion_df functions when 0 obs in one arm", {
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(rep("A", 6), levels = c("B", "A"))

  result <- h_proportion_df(rsp = rsp, arm = arm)

  expected <- data.frame(
    arm = factor(c("B", "A"), levels = c("B", "A")),
    n = c(0, 6),
    n_rsp = c(NA, 2),
    prop = c(NA, 0.333333333333333),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("h_proportion_subgroups_df functions as expected with valid input and default arguments", {
  adrs <- adrs_20

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- data.frame(
    arm = factor(rep(c("B: Placebo", "A: Drug X"), 5), levels = c("B: Placebo", "A: Drug X")),
    n = c(7, 13, 3, 5, 4, 8, 2, 9, 5, 4),
    n_rsp = c(4, 11, 1, 5, 3, 6, 2, 8, 2, 3),
    prop = c(0.5714286, 0.8461538, 0.3333333, 1, 0.75, 0.75, 1, 0.8888889, 0.4, 0.75),
    subgroup = c("All Patients", "All Patients", "F", "F", "M", "M", "S1", "S1", "S2", "S2"),
    var = c(rep("ALL", 2), rep("SEX", 4), rep("STRATA2", 4)),
    var_label = c(rep("All Patients", 2), rep("Sex", 4), rep("Stratification Factor 2", 4)),
    row_type = c(rep("content", 2), rep("analysis", 8)),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_proportion_subgroups_df functions as expected when subgroups is NULL.", {
  adrs <- adrs_20

  result <- h_proportion_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs
  )

  expected <- data.frame(
    arm = factor(c("B: Placebo", "A: Drug X"), levels = c("B: Placebo", "A: Drug X")),
    n = c(7, 13),
    n_rsp = c(4, 11),
    prop = c(0.5714286, 0.8461538),
    subgroup = c("All Patients", "All Patients"),
    var = rep("ALL", 2),
    var_label = rep("All Patients", 2),
    row_type = rep("content", 2),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_proportion_subgroups_df works as expected with groups_lists", {
  adrs <- adrs_20

  result <- h_proportion_subgroups_df(
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

  testthat::expect_setequal(
    result[result$var == "BMRKR2", "subgroup"],
    c("low", "low/medium", "low/medium/high")
  )
})

testthat::test_that("h_odds_ratio_df functions as expected with valid input and default arguments", {
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

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_df functions as expected with valid input and non-default arguments", {
  adrs <- adrs_100

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

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_df functions as expected with strata", {
  adrs <- adrs_100

  result <- h_odds_ratio_df(
    rsp = adrs$rsp,
    arm = adrs$ARM,
    strata_data = adrs[, c("STRATA1", "STRATA2")],
    method = "cmh",
    conf_level = 0.9
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 100L,
    or = 2.30758672923386,
    lcl = 0.871548407676952,
    ucl = 6.10976563783706,
    conf_level = 0.9,
    pval = 0.150916197874731,
    pval_label = "p-value (Cochran-Mantel-Haenszel Test)",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_df functions when 0 obs in one arm", {
  rsp <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  arm <- factor(rep("A", 6), levels = c("B", "A"))

  result <- h_odds_ratio_df(
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    arm = factor(rep("A", 6), levels = c("B", "A")),
    method = "chisq"
  )

  expected <- data.frame(
    arm = " ",
    n_tot = 6,
    or = NA,
    lcl = NA,
    ucl = NA,
    conf_level = 0.95,
    pval = NA,
    pval_label = NA,
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_subgroups_df functions as expected with valid input and default arguments", {
  adrs <- adrs_100

  result <- h_odds_ratio_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
    data = adrs
  )

  expected <- data.frame(
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

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_subgroups_df functions as expected when subgroups is NULL.", {
  adrs <- adrs_100

  result <- h_odds_ratio_subgroups_df(
    variables = list(rsp = "rsp", arm = "ARM"),
    data = adrs
  )

  expected <- data.frame(
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

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_subgroups_df functions as expected with strata", {
  adrs <- adrs_100

  result <- h_odds_ratio_subgroups_df(
    variables = list(
      rsp = "rsp",
      arm = "ARM",
      subgroups = c("SEX", "STRATA2"),
      strat = "STRATA1"
    ),
    data = adrs,
    method = "cmh"
  )

  expected <- data.frame(
    arm = c(" ", " ", " ", " ", " "),
    n_tot = c(100L, 56L, 44L, 48L, 52L),
    or = c(2.44435836096141, 3.93615491524354, 1.33764497396648, 1.76534154255098, 2.96199676942766),
    lcl = c(0.786672949464047, 0.771183247516691, 0.238778312539021, 0.330424835389017, 0.557995974203059),
    ucl = c(7.59513569250423, 20.0903164931116, 7.49353681811187, 9.43158754452345, 15.7230970611038),
    conf_level = c(0.95, 0.95, 0.95, 0.95, 0.95),
    pval = c(0.11439763791237, 0.0817702804665442, 0.740127116433065, 0.503305076219993, 0.187331184135787),
    pval_label = "p-value (Cochran-Mantel-Haenszel Test)",
    subgroup = c("All Patients", "F", "M", "S1", "S2"),
    var = c("ALL", "SEX", "SEX", "STRATA2", "STRATA2"),
    var_label = c("All Patients", "Sex", "Sex", "Stratification Factor 2", "Stratification Factor 2"),
    row_type = c("content", "analysis", "analysis", "analysis", "analysis"),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(result, expected, tol = 0.000001)
})

testthat::test_that("h_odds_ratio_subgroups_df works as expected with groups_lists", {
  adrs <- adrs_20

  result <- h_odds_ratio_subgroups_df(
    variables = list(
      rsp = "rsp",
      arm = "ARM",
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )

  testthat::expect_setequal(
    result[result$var == "BMRKR2", "subgroup"],
    c("low", "low/medium", "low/medium/high")
  )
})
