library(scda)
library(dplyr)
library(survival)

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte # nolintr

test_fit <- local({
  dta <- adtte[adtte$PARAMCD == "OS", ]
  survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
})

# h_data_plot ----
testthat::test_that("h_data_plot works as expected", {
  data <- test_fit
  result <- h_data_plot(data)
  testthat::expect_is(result, "tbl_df")
  testthat::expect_identical(
    names(result),
    c(
      "time", "n.risk", "n.event", "n.censor", "estimate", "std.error",
      "conf.high", "conf.low", "strata", "censor"
    )
  )
})

testthat::test_that("h_data_plot respects the ordering of the arm variable factor levels", {
  data <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM C", "ARM A"))) %>%
    survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
  result <- h_data_plot(data)
  testthat::expect_is(result, "tbl_df")
  testthat::expect_is(result$strata, "factor")
  testthat::expect_identical(levels(result$strata), c("ARM B", "ARM C", "ARM A"))
})

testthat::test_that("h_data_plot adds rows that have time 0 and estimate 1", {
  data <- test_fit
  result <- h_data_plot(data)
  result_corner <- result %>%
    dplyr::filter(time == 0, estimate == 1)
  testthat::expect_identical(result_corner$strata, factor(c("ARM A", "ARM B", "ARM C")))
  testthat::expect_true(with(
    result_corner,
    all(conf.high == 1) && all(conf.low == 1) && all(n.event == 0) && all(n.censor == 0)
  ))
})

# h_xticks ----
testthat::test_that("h_xticks works with default settings", {
  result <- h_data_plot(test_fit) %>%
    h_xticks()
  expected <- seq(0, 5000, 1000)
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_xticks works with xticks number", {
  result <- h_data_plot(test_fit) %>%
    h_xticks(xticks = 100)
  expected <- seq(0, 4700, 100)
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_xticks works with xticks numeric", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(test_fit) %>%
    h_xticks(xticks = expected)
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_xticks works with max_time only", {
  expected <- c(0, 1000, 2000, 3000)
  result <- h_data_plot(test_fit, max_time = 3000) %>%
    h_xticks(max_time = 3000)
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_xticks works with xticks numeric when max_time is not NULL", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(test_fit, max_time = 1500) %>%
    h_xticks(xticks = expected, max_time = 1500)
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_xticks works with xticks number when max_time is not NULL", {
  expected <- c(0, 500, 1000, 1500)
  result <- h_data_plot(test_fit, max_time = 1500) %>%
    h_xticks(xticks = 500, max_time = 1500)
  testthat::expect_identical(result, expected)
})


# h_tbl_median_surv ----
testthat::test_that("h_tbl_median_surv estimates median survival time with CI", {
  result <- h_tbl_median_surv(fit_km = test_fit)
  expected <- structure(
    list(
      N = c(134, 134, 132),
      Median = c(1260, 837.4, 337.2),
      `95% CI` = c("(849.3, 1723)", "(527.7, 985.6)", "(293.7, 484.4)")
    ),
    row.names = c("ARM A", "ARM B", "ARM C"),
    class = "data.frame"
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_tbl_coxph_pairwise estimates HR, CI and pvalue", {
  df <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")
  result1 <- h_tbl_coxph_pairwise(
    df = df,
    variables = variables,
    control_coxph_pw = control_coxph()
  )
  expected1 <- structure(
    list(
      HR = c("1.39", "2.75"),
      `95% CI` = c("(1.03, 1.90)", "(2.05, 3.70)"),
      `p-value (log-rank)` = c("0.0334", "<0.0001")
    ),
    row.names = c("ARM B", "ARM C"),
    class = "data.frame"
  )
  testthat::expect_identical(result1, expected1)

  result2 <- h_tbl_coxph_pairwise(
    df = df,
    variables = c(variables, list(strat = "SEX")),
    control_coxph_pw = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99)
  )
  expected2 <- structure(
    list(
      HR = c("1.36", "2.73"),
      `99% CI` = c("(0.91, 2.05)", "(1.83, 4.06)"),
      `p-value (wald)` = c("0.0487", "<0.0001")
    ),
    row.names = c("ARM B", "ARM C"),
    class = "data.frame"
  )
  testthat::expect_identical(result2, expected2)
})
