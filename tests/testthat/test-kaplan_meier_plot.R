library(random.cdisc.data)
library(dplyr)
library(survival)

get_test_data <- function() {
  dta <- radtte(cached = TRUE) # nolintr
  dta <- dta[dta$PARAMCD == "OS", ]
  survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
}

# h_data_plot ----
test_that("h_data_plot works as expected", {
  data <- get_test_data()
  result <- h_data_plot(data)
  expect_is(result, "tbl_df")
  expect_identical(
    names(result),
    c("time", "n.risk", "n.event", "n.censor", "estimate", "std.error",
      "conf.high", "conf.low", "strata", "censor")
  )
})

test_that("h_data_plot respects the ordering of the arm variable factor levels", {
  data <- radtte(cached = TRUE) %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM C", "ARM A"))) %>%
    survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
  result <- h_data_plot(data)
  expect_is(result, "tbl_df")
  expect_is(result$strata, "factor")
  expect_identical(levels(result$strata), c("ARM B", "ARM C", "ARM A"))
})

# h_xticks ----
test_that("h_xticks works with default settings", {
  result <- h_data_plot(get_test_data()) %>%
    h_xticks()
  expected <- seq(0, 5000, 1000)
  expect_identical(result, expected)
})

test_that("h_xticks works with xticks number", {
  result <- h_data_plot(get_test_data()) %>%
    h_xticks(xticks = 100)
  expected <- seq(0, 4700, 100)
  expect_identical(result, expected)
})

test_that("h_xticks works with xticks numeric", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(get_test_data()) %>%
    h_xticks(xticks = expected)
  expect_identical(result, expected)
})

test_that("h_xticks works with max_time only", {
  expected <- c(0, 1000, 2000, 3000)
  result <- h_data_plot(get_test_data(), max_time = 3000) %>%
    h_xticks(max_time = 3000)
  expect_identical(result, expected)
})

test_that("h_xticks works with xticks numeric when max_time is not NULL", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(get_test_data(), max_time = 1500) %>%
    h_xticks(xticks = expected, max_time = 1500)
  expect_identical(result, expected)
})

test_that("h_xticks works with xticks number when max_time is not NULL", {
  expected <- c(0, 500, 1000, 1500)
  result <- h_data_plot(get_test_data(), max_time = 1500) %>%
    h_xticks(xticks = 500, max_time = 1500)
  expect_identical(result, expected)
})


# h_tbl_median_surv ----
test_that("h_tbl_median_surv estimates median survival time with CI", {
  result <- h_tbl_median_surv(fit_km = get_test_data())
  expected <- structure(
    list(
      N = c(134, 134, 132),
      Median = c(1260, 837.4, 337.2),
      `95% CI` = c("(849.3, 1723)", "(527.7, 985.6)", "(293.7, 484.4)")
    ),
    row.names = c("ARM A", "ARM B", "ARM C"),
    class = "data.frame"
  )
  expect_identical(result, expected)
})

test_that("h_tbl_coxph_pairwise estimates HR, CI and pvalue", {
  df <- radtte(cached = TRUE) %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)
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
  expect_identical(result1, expected1)

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
  expect_identical(result2, expected2)
})
