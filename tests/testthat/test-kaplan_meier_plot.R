library(random.cdisc.data)
library(dplyr)
library(survival)

get_test_data <- function() {
  dta <- radtte(cached = TRUE) # nolintr
  dta <- dta[dta$PARAMCD == "OS", ]
  survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
}

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
      HR = c("1.3940", "2.7532"),
      `95% CI` = c("(1.0251, 1.8957)", "(2.0478, 3.7017)"),
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
      HR = c("1.3644", "2.7277"),
      `99% CI` = c("(0.9091, 2.0476)", "(1.8346, 4.0555)"),
      `p-value (wald)` = c("0.0487", "<0.0001")
    ),
    row.names = c("ARM B", "ARM C"),
    class = "data.frame"
  )
  expect_identical(result2, expected2)
})
