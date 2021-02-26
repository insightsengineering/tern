library(random.cdisc.data)
library(dplyr)
library(survival)

get_test_data <- function() {
  dta <- radtte(cached = TRUE) # nolintr
  dta <- dta[dta$PARAMCD == "OS", ]
  survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
}

# h_xticks ----
test_that("h_xticks works with xticks NULL", {
  result <- h_data_plot(get_test_data()) %>%
    h_xticks()
  expected <- seq(0, 5000, 1000)
  expect_identical(result, expected)
})

test_that("h_xticks works with xticks number", {
  result <- h_data_plot(get_test_data()) %>%
    h_xticks(100)
  expected <- seq(0, 4700, 100)
  expect_identical(result, expected)
})

test_that("h_xticks works with xticks numeric", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(get_test_data()) %>%
    h_xticks(expected)
  expect_identical(result, expected)
})

# h_tbl_median_surv ----
test_that("h_tbl_median_surv estimates median survival time with CI", {
  result <- h_tbl_median_surv(fit_km = get_test_data())
  expected <- structure(
    list(
      N = c(134, 134, 132),
      Median = c(1010, 813.6, 451),
      `95% CI` = c("(951.2, 1401)", "(638.3, 1136)", "(339.2, 701.1)")
    ),
    row.names = c("ARMCD=ARM A", "ARMCD=ARM B", "ARMCD=ARM C"),
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
      HR = c("1.1887", "2.1107"),
      `95% CI` = c("(0.8805, 1.6048)", "(1.5689, 2.8397)"),
      `p-value (log-rank)` = c("0.2584", "<0.0001")
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
      HR = c("1.1825", "2.0803"),
      `99% CI` = c("(0.7933, 1.7626)", "(1.4075, 3.0745)"),
      `p-value (wald)` = c("0.2795", "<0.0001")
    ),
    row.names = c("ARM B", "ARM C"),
    class = "data.frame"
  )
  expect_identical(result2, expected2)
})
