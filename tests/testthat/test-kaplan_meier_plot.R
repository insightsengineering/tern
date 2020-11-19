library(random.cdisc.data)
library(dplyr)
library(survival)

get_test_data <- function() {
  dta <- radtte(cached = TRUE)
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
