# Local data pre-processing
test_fit <- local({
  dta <- tern_ex_adtte[tern_ex_adtte$PARAMCD == "OS", ]
  survival::survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
})

testthat::test_that("h_xticks works with default settings", {
  result <- h_data_plot(test_fit) %>%
    h_xticks()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks number", {
  result <- h_data_plot(test_fit) %>%
    h_xticks(xticks = 100)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks numeric", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(test_fit) %>%
    h_xticks(xticks = expected)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks returns error when xticks non-numeric", {
  testthat::expect_error(
    h_data_plot(test_fit) %>% h_xticks(xticks = TRUE)
  )
})

testthat::test_that("h_xticks works with max_time only", {
  result <- h_data_plot(test_fit, max_time = 3000) %>%
    h_xticks(max_time = 3000)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks numeric when max_time is not NULL", {
  expected <- c(0, 365, 1000)
  result <- h_data_plot(test_fit, max_time = 1500) %>%
    h_xticks(xticks = expected, max_time = 1500)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks number when max_time is not NULL", {
  result <- h_data_plot(test_fit, max_time = 1500) %>%
    h_xticks(xticks = 500, max_time = 1500)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_tbl_median_surv estimates median survival time with CI", {
  result <- h_tbl_median_surv(fit_km = test_fit)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_tbl_coxph_pairwise estimates HR, CI and pvalue", {
  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)
  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")
  result1 <- h_tbl_coxph_pairwise(
    df = df,
    variables = variables,
    control_coxph_pw = control_coxph()
  )

  res <- testthat::expect_silent(result1)
  testthat::expect_snapshot(res)

  result2 <- h_tbl_coxph_pairwise(
    df = df,
    variables = c(variables, list(strata = "SEX")),
    control_coxph_pw = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99)
  )

  res <- testthat::expect_silent(result2)
  testthat::expect_snapshot(res)
})
