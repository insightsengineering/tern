# Local data pre-processing
test_fit <- local({
  dta <- tern_ex_adtte[tern_ex_adtte$PARAMCD == "OS", ]
  survival::survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
})

# h_data_plot ----
testthat::test_that("h_data_plot works as expected", {
  data <- test_fit
  result <- h_data_plot(data)

  res <- testthat::expect_silent("`g_km` now generates `ggplot` objects")
  testthat::expect_snapshot(res)
  testthat::expect_s3_class(result, "tbl_df")
})

testthat::test_that("h_data_plot respects the ordering of the arm variable factor levels", {
  data <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM C", "ARM A"))) %>%
    survival::survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
  result <- h_data_plot(data)

  res <- testthat::expect_silent(levels(result$strata))
  testthat::expect_snapshot(res)
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_s3_class(result$strata, "factor")
})

testthat::test_that("h_data_plot adds rows that have time 0 and estimate 1", {
  data <- test_fit
  result <- h_data_plot(data)
  result_corner <- result %>%
    dplyr::filter(time == 0, estimate == 1)

  res <- testthat::expect_silent(result_corner$strata)
  testthat::expect_snapshot(res)
  testthat::expect_true(with(
    result_corner,
    all(conf.high == 1) && all(conf.low == 1) && all(n.event == 0) && all(n.censor == 0)
  ))
})

testthat::test_that("h_grob_coxph returns error when only one arm", {
  df <- tern_ex_adtte %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)
  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  testthat::expect_warning(
    h_grob_coxph(df = df, variables = variables),
    "`g_km` now generates `ggplot` objects"
  )
})
