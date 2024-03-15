# Local data pre-processing
test_fit <- local({
  dta <- tern_ex_adtte[tern_ex_adtte$PARAMCD == "OS", ]
  survival::survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = dta)
})

testthat::test_that("control_surv_med_annot works with default settings", {
  result <- control_surv_med_annot()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("control_coxph_annot works with default settings", {
  result <- control_coxph_annot()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with default settings", {
  result <- ggsurvfit::tidy_survfit(test_fit) %>%
    h_xticks()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks number", {
  result <- ggsurvfit::tidy_survfit(test_fit) %>%
    h_xticks(xticks = 100)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks numeric", {
  expected <- c(0, 365, 1000)
  result <- ggsurvfit::tidy_survfit(test_fit) %>%
    h_xticks(xticks = expected)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks returns error when xticks non-numeric", {
  testthat::expect_error(
    ggsurvfit::tidy_survfit(test_fit) %>% h_xticks(xticks = TRUE)
  )
})

testthat::test_that("h_xticks works with max_time only", {
  result <- ggsurvfit::tidy_survfit(test_fit) %>%
    filter(time <= 3000) %>%
    h_xticks(max_time = 3000)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks numeric when max_time is not NULL", {
  expected <- c(0, 365, 1000)
  result <- ggsurvfit::tidy_survfit(test_fit) %>%
    filter(time <= 1500) %>%
    h_xticks(xticks = expected, max_time = 1500)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_xticks works with xticks number when max_time is not NULL", {
  result <- ggsurvfit::tidy_survfit(test_fit) %>%
    filter(time <= 1500) %>%
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

# Deprecated Functions ----

# h_data_plot ----
testthat::test_that("h_data_plot works as expected", {
  data <- test_fit
  testthat::expect_warning(result <- h_data_plot(data))
  res <- names(result)

  testthat::expect_snapshot(res)
  testthat::expect_s3_class(result, "tbl_df")
})

testthat::test_that("h_data_plot respects the ordering of the arm variable factor levels", {
  data <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM C", "ARM A"))) %>%
    survival::survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
  testthat::expect_warning(result <- h_data_plot(data))
  res <- levels(result$strata)

  testthat::expect_snapshot(res)
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_s3_class(result$strata, "factor")
})

testthat::test_that("h_data_plot adds rows that have time 0 and estimate 1", {
  data <- test_fit
  testthat::expect_warning(result <- h_data_plot(data))
  result_corner <- result %>%
    dplyr::filter(time == 0, estimate == 1)
  res <- result_corner$strata

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
