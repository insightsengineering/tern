adlb <- tern_ex_adlb %>%
  filter(PARAMCD == "ALT", !(AVISIT %in% c("SCREENING", "BASELINE"))) %>%
  slice(1:36)

testthat::test_that("h_g_ipp works correctly", {
  testthat::expect_silent(h_g_ipp(
    df = adlb,
    xvar = "AVISIT",
    yvar = "AVAL",
    xlab = "Visit",
    id_var = "USUBJID",
    ylab = "SGOT/ALT (U/L)",
    add_baseline_hline = TRUE,
    col = nestcolor::color_palette(8)
  ))
})

testthat::test_that("h_g_ipp works with default color palette", {
  testthat::expect_silent(h_g_ipp(
    df = adlb,
    xvar = "AVISIT",
    yvar = "AVAL",
    xlab = "Visit",
    id_var = "USUBJID",
    ylab = "SGOT/ALT (U/L)",
    add_baseline_hline = TRUE
  ))
})

testthat::test_that("g_ipp default plot works with plotting_choices = all_in_one", {
  result <- testthat::expect_silent(g_ipp(
    df = adlb,
    xvar = "AVISIT",
    yvar = "AVAL",
    xlab = "Visit",
    ylab = "SGOT/ALT (U/L)",
    title = "Individual Patient Plots",
    plotting_choices = "all_in_one"
  ))
})

testthat::test_that("g_ipp default plot works with plotting_choices = split_by_max_obs", {
  result <- testthat::expect_silent(g_ipp(
    df = adlb,
    xvar = "AVISIT",
    yvar = "AVAL",
    xlab = "Visit",
    ylab = "SGOT/ALT (U/L)",
    title = "Individual Patient Plots",
    add_baseline_hline = TRUE,
    plotting_choices = "split_by_max_obs",
    max_obs_per_plot = 5
  ))
  testthat::expect_equal(length(result), 2)
})

testthat::test_that("g_ipp default plot works with plotting_choices = separate_by_obs", {
  result <- testthat::expect_silent(g_ipp(
    df = adlb,
    xvar = "AVISIT",
    yvar = "AVAL",
    xlab = "Visit",
    ylab = "SGOT/ALT (U/L)",
    title = "Individual Patient Plots",
    add_baseline_hline = TRUE,
    plotting_choices = "separate_by_obs",
    max_obs_per_plot = 5
  ))
  testthat::expect_equal(length(result), 8)
})
