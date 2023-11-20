adsl <- tern_ex_adsl
adlb <- tern_ex_adlb %>% dplyr::filter(ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
adlb$AVISIT <- droplevels(adlb$AVISIT)
adlb <- dplyr::mutate(adlb, AVISIT = forcats::fct_reorder(AVISIT, AVISITN, min))

testthat::test_that("g_lineplot works with default settings", {
  g_lineplot <- g_lineplot(adlb, adsl)

  vdiffr::expect_doppelganger(title = "g_lineplot", fig = g_lineplot)
})

testthat::test_that("g_lineplot works with custom settings and statistics table", {
  g_lineplot_w_stats <- g_lineplot(
    adlb,
    adsl,
    group_var = control_lineplot_vars(group_var = NULL),
    mid = "median",
    table = c("n", "mean", "mean_ci"),
    control = control_analyze_vars(conf_level = 0.80),
    title = "Plot of Mean and 80% Confidence Limits by Visit",
    x_lab = "Time",
    y_lab = "Lab Test",
    subtitle = "Laboratory Test:",
    caption = "caption"
  )

  vdiffr::expect_doppelganger(title = "g_lineplot_w_stats", fig = g_lineplot_w_stats)
})

testthat::test_that("g_lineplot works with cohort_id specified", {
  g_lineplot_cohorts <- g_lineplot(
    adlb,
    adsl,
    group_var = control_lineplot_vars(group_var = "ARM", subject_var = "USUBJID"),
    mid = "median",
    table = c("n", "mean", "mean_ci"),
    control = control_analyze_vars(conf_level = 0.80),
    title = "Plot of Mean and 80% Confidence Limits by Visit",
    y_lab = "Lab Test",
    subtitle = "Laboratory Test:",
    caption = "caption"
  )
  vdiffr::expect_doppelganger(title = "g_lineplot_cohorts", fig = g_lineplot_cohorts)
})
