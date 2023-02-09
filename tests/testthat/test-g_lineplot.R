adsl <- tern_ex_adsl
adlb <- tern_ex_adlb %>% dplyr::filter(ANL01FL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING")
adlb$AVISIT <- droplevels(adlb$AVISIT)
adlb <- dplyr::mutate(adlb, AVISIT = forcats::fct_reorder(AVISIT, AVISITN, min))

testthat::test_that("g_lineplot works with default settings", {
  some_warning <- capture_warnings(g_lineplot(adlb, adsl))
  if (isTRUE(length(some_warning) > 0)){
    stop("THIS IS THE WARNING 1: ", some_warning)
  }
})

testthat::test_that("g_lineplot works with custom settings and statistics table", {
  some_warning <- capture_warnings(g_lineplot(
    adlb,
    adsl,
    strata = control_lineplot_vars(strata = NULL),
    mid = "median",
    table = c("n", "mean", "mean_ci"),
    control = control_summarize_vars(conf_level = 0.80),
    title = "Plot of Mean and 80% Confidence Limits by Visit",
    y_lab = "Lab Test",
    subtitle = "Laboratory Test:",
    caption = "caption"
  ))
  if (isTRUE(length(some_warning) > 0)){
    stop("THIS IS THE WARNING 2: ", some_warning)
  }
})
