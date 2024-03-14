df <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(is_event = CNSR == 0)

variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

testthat::test_that("g_km default plot works", {
  g_km_default <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables
    )
  )
  expect_snapshot_ggplot("g_km_default", g_km_default, width = 8, height = 5)
})

testthat::test_that("g_km works with title/footnotes and annotation", {
  g_km_title_footer <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      title = "KM Plot",
      footnotes = "footnotes"
    )
  )
  expect_snapshot_ggplot("g_km_title_footer", g_km_title_footer, width = 8, height = 5)
})

testthat::test_that("g_km default plot witch ci_ribbon = TRUE works", {
  g_km_ci_ribbon <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      ci_ribbon = TRUE
    )
  )
  expect_snapshot_ggplot("g_km_ci_ribbon", g_km_ci_ribbon, width = 8, height = 5)
})

testthat::test_that("g_km plot with < = > in group labels works", {
  df <- tern_ex_adtte %>%
    df_explicit_na() %>%
    dplyr::filter(PARAMCD == "OS", ARM == "A: Drug X") %>%
    dplyr::mutate(
      is_event = CNSR == 0,
      group = as.factor(ifelse(AGE > 34, ">Median", "<=Median"))
    )

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "group")

  g_km_eq_lbls <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_surv_med = FALSE
    )
  )
  expect_snapshot_ggplot("g_km_eq_lbls", g_km_eq_lbls, width = 8, height = 5)
})

testthat::test_that("g_km ylim parameter works as expected", {
  g_km_crop_ylim <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_surv_med = FALSE,
      annot_at_risk = FALSE,
      max_time = 1000
    )
  )
  expect_snapshot_ggplot("g_km_crop_ylim", g_km_crop_ylim, width = 8, height = 5)

  g_km_crop_ylim_failure <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      yval = "Failure",
      annot_surv_med = FALSE,
      annot_at_risk = FALSE,
      max_time = 1000
    )
  )
  expect_snapshot_ggplot("g_km_crop_ylim_failure", g_km_crop_ylim_failure, width = 8, height = 4)

  g_km_custom_ylim <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_surv_med = FALSE,
      annot_at_risk = FALSE,
      ylim = c(0.25, 0.75)
    )
  )
  expect_snapshot_ggplot("g_km_custom_ylim", g_km_custom_ylim, width = 8, height = 4)
})

testthat::test_that("annot_at_risk_title parameter works as expected", {
  g_km_at_risk_title <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_at_risk_title = TRUE
    )
  )
  expect_snapshot_ggplot("g_km_at_risk_title", g_km_at_risk_title, width = 8, height = 5)
})

testthat::test_that("ref_group_coxph parameter works as expected", {
  g_km_ref_group_coxph <-  withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_coxph = TRUE,
      ref_group_coxph = "ARM B",
      control_annot_coxph = control_coxph_annot(x = 0.25, y = 0.44, w = 0.35, ref_lbls = TRUE)
    )
  )
  expect_snapshot_ggplot("g_km_ref_group_coxph", g_km_ref_group_coxph, width = 10, height = 8)
})

testthat::test_that("g_km works with custom arguments", {
  g_km_custom <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      pch = "?",
      size = 4,
      lty = c("solid", "dotted", "dashed"),
      lwd = 2,
      rel_height_plot = 0.6,
      font_size = 12,
      col = c("red", "green", "purple")
    )
  )
  expect_snapshot_ggplot("g_km_custom", g_km_custom, width = 10, height = 5)
})

testthat::test_that("g_km as_list argument works", {
  g_km_list <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      as_list = TRUE
    )
  )
  g_km_plot_only <- g_km_list$plot
  g_km_table_only <- g_km_list$table

  expect_snapshot_ggplot("g_km_plot_only", g_km_plot_only, width = 10, height = 4)
  expect_snapshot_ggplot("g_km_table_only", g_km_table_only, width = 9, height = 3)
})
