testthat::test_that("g_km default plot works", {
  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  grob_tmp <- g_km(
    df = df,
    variables = variables,
    ci_ribbon = FALSE
  )

  vdiffr::expect_doppelganger(title = "grob_tmp", fig = grob_tmp)
})

testthat::test_that("g_km default plot witch ci_ribbon = TRUE works", {
  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  grob_tmp_ci <- g_km(
    df = df,
    variables = variables,
    ci_ribbon = TRUE
  )

  vdiffr::expect_doppelganger(title = "grob_tmp_ci", fig = grob_tmp_ci)
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

  # snapshot test fails in integration tests
  testthat::expect_silent(g_km(
    df = df,
    variables = variables,
    annot_surv_med = FALSE
  ))
})

testthat::test_that("g_km ylim parameter works as expected", {
  set.seed(123)

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  g_km_crop_ylim <- g_km(
    df = df,
    variables = variables,
    annot_surv_med = FALSE,
    annot_at_risk = FALSE,
    max_time = 1000
  )
  vdiffr::expect_doppelganger(title = "g_km_crop_ylim", fig = g_km_crop_ylim)

  g_km_crop_ylim_failure <- g_km(
    df = df,
    variables = variables,
    yval = "Failure",
    annot_surv_med = FALSE,
    annot_at_risk = FALSE,
    max_time = 1000
  )
  vdiffr::expect_doppelganger(title = "g_km_crop_ylim_failure", fig = g_km_crop_ylim_failure)

  g_km_custom_ylim <- g_km(
    df = df,
    variables = variables,
    annot_surv_med = FALSE,
    annot_at_risk = FALSE,
    ylim = c(0.25, 0.75)
  )
  vdiffr::expect_doppelganger(title = "g_km_custom_ylim", fig = g_km_custom_ylim)
})

testthat::test_that("annot_at_risk_title parameter works as expected", {
  set.seed(123)

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  g_km_at_risk_title <- g_km(
    df = df,
    variables = variables,
    annot_at_risk_title = TRUE
  )
  vdiffr::expect_doppelganger(title = "g_km_at_risk_title", fig = g_km_at_risk_title)
})

testthat::test_that("ref_group_coxph parameter works as expected", {
  set.seed(123)

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  g_km_ref_group_coxph <- g_km(
    df = df,
    variables = variables,
    annot_coxph = TRUE,
    ref_group_coxph = "ARM B",
    annot_coxph_ref_lbls = TRUE
  )
  vdiffr::expect_doppelganger(title = "g_km_ref_group_coxph", fig = g_km_ref_group_coxph)
})
