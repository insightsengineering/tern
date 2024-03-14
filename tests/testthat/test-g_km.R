testthat::test_that("g_km works with default settings", {
  df <- tern_ex_adtte %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)
  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  testthat::expect_silent(
    g_km(df = df, variables = variables)
  )
})

testthat::test_that("g_km works with title/footnotes and annotation", {
  df <- tern_ex_adtte %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)
  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  withr::with_options(
    opts_partial_match_old,
    testthat::expect_silent(
      g_km(
        df = df,
        variables = variables,
        title = "KM Plot",
        footnotes = "footnotes",
        annot_coxph = TRUE
      )
    )
  )
})

testthat::test_that("g_km default plot works", {
  skip_on_ci()

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  withr::with_options(
    opts_partial_match_old,
    g_km_default <- g_km(
      df = df,
      variables = variables,
      ci_ribbon = FALSE
    )
  )

  expect_snapshot_ggplot("g_km_default", g_km_default, width = 9, height = 6)
})

testthat::test_that("g_km default plot witch ci_ribbon = TRUE works", {
  skip_on_ci()

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  g_km_ci_ribbon <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      ci_ribbon = TRUE
    )
  )

  expect_snapshot_ggplot("g_km_ci_ribbon", g_km_ci_ribbon, width = 9, height = 9)
})

testthat::test_that("g_km plot with < = > in group labels works", {
  skip_on_ci()

  df <- tern_ex_adtte %>%
    df_explicit_na() %>%
    dplyr::filter(PARAMCD == "OS", ARM == "A: Drug X") %>%
    dplyr::mutate(
      is_event = CNSR == 0,
      group = as.factor(ifelse(AGE > 34, ">Median", "<=Median"))
    )

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "group")

  # snapshot test fails in integration tests
  testthat::expect_silent(
    withr::with_options(
      opts_partial_match_old,
      g_km(
        df = df,
        variables = variables,
        annot_surv_med = FALSE
      )
    )
  )
})

testthat::test_that("g_km ylim parameter works as expected", {
  skip_on_ci()
  set.seed(123)

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

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
  expect_snapshot_ggplot("g_km_crop_ylim", g_km_crop_ylim, width = 9, height = 6)

  g_km_crop_ylim_failure <- withr::with_options(
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
  expect_snapshot_ggplot("g_km_crop_ylim_failure", g_km_crop_ylim_failure, width = 9, height = 6)

  g_km_custom_ylim <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_surv_med = FALSE,
      annot_at_risk = FALSE,
      ylim = c(0.25, 0.75)
    )
  )
  expect_snapshot_ggplot("g_km_custom_ylim", g_km_custom_ylim, width = 9, height = 6)
})

testthat::test_that("annot_at_risk_title parameter works as expected", {
  skip_on_ci()
  set.seed(123)

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  g_km_at_risk_title <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_at_risk_title = TRUE
    )
  )
  expect_snapshot_ggplot("g_km_at_risk_title", g_km_at_risk_title, width = 9, height = 9)
})

testthat::test_that("ref_group_coxph parameter works as expected", {
  skip_on_ci()
  set.seed(123)

  df <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  g_km_ref_group_coxph <- withr::with_options(
    opts_partial_match_old,
    g_km(
      df = df,
      variables = variables,
      annot_coxph = TRUE,
      ref_group_coxph = "ARM B",
      annot_coxph_ref_lbls = TRUE
    )
  )
  expect_snapshot_ggplot("g_km_ref_group_coxph", g_km_ref_group_coxph, width = 9, height = 9)
})
