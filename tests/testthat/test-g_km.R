library(scda)
library(dplyr)

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

testthat::test_that("g_km default plot works", {
  df <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  result <- testthat::expect_silent(
    g_km(
      df = df,
      variables = variables,
      ci_ribbon = FALSE,
      draw = FALSE
    )
  )
})

testthat::test_that("g_km default plot witch ci_ribbon = TRUE works", {
  df <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  result <- testthat::expect_silent(
    g_km(
      df = df,
      variables = variables,
      ci_ribbon = TRUE,
      draw = FALSE
    )
  )
})

testthat::test_that("g_km plot with < = > in group labels works", {
  df <- ex_adtte %>%
    df_explicit_na() %>%
    dplyr::filter(PARAMCD == "OS", ARM == "A: Drug X", BEP01FL == "Y") %>%
    dplyr::mutate(
      is_event = CNSR == 0,
      group = as.factor(ifelse(AGE > 34, ">Median", "<=Median"))
    )

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "group")

  result <- testthat::expect_silent(
    g_km(
      df = df,
      variables = variables,
      annot_surv_med = FALSE,
      draw = FALSE
    )
  )
})
