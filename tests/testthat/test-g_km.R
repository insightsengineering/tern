library(scda)
library(dplyr)

adtte <- synthetic_cdisc_data("rcd_2021_05_05")$adtte

test_that("g_km default plot works", {

  df <- adtte %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  result <- expect_silent(
    g_km(
      df = df,
      variables = variables,
      ci_ribbon = FALSE,
      draw = FALSE
    )
  )
})

test_that("g_km default plot witch ci_ribbon = TRUE works", {

  df <- adtte %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  result <- expect_silent(
    g_km(
      df = df,
      variables = variables,
      ci_ribbon = TRUE,
      draw = FALSE
    )
  )
})

test_that("g_km plot with < = > in group labels works", {

  df <- ex_adtte %>%
    df_explicit_na() %>%
    filter(PARAMCD == "OS", ARM == "A: Drug X", BEP01FL == "Y") %>%
    mutate(is_event = CNSR == 0,
           group = as.factor(ifelse(AGE > 34, ">Median", "<=Median")))

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "group")

  result <- expect_silent(
    g_km(
      df = df,
      variables = variables,
      annot_surv_med = FALSE,
      draw = FALSE
    )
  )
})
