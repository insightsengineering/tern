library(random.cdisc.data)
library(dplyr)

test_that("g_km default plot works", {

  df <- radtte(cached = TRUE) %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

   result <- expect_silent(g_km(
    df = df,
    variables = variables,
    ci_ribbon = FALSE,
    draw = FALSE
  ))
})

test_that("g_km default plot witch ci_ribbon = TRUE works", {

  df <- radtte(cached = TRUE) %>%
    filter(PARAMCD == "OS") %>%
    mutate(is_event = CNSR == 0)

  variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")

  result <- expect_silent(g_km(
    df = df,
    variables = variables,
    ci_ribbon = TRUE,
    draw = FALSE
  ))

})
