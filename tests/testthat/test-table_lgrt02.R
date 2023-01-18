# Tests variants of LGRT02

adsl <- adsl_raw
adrs <- adrs_raw

adsl_cached <- adsl %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(adsl))
adrs_cached <- adrs %>%
  dplyr::filter(SEX %in% c("F", "M")) %>%
  reapply_varlabels(formatters::var_labels(adrs))

get_adrs <- function() {
  adrs_f <- adrs_cached %>%
    dplyr::filter(
      PARAMCD == "BESRSPI"
    ) %>%
    dplyr::mutate(
      Response = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
      SEX = factor(SEX, c("M", "F")),
      RACE = as.character(RACE),
      RACE = factor(
        RACE,
        levels = c(
          "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN", "BLACK OR AFRICAN AMERICAN",
          "WHITE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"
        )
      )
    )
  formatters::var_labels(adrs_f) <- c(formatters::var_labels(adrs_cached), Response = "Response") # nolint
  adrs_f
}

testthat::test_that("LGRT02 without interaction term is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(response = "Response", arm = "ARMCD", covariates = c("SEX", "RACE", "AGE"))
  )
  conf_level <- 0.95
  df <- broom::tidy(model, conf_level = conf_level) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = conf_level,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LGRT02 with categorical interaction is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("SEX", "AGE"),
      interaction = "SEX"
    )
  )
  conf_level <- 0.95
  df <- broom::tidy(model, conf_level = conf_level) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = conf_level,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LGRT02 with continuous interaction is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("SEX", "AGE"),
      interaction = "AGE"
    )
  )
  conf_level <- 0.95
  df <- broom::tidy(model, conf_level = conf_level, at = c(18, 65)) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = conf_level,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("LGRT02 with setting values indicating an event and custom alpha level is produced correctly", {
  adrs <- get_adrs()
  model <- fit_logistic(
    adrs,
    variables = list(
      response = "Response",
      arm = "ARMCD",
      covariates = c("SEX", "AGE", "RACE")
    ),
    response_definition = "1 - response"
  )
  conf_level <- 0.9
  df <- broom::tidy(model, conf_level = conf_level) %>%
    df_explicit_na(na_level = "_")

  result <- basic_table() %>%
    summarize_logistic(
      conf_level = conf_level,
      drop_and_remove_str = "_"
    ) %>%
    build_table(df = df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
