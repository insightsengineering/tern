# Tests LGRT02.

library(random.cdisc.data)
library(dplyr)

get_adrs <- function(){
  adsl <- radsl(cached = TRUE)
  adsl <- adsl %>% dplyr::filter(SEX %in% c("F", "M")) #nolint
  adrs <- radrs(adsl, seed = 2)
  adrs_f <- adrs %>%
    dplyr::filter(
      PARAMCD == "BESRSPI"
    ) %>%
    dplyr::mutate(
      Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
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
  adrs_f
}
test_that("LGRT02 without interaction term is produced correctly", {
  adrs <- get_adrs()
  mod1 <- fit_logistic(
    adrs,
    variables = list(response = "Response", arm = "ARMCD", covariates = c("SEX", "RACE", "AGE"))
  )
  df <- broom::tidy(mod1)
  result <- basic_table() %>%
    split_rows_by("variable") %>%
    split_rows_by("term", split_fun = drop_split_levels) %>%
    summarize_logistic(conf_level = 0.95) %>%
    build_table(df = df)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "ARMCD", "", "ARMCD Reference = ARM A", "", "ARM B", "", "ARM C", "SEX",
      "", "SEX Reference = M", "", "F", "RACE", "",
      "RACE Reference = AMERICAN INDIAN OR ALASKA NATIVE", "", "ASIAN", "", "BLACK OR AFRICAN AMERICAN",
      "", "WHITE", "", "MULTIPLE", "", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "AGE", "", "AGE",
      "Degrees of Freedom", "", "", "2", "", "1", "", "1", "",
      "", "", "", "1", "", "", "5", "", "1", "", "1", "", "1", "", "1", "", "1", "", "", "1",
      "Parameter Estimate", "", "", "", "", "-1.426", "", "16.837", "", "", "", "", "1.086", "", "",
      "", "", "-0.088", "", "-0.389", "", "0.151", "", "18.626", "", "-23.632", "", "", "-0.002",
      "Standard Error", "", "", "", "", "0.672", "", "1512.662", "", "", "", "", "0.588", "", "",
      "", "", "1.118", "", "1.17", "", "1.274", "", "17730.37", "", "17730.37", "", "", "0.037",
      "Odds Ratio", "", "", "", "", "0.24", "", ">999.99", "", "", "", "", "2.96", "", "",
      "", "", "0.92", "", "0.68", "", "1.16", "", ">999.99", "", "0", "", "", "1",
      "Wald 95% CI", "", "", "", "", "(0.06, 0.9)", "", "(0, Inf)", "", "", "", "", "(0.94, 9.39)", "", "",
      "", "", "(0.1, 8.2)", "", "(0.07, 6.72)", "", "(0.1, 14.13)", "",
      "(0, Inf)", "", "(0, Inf)", "", "", "(0.93, 1.07)",
      "p-value", "", "", "0.1051", "", "0.0338", "", "0.9911", "", "", "", "", "0.0647", "", "",
      "0.9950", "", "0.9374", "", "0.7395", "", "0.9059", "", "0.9992", "", "0.9989", "", "", "0.9662"
    ),
    .Dim = c(29L, 7L)
  )
  expect_identical(result_matrix, expected_matrix)
})

# To add variants for model with interaction term
