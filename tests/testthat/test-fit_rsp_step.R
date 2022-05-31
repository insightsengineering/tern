library(scda)

raw_data <- local({
  adrs <- synthetic_cdisc_data("rcd_2022_02_28")$adrs # nolint
  adrs %>%
    dplyr::filter(
      PARAMCD == "BESRSPI",
      ARM %in% c("B: Placebo", "A: Drug X")
    ) %>%
    dplyr::mutate(
      # Reorder levels of ARM to have Placebo as reference arm for Odds Ratio calculations.
      ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
      RSP = dplyr::case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
      SEX = factor(SEX)
    )
})

testthat::test_that("fit_rsp_step works as expected with default options", {
  data <- raw_data
  variables <- list(
    arm = "ARM",
    biomarker = "BMRKR1",
    covariates = "AGE",
    response = "RSP"
  )
  result <- testthat::expect_silent(fit_rsp_step(
    variables = variables,
    data = data
  ))
  testthat::expect_is(result, c("matrix", "step"))
  testthat::expect_identical(ncol(result), 11L)
  testthat::expect_identical(
    colnames(result),
    c(
      "Percentile Center", "Percentile Lower", "Percentile Upper",
      "Interval Center", "Interval Lower", "Interval Upper", "n",
      "logor", "se", "ci_lower", "ci_upper"
    )
  )
})

testthat::test_that("fit_rsp_step works as expected with global model fit", {
  data <- raw_data
  variables <- list(
    arm = "ARM",
    biomarker = "BMRKR1",
    covariates = "AGE",
    response = "RSP"
  )
  result <- testthat::expect_silent(fit_rsp_step(
    variables = variables,
    data = data,
    control = c(
      control_logistic(conf_level = 0.8),
      control_step(
        biomarker = data$BMRKR1,
        use_percentile = FALSE,
        num_points = 3L
      )
    )
  ))
  testthat::expect_is(result, c("matrix", "step"))
  testthat::expect_identical(ncol(result), 8L)
  testthat::expect_identical(
    colnames(result),
    c(
      "Interval Center", "Interval Lower", "Interval Upper", "n",
      "logor", "se", "ci_lower", "ci_upper"
    )
  )
})

testthat::test_that("fit_rsp_step works as expected with strata", {
  data <- raw_data
  variables <- list(
    arm = "ARM",
    biomarker = "BMRKR1",
    covariates = "AGE",
    response = "RSP",
    strata = c("STRATA1", "STRATA2")
  )
  result <- testthat::expect_silent(fit_rsp_step(
    variables = variables,
    data = data,
    control = c(control_logistic(), control_step(bandwidth = 0.6))
  ))
  testthat::expect_is(result, c("matrix", "step"))
  testthat::expect_identical(ncol(result), 11L)
  testthat::expect_identical(
    colnames(result),
    c(
      "Percentile Center", "Percentile Lower", "Percentile Upper",
      "Interval Center", "Interval Lower", "Interval Upper", "n",
      "logor", "se", "ci_lower", "ci_upper"
    )
  )
})
