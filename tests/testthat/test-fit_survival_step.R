raw_data <- scda::synthetic_cdisc_data("rcd_2022_02_28")$adtte %>%
  dplyr::filter(
    PARAMCD == "OS",
    ARM %in% c("B: Placebo", "A: Drug X")
  ) %>%
  dplyr::mutate(
    # Reorder levels of ARM to display reference arm before treatment arm.
    ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
    is_event = CNSR == 0
  )
columns <- c("ARM", "is_event")
labels <- c("Treatment Arm", "Event Flag")
for (i in seq_along(columns)) {
  attr(raw_data[[columns[i]]], "label") <- labels[i]
}

testthat::test_that("fit_survival_step works as expected with default options", {
  data <- raw_data
  variables <- list(
    arm = "ARM",
    biomarker = "BMRKR1",
    covariates = c("AGE", "BMRKR2"),
    event = "is_event",
    time = "AVAL"
  )
  result <- testthat::expect_silent(fit_survival_step(
    variables = variables,
    data = data
  ))
  testthat::expect_is(result, c("matrix", "step"))
  testthat::expect_identical(ncol(result), 12L)
  testthat::expect_identical(
    colnames(result),
    c(
      "Percentile Center", "Percentile Lower", "Percentile Upper",
      "Interval Center", "Interval Lower", "Interval Upper", "n", "events",
      "loghr", "se", "ci_lower", "ci_upper"
    )
  )
})

testthat::test_that("fit_survival_step works as expected with global model fit", {
  data <- raw_data
  variables <- list(
    arm = "ARM",
    biomarker = "BMRKR1",
    covariates = c("AGE", "BMRKR2"),
    strata = "STRATA1",
    event = "is_event",
    time = "AVAL"
  )
  result <- testthat::expect_silent(fit_survival_step(
    variables = variables,
    data = data,
    control = c(
      control_coxph(conf_level = 0.8),
      control_step(
        biomarker = data$BMRKR1,
        use_percentile = FALSE,
        num_points = 3L
      )
    )
  ))
  testthat::expect_is(result, c("matrix", "step"))
  testthat::expect_identical(ncol(result), 9L)
  testthat::expect_identical(
    colnames(result),
    c(
      "Interval Center", "Interval Lower", "Interval Upper", "n", "events",
      "loghr", "se", "ci_lower", "ci_upper"
    )
  )
})
