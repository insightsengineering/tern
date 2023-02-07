# Local data pre-processing
adtte_local <- tern_ex_adtte %>%
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
  attr(adtte_local[[columns[i]]], "label") <- labels[i]
}

testthat::test_that("fit_survival_step works as expected with default options", {
  data <- adtte_local
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
  testthat::expect_s3_class(result, c("matrix", "step"))

  res <- testthat::expect_silent(ncol(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(colnames(result))
  testthat::expect_snapshot(res)
})

testthat::test_that("fit_survival_step works as expected with global model fit", {
  data <- adtte_local
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
  testthat::expect_s3_class(result, c("matrix", "step"))

  res <- testthat::expect_silent(ncol(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(colnames(result))
  testthat::expect_snapshot(res)
})

testthat::test_that("fit_survival_step works as expected with null bandwidth", {
  data <- adtte_local
  variables <- list(
    arm = "ARM",
    biomarker = "BMRKR1",
    covariates = c("AGE", "BMRKR2"),
    event = "is_event",
    time = "AVAL"
  )
  result <- testthat::expect_silent(fit_survival_step(
    variables = variables,
    data = data,
    control = c(control_logistic(), control_step(bandwidth = NULL))
  ))
  testthat::expect_s3_class(result, c("matrix", "step"))

  res <- testthat::expect_silent(ncol(result))
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(colnames(result))
  testthat::expect_snapshot(res)
})
