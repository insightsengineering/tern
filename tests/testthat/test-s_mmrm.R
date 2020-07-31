context("test s_mmrm and associated helper functions")

library(random.cdisc.data)
library(dplyr)

test_that("check_mmrm_vars passes with healthy inputs", {
  data <- radqs(cached = TRUE) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")))

  # No additional covariates.
  vars1 <- list(
    response = "AVAL",
    covariates = c(),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  expect_silent(check_mmrm_vars(vars1, data))

  # Additional covariates.
  vars2 <- vars1
  vars2$covariates <- c("STRATA1", "BMRKR2")
  expect_silent(check_mmrm_vars(vars2, data))
})

test_that("check_mmrm_vars passes with interaction terms in `covariates`", {
  data <- radqs(cached = TRUE) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")))

  vars <- list(
    response = "AVAL",
    covariates = c("ARM*BMRKR1", "STRATA1", "STRATA1:ARM"),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  expect_silent(check_mmrm_vars(vars, data))
})

test_that("check_mmrm_vars works when there are missing values", {
  set.seed(123)
  data <- radqs(cached = TRUE) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
    dplyr::mutate(
      # Introduce extra missing response variable values.
      AVAL = ifelse(
        sample(c(TRUE, FALSE), size = length(AVAL), replace = TRUE, prob = c(0.1, 0.9)),
        NA,
        AVAL
      ),
      # And also covariate values.
      BMRKR1 = ifelse(
        sample(c(TRUE, FALSE), size = length(BMRKR1), replace = TRUE, prob = c(0.1, 0.9)),
        NA,
        BMRKR1
      )
    )

  vars <- list(
    response = "AVAL",
    covariates = c("ARM*BMRKR1", "STRATA1", "STRATA1:ARM"),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  expect_silent(check_mmrm_vars(vars, data))
})

test_that("check_mmrm_vars fails if a variable is missing", {
  data <- radqs(cached = TRUE) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")))
  full_vars <- list(
    response = "AVAL",
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )

  for (var in names(full_vars)) {
    incomplete_vars <- full_vars
    incomplete_vars[[var]] <- NULL
    expect_error(check_mmrm_vars(incomplete_vars, data))
  }
})

test_that("check_mmrm_vars fails if a variable is not included in `data`", {
  data <- radqs(cached = TRUE) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")))
  vars <- list(
    response = "AVAL",
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )

  for (var in names(vars)) {
    var_name <- vars[[var]]
    incomplete_data <- data
    incomplete_data[[var_name]] <- NULL
    expect_error(check_mmrm_vars(vars, incomplete_data))
  }
})

test_that("build_mmrm_formula builds the correct formula", {
  # No additional covariates.
  vars1 <- list(
    response = "AVAL",
    covariates = c(),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  cor_struct1 <- "unstructured"
  result1 <- build_mmrm_formula(vars1, cor_struct1)
  expected1 <- AVAL ~ ARM * AVISIT + (0 + AVISIT | USUBJID)
  expect_equal(result1, expected1)

  # Additional covariates.
  vars2 <- vars1
  vars2$covariates <- c("STRATA1", "BMRKR2")
  cor_struct2 <- "compound-symmetry"
  result2 <- build_mmrm_formula(vars2, cor_struct2)
  expected2 <- AVAL ~ STRATA1 + BMRKR2 + ARM * AVISIT + (1 | USUBJID)
  expect_equal(result2, expected2)
})

test_that("fit_lme4 works with healthy inputs", {
  result <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  expect_s4_class(result, "lmerModLmerTest")
})

test_that("fit_lme4 fails when there are convergence issues", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  expect_error(
    fit_lme4(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data
    ),
    msg = "Model failed to converge",
    fixed = TRUE
  )
})

test_that("get_mmrm_lsmeans can calculate the LS mean results", {
  data <- radqs(cached = TRUE) %>%
    dplyr::filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
    droplevels() %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")))
  vars <- list(
    response = "AVAL",
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  fit <- fit_lme4(
    formula = AVAL ~ ARM * AVISIT + (0 + AVISIT | USUBJID),
    data = data
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  ))
  expect_is(result, "list")
  expect_is(result$estimate, "data.frame")
  expect_is(result$contrast, "data.frame")
})

# Helper function to compare result and expected tables with proper handling of p-value column.
expect_equal_result_tables <- function(result,
                                       expected,
                                       tol = 0.001,
                                       pval_name = "Pr(>|t|)",
                                       pval_threshold = 0.0001) {
  pval_col <- match(pval_name, colnames(result))

  # Compare first non-pvalue columns.
  expect_equal(
    result[, -pval_col],
    expected[, -pval_col],
    tol = tol
  )

  # Then compare p-values which are not below the threshold in the expected table.
  exp_pval_is_below_thresh <- expected[, pval_col] == 0
  expect_equal(
    result[, pval_col][!exp_pval_is_below_thresh],
    expected[, pval_col][!exp_pval_is_below_thresh],
    tol = tol
  )

  # Now expect that the same p-values are below the thresholds in both tables.
  res_pval_is_below_thresh <- result[, pval_col] < pval_threshold
  expect_identical(
    exp_pval_is_below_thresh,
    res_pval_is_below_thresh
  )
}

test_that("s_mmrm works with unstructured covariance matrix and produces same results as SAS", {

  adsl <- radsl(cached = TRUE)
  adqs <- radqs(cached = TRUE)
  adqs_f <- adqs %>%
    dplyr::filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
    droplevels() %>%
    dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM A", "ARM C"))) %>%
    dplyr::mutate(
      AVISITN = rank(AVISITN) %>%
        as.factor() %>%
        as.numeric() %>%
        as.factor()
    )

  mmrm_results <- s_mmrm(
    vars = list(
      response = "AVAL",
      covariates = c("STRATA1", "BMRKR2"),
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = adqs_f,
    cor_struct = "unstructured",
    weights_emmeans = "equal"
  )

  # Compare vs. SAS results calculated with the following statements:
  #
  # PROC MIXED DATA = ana.dat cl method=reml;
  # CLASS USUBJID ARMCD(ref='ARM B') AVISIT(ref='SCREENING') STRATA1(ref='A') BMRKR2(ref='LOW');
  # MODEL AVAL = STRATA1 BMRKR2 ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
  # REPEATED AVISIT / subject=USUBJID type=un r rcorr;
  # LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
  # RUN;
  #
  # See https://github.roche.com/sabanesd/allinR/blob/master/mmrm/comparison/test_mmrm_1.R
  # for reproducing the numbers below.

  # REML criterion value.
  expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    17672.9,
    tol = 0.1
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit, ddf = "Satterthwaite")
  fixed_effects <- as.data.frame(summary_table$coefficients[, c("Estimate", "df", "Pr(>|t|)")])

  expected_fixed_effects <- data.frame(
    Estimate = c(
      45.778, -0.1907, -0.3187, -0.3822, -0.8639, -0.6115, -0.4568,
      9.1165, 14.7637, 19.1756, 23.731, 29.1319, 1.3369, 0.1241, 0.4617,
      -0.1766, 3.0454, 2.6914, 1.955, 1.8577, 2.0059, 0.6498
    ),
    df = c(
      543, 393, 393, 393, 393, 398, 398, 397, 397, 397, 397, 397,
      397, 397, 397, 397, 397, 397, 397, 397, 397, 397
    ),
    "Pr(>|t|)" = c(
      0, 0.6916, 0.5014, 0.4167, 0.0688, 0.5583, 0.663, 0, 0, 0,
      0, 0, 0.3412, 0.9298, 0.7601, 0.9074, 0.0607, 0.0985, 0.2619,
      0.2881, 0.2581, 0.715
    ),
    row.names = c(
      "(Intercept)", "STRATA1B", "STRATA1C", "BMRKR2MEDIUM", "BMRKR2HIGH",
      "ARMCDARM A", "ARMCDARM C", "AVISITWEEK 1 DAY 8", "AVISITWEEK 2 DAY 15",
      "AVISITWEEK 3 DAY 22", "AVISITWEEK 4 DAY 29", "AVISITWEEK 5 DAY 36",
      "ARMCDARM A:AVISITWEEK 1 DAY 8", "ARMCDARM C:AVISITWEEK 1 DAY 8",
      "ARMCDARM A:AVISITWEEK 2 DAY 15", "ARMCDARM C:AVISITWEEK 2 DAY 15",
      "ARMCDARM A:AVISITWEEK 3 DAY 22", "ARMCDARM C:AVISITWEEK 3 DAY 22",
      "ARMCDARM A:AVISITWEEK 4 DAY 29", "ARMCDARM C:AVISITWEEK 4 DAY 29",
      "ARMCDARM A:AVISITWEEK 5 DAY 36", "ARMCDARM C:AVISITWEEK 5 DAY 36"
    ),
    check.names = FALSE  # Necessary to get right p-value column name.
  )

  expect_equal_result_tables(
    fixed_effects,
    expected_fixed_effects
  )

  # Now compare LS means and their contrasts.
  lsmeans_estimates <- mmrm_results$lsmeans$estimate[, c("ARMCD", "AVISIT", "emmean", "lower.CL", "upper.CL")]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      labels = c("ARM B", "ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L),
      labels = c("SCREENING", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"),
    ),
    emmean = c(
      45.1928, 44.5813, 44.7361, 54.3093, 55.0347, 53.9767, 59.9566,
      59.8068, 59.3232, 64.3685, 66.8024, 66.6031, 68.9239, 70.2673,
      70.3249, 74.3247, 75.7191, 74.5178
    ),
    lower.CL = c(
      43.7422, 43.1316, 43.2759, 52.9548, 53.6811, 52.6134, 58.451,
      58.302, 57.8076, 62.6578, 65.0925, 64.8808, 66.9695, 68.3136,
      68.3568, 72.2462, 73.6411, 72.4245
    ),
    upper.CL = c(
      46.6435, 46.0311, 46.1962, 55.6639, 56.3884, 55.3399, 61.4622,
      61.3116, 60.8388, 66.0791, 68.5123, 68.3254, 70.8782, 72.2211,
      72.2929, 76.4033, 77.797, 76.611
    )
  )
  expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tol = 0.00001
  )

  lsmeans_contrasts <-
    mmrm_results$lsmeans$contrast[, c("ARMCD", "AVISIT", "estimate", "df", "lower.CL", "upper.CL", "p.value")]
  expected_lsmeans_contrasts <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      labels = c("ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L),
      labels = c("SCREENING", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36")
    ),
    estimate = c(
      -0.6115, -0.4568, 0.7254, -0.3327, -0.1498, -0.6334, 2.4339,
      2.2346, 1.3435, 1.401, 1.3944, 0.193
    ),
    df = c(398, 398, 396, 396, 393, 393, 397, 396, 398, 398, 397, 397),
    lower.CL = c(
      -2.6636, -2.5161, -1.1909, -2.2556, -2.2797, -2.7707, 0.0142,
      -0.1938, -1.4209, -1.3734, -1.5456, -2.7576
    ),
    upper.CL = c(
      1.4407, 1.6026, 2.6417, 1.5902, 1.9801, 1.504, 4.8537, 4.663,
      4.1078, 4.1754, 4.3343, 3.1437
    ),
    pval = c(
      0.5583, 0.663, 0.4572, 0.7339, 0.8901, 0.5605, 0.0487, 0.0712,
      0.3399, 0.3214, 0.3517, 0.8977
    )
  )
  expect_equal_result_tables(
    lsmeans_contrasts,
    expected_lsmeans_contrasts,
    pval_name = "p.value"
  )
})
