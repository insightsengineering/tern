library(random.cdisc.data)
library(dplyr)

test_that("check_mmrm_vars passes with healthy inputs and returns correct labels", {
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
  expect_silent(result1 <- check_mmrm_vars(vars1, data))
  expected1 <- list(
    response = setNames("Analysis Value", "AVAL"),
    id = setNames("Unique Subject Identifier", "USUBJID"),
    arm = setNames("ARM", "ARM"),
    visit = setNames("Analysis Visit", "AVISIT")
  )
  expect_identical(result1, expected1)

  # Additional covariates.
  vars2 <- vars1
  vars2$covariates <- c("STRATA1", "BMRKR2")
  expect_silent(result2 <- check_mmrm_vars(vars2, data))
  expected2 <- c(
    expected1,
    list(
      parts = setNames(
        c("Stratification Factor 1", "Categorical Level Biomarker 2"),
        c("STRATA1", "BMRKR2")
      )
    )
  )
  expect_identical(result2, expected2)
})

test_that("check_mmrm_vars works with interaction terms in `covariates`", {
  data <- radqs(cached = TRUE) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")))

  vars <- list(
    response = "AVAL",
    covariates = c("ARM*BMRKR1", "STRATA1", "STRATA1:ARM"),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )
  expect_silent(result <- check_mmrm_vars(vars, data))
  expected <- list(
    response = setNames("Analysis Value", "AVAL"),
    id = setNames("Unique Subject Identifier", "USUBJID"),
    arm = setNames("ARM", "ARM"),
    visit = setNames("Analysis Visit", "AVISIT"),
    parts = setNames(
      c("ARM", "Continous Level Biomarker 1", "Stratification Factor 1"),
      c("ARM", "BMRKR1", "STRATA1")
    )
  )
  expect_identical(result, expected)
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
  expect_silent(result <- check_mmrm_vars(vars, data))
  expected <- list(
    response = setNames("AVAL", "AVAL"),
    id = setNames("Unique Subject Identifier", "USUBJID"),
    arm = setNames("ARM", "ARM"),
    visit = setNames("Analysis Visit", "AVISIT"),
    parts = setNames(
      c("ARM", "BMRKR1", "Stratification Factor 1"),
      c("ARM", "BMRKR1", "STRATA1")
    )
  )
  expect_identical(result, expected)
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

test_that("fit_lme4_single_optimizer works as expected when there are no warnings or messages", {
  # Default optimizer used.
  result1 <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  expect_s4_class(result1, "lmerModLmerTest")
  expect_identical(attr(result1, "optimizer"), "nloptwrap_bobyqa")
  expect_identical(attr(result1, "messages"), character(0))

  # Non-default optimizer used.
  result2 <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy,
    optimizer = "nmkbw"
  )
  expect_s4_class(result2, "lmerModLmerTest")
  expect_identical(attr(result2, "optimizer"), "nmkbw")
  expect_identical(attr(result2, "messages"), character(0))

  # Results should be equal (without attributes which capture optimizer details).
  expect_equal(result1, result2, check.attributes = FALSE)
})

# Helper function which is another implementation of the covariance matrix estimate from
# https://stackoverflow.com/questions/45650548/get-residual-variance-covariance-matrix-in-lme4
alternative_cov_estimate <- function(fit) {
  # We want to keep the same variable names etc. as in the reference above, therefore no linting here.
  #nolint start
  var.d <- Matrix::crossprod(lme4::getME(fit, "Lambdat"))
  Zt <- lme4::getME(fit, "Zt")
  vr <- stats::sigma(fit)^2
  var.b <- vr * (Matrix::t(Zt) %*% var.d %*% Zt)
  sI <- vr * Matrix::Diagonal(nrow(fit@frame))
  var.y <- var.b + sI
  return(var.y)
  #nolint end
}

test_that("get_lme4_cov_estimate works as expected with a random slope model", {
  fit <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  result <- get_lme4_cov_estimate(fit)
  expected <- as.matrix(alternative_cov_estimate(fit)[1:10, 1:10])  # We use first 10 obs.
  expect_equal(result, expected, check.attributes = FALSE)
  expect_identical(
    attributes(result),
    list(
      dim = c(10L, 10L),
      id = "308",
      n_parameters = 4L
    )
  )
})

test_that("get_lme4_cov_estimate works with a random intercept model", {
  fit <- fit_lme4(
    formula = Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  result <- get_lme4_cov_estimate(fit)
})

test_that("get_lme4_cov_estimate works as expected with unbalanced data and independent of sorting", {
  # Obtain unbalanced data set.
  set.seed(123, kind = "Mersenne-Twister")
  data_unsorted <- lme4::sleepstudy %>%
    dplyr::sample_frac(0.5)  # This randomly samples 50% of the rows of the data set.

  # Fit with unsorted data.
  fit_unsorted <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = data_unsorted
  )
  result_unsorted <- get_lme4_cov_estimate(fit_unsorted)
  expect_identical(
    attributes(result_unsorted),
    list(
      dim = c(10L, 10L),
      id = "372",
      n_parameters = 4L
    )
  )

  # Fit with sorted data.
  data_sorted <- data_unsorted %>%
    dplyr::arrange(Subject, Days)
  fit_sorted <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = data_sorted
  )
  result_sorted <- get_lme4_cov_estimate(fit_sorted)
  expect_identical(
    attributes(result_sorted),
    list(
      dim = c(10L, 10L),
      id = "372",
      n_parameters = 4L
    )
  )

  # Check if the reordered result from unsorted fit equals the sorted fit result.
  order_index <- data_unsorted %>%
    dplyr::filter(Subject == "372") %>%
    dplyr::pull(Days) %>%
    order()
  expect_equal(
    result_unsorted[order_index, order_index],
    result_sorted,
    check.attributes = FALSE
  )
})

test_that("get_lme4_cov_estimate works as expected with a random intercept model and unbalanced data", {
  set.seed(123, kind = "Mersenne-Twister")
  data <- lme4::sleepstudy %>%
    dplyr::sample_frac(0.5)
  fit <- fit_lme4(
    formula = Reaction ~ Days + (1 | Subject),
    data = data
  )
  result <- get_lme4_cov_estimate(fit)
  id_indices <- which(data$Subject == "372")  # We get id 372 here.
  expected <- as.matrix(alternative_cov_estimate(fit)[id_indices, id_indices])
  expect_equal(result, expected, check.attributes = FALSE)
  expect_identical(
    attributes(result),
    list(
      dim = c(10L, 10L),
      id = "372",
      n_parameters = 2L
    )
  )
})

test_that("get_lme4_diagnostics works as expected with a random slope model", {
  fit <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  result <- get_lme4_diagnostics(fit)
  # Expected value from SAS, see
  # https://github.roche.com/sabanesd/allinR/blob/master/mmrm/comparison/test_mmrm_4.R
  expected <- list(
    "REML criterion" = 1743.6,
    AIC = 1751.6,
    AICc = 1751.9,
    BIC = 1755.2
  )
  expect_equal(result, expected, tol = 0.0001)
})

test_that("fit_lme4_single_optimizer correctly captures warnings and messages", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  expect_silent(
    result <- fit_lme4_single_optimizer(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data
    )
  )
  expect_s4_class(result, "lmerModLmerTest")
  expect_identical(attr(result, "optimizer"), "nloptwrap_bobyqa")
  expect_gt(length(attr(result, "messages")), 0)
})

test_that("fit_lme4_single_optimizer fails when there is an error", {
  expect_error(
    fit_lme4_single_optimizer(
      formula = Reaction ~ Days + (Days | Subject),
      data = does_not_exist
    ),
    "bad 'data'"
  )
})

test_that("summary_all_fits works as expected", {
  single_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  all_fits <- list(a = single_fit, b = single_fit, c = single_fit)
  result <- summary_all_fits(all_fits)
  expect_is(result, "list")
  expect_named(result, c("messages", "fixef", "llik", "feval"))
  lapply(
    result,
    expect_named,
    expected = c("a", "b", "c")  # Note that is also implicitly tests the length of the result elements.
  )
})

test_that("refit_lme4_all_optimizers fails when no optimizer succeeds", {
  original_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (factor(Days) | Subject),
    data = lme4::sleepstudy,
    optimizer = "nloptwrap_bobyqa"
  )
  expect_gt(length(attr(original_fit, "messages")), 0)
  expect_error(
    refit_lme4_all_optimizers(original_fit),
    "No optimizer led to a successful model fit"
  )
})

test_that("refit_lme4_all_optimizers can find a working optimizer if there is one", {
  data <- lme4::sleepstudy %>%
    dplyr::mutate(
      days_grouped = cut(
        Days,
        breaks = stats::quantile(Days, probs = seq(0, 1, length = 5)),
        include.lowest = TRUE
      )
    )
  # This optimizer fails.
  failed_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ days_grouped + (days_grouped | Subject),
    data = data,
    optimizer = "nloptwrap_bobyqa"
  )
  expect_gt(length(attr(failed_fit, "messages")), 0)
  # But this one works.
  successful_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ days_grouped + (days_grouped | Subject),
    data = data,
    optimizer = "nloptwrap_neldermead"
  )
  expect_length(attr(successful_fit, "messages"), 0L)
  # So we expect that we can find the working one (or at least one working one).
  final_fit <- refit_lme4_all_optimizers(failed_fit)
  expect_length(attr(final_fit, "messages"), 0L)
  expect_equal(successful_fit, final_fit, check.attributes = FALSE)
})

test_that("refit_lme4_all_optimizers works with parallelization", {
  original_fit <- fit_lme4_single_optimizer(
    formula = Reaction ~ Days + (factor(Days) | Subject),
    data = lme4::sleepstudy,
    optimizer = "nloptwrap_bobyqa"
  )
  expect_gt(length(attr(original_fit, "messages")), 0)
  # Note that here we get the wrong error message somehow in devtools::check.
  # Therefore we don't compare the message text.
  expect_error(
    refit_lme4_all_optimizers(original_fit, n_cores = 4L)
  )
})

test_that("fit_lme4 works with healthy inputs", {
  result <- fit_lme4(
    formula = Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  expect_s4_class(result, "lmerModLmerTest")
})

test_that("fit_lme4 fails when there are convergence issues with all optimizers", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  expect_error(
    fit_lme4(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data,
      n_cores = 2L
    ),
    msg = "No optimizer led to a successful model fit"
  )
})

test_that("fit_lme4 fails when there are convergence issues with a specific optimizer", {
  data <- lme4::sleepstudy
  data$days_copy <- data$Days

  expect_error(
    fit_lme4(
      formula = Reaction ~ Days + (Days + days_copy | Subject),
      data = data,
      optimizer = "bobyqa"
    ),
    msg = "Chosen optimizer 'bobyqa' led to problems during model fit"
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
    data = data,
    optimizer = "bobyqa"
  )
  expect_silent(result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  ))
  expect_is(result, "list")
  expect_is(result$estimates, "data.frame")
  expect_is(result$contrasts, "data.frame")
})

test_that("get_mmrm_lsmeans preserves combined arm levels.", {

  data <- radqs(cached = TRUE) %>%
    dplyr::filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
    droplevels() %>%
    dplyr::mutate(
      ARM = factor(
        ARM,
        levels = c("B: Placebo", "A: Drug X", "C: Combination")
      )
    )

  data$ARM <- combine_levels( # nolint
    data$ARM,
    levels = c("A: Drug X", "C: Combination")
  )

  vars <- list(
    response = "AVAL",
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  )

  fit <- fit_lme4(
    formula = AVAL ~ ARM * AVISIT + (0 + AVISIT | USUBJID),
    data = data,
    optimizer = "bobyqa"
  )


  result <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = 0.95,
    weights = "proportional"
  )

  expect_identical(levels(data$ARM), levels(result$estimates$ARM))
  expect_identical(levels(data$ARM)[-1], levels(result$contrasts$ARM))

})

test_that("s_mmrm works with parallelization", {
  dat <- lme4::sleepstudy %>%
    dplyr::mutate(
      group = factor(rep(c("A", "B"), length = nrow(lme4::sleepstudy))),
      days_grouped = cut(
        Days,
        breaks = stats::quantile(Days, probs = seq(0, 1, length = 5)),
        include.lowest = TRUE
      )
    )
  result <- s_mmrm(
    vars = list(
      response = "Reaction",
      covariates = c(),
      id = "Subject",
      arm = "group",
      visit = "days_grouped"
    ),
    data = dat,
    cor_struct = "compound-symmetry",
    parallel = TRUE
  )
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

# Produces different version of a ADQS subset.
get_adqs <- function(version = c("A", "B")) {
  version <- match.arg(version)
  adqs <- random.cdisc.data::radqs(cached = TRUE)
  set.seed(123, kind = "Mersenne-Twister")  # Because of `sample` below.
  adqs_f <- adqs %>% {
    if (version == "A") {
      dplyr::filter(., .data$PARAMCD == "FKSI-FWB" & !.data$AVISIT %in% c("BASELINE"))
    } else {
      dplyr::filter(., .data$PARAMCD == "FATIGI" & !.data$AVISIT %in% c("BASELINE", "SCREENING"))
    }
  } %>%
    droplevels() %>%
    dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM A", "ARM C"))) %>%
    dplyr::mutate(
      AVISITN = rank(AVISITN) %>%
        as.factor() %>%
        as.numeric() %>%
        as.factor()
    ) %>%
    { # nolint
      if (version == "B") {
        dplyr::mutate(
          .,
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
      } else {
        # No further changes in version A.
        .
      }
    }

  return(adqs_f)
}

test_that("s_mmrm works with unstructured covariance matrix and produces same results as SAS", {
  adqs_f <- get_adqs(version = "A")
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
    weights_emmeans = "equal",
    optimizer = "nloptwrap_neldermead"  # To speed up this test.
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
    tol = 0.0001
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
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[, c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      labels = c("ARM B", "ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L),
      labels = c("SCREENING", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"),
    ),
    estimate = c(
      45.1928, 44.5813, 44.7361, 54.3093, 55.0347, 53.9767, 59.9566,
      59.8068, 59.3232, 64.3685, 66.8024, 66.6031, 68.9239, 70.2673,
      70.3249, 74.3247, 75.7191, 74.5178
    ),
    lower_cl = c(
      43.7422, 43.1316, 43.2759, 52.9548, 53.6811, 52.6134, 58.451,
      58.302, 57.8076, 62.6578, 65.0925, 64.8808, 66.9695, 68.3136,
      68.3568, 72.2462, 73.6411, 72.4245
    ),
    upper_cl = c(
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
    mmrm_results$lsmeans$contrasts[, c("ARMCD", "AVISIT", "estimate", "df", "lower_cl", "upper_cl", "p_value")]
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
    lower_cl = c(
      -2.6636, -2.5161, -1.1909, -2.2556, -2.2797, -2.7707, 0.0142,
      -0.1938, -1.4209, -1.3734, -1.5456, -2.7576
    ),
    upper_cl = c(
      1.4407, 1.6026, 2.6417, 1.5902, 1.9801, 1.504, 4.8537, 4.663,
      4.1078, 4.1754, 4.3343, 3.1437
    ),
    p_value = c(
      0.5583, 0.663, 0.4572, 0.7339, 0.8901, 0.5605, 0.0487, 0.0712,
      0.3399, 0.3214, 0.3517, 0.8977
    )
  )
  expect_equal_result_tables(
    lsmeans_contrasts,
    expected_lsmeans_contrasts,
    pval_name = "p_value"
  )

  # Covariance matrix estimate.
  cov_estimate <- mmrm_results$cov_estimate
  expected_cov_estimate <- matrix(
    c(
      72.7336, 2.1239, -0.9334, -0.8405, 1.0401, 6.0476,
      2.1239, 63.3863, -0.6263, 2.7491, -1.5392, 0.3819, -0.9334, -0.6263,
      78.3635, 1.532, -12.1098, -0.6274, -0.8405, 2.7491, 1.532, 101.23,
      -5.975, 7.0188, 1.0401, -1.5392, -12.1098, -5.975, 132.2, -5.5353,
      6.0476, 0.3819, -0.6274, 7.0188, -5.5353, 149.56
    ),
    nrow = 6L,
    ncol = 6L
  )
  expect_equal(
    cov_estimate,
    expected_cov_estimate,
    check.attributes = FALSE,
    tol = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(17672.9, 17714.9, 17715.3, 17798.7)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tol = 0.00001,
    check.attributes = FALSE
  )
})

test_that("s_mmrm works also with missing data", {
  adqs_f <- get_adqs(version = "B")
  stopifnot(identical(
    nrow(na.omit(adqs_f)),
    469L
  ))

  mmrm_results <- s_mmrm(
    vars = list(
      response = "AVAL",
      covariates = "BMRKR1",
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = adqs_f,
    cor_struct = "unstructured",
    weights_emmeans = "equal",
    optimizer = "bobyqa"
  )

  # Compare vs. SAS results calculated with the following statements:
  #
  # PROC MIXED DATA = ana.dat cl method=reml;
  # CLASS USUBJID ARMCD(ref='ARM B') AVISIT(ref='WEEK 1 DAY 8');
  # MODEL AVAL = BMRKR1 ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
  # REPEATED AVISIT / subject=USUBJID type=un r rcorr;
  # LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
  # RUN;
  #
  # See https://github.roche.com/sabanesd/allinR/blob/master/mmrm/comparison/test_mmrm_2.R
  # for reproducing the numbers below.

  # REML criterion value.
  expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    12003.9,
    tol = 0.00001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit, ddf = "Satterthwaite")
  fixed_effects <- as.data.frame(summary_table$coefficients[, c("Estimate", "df", "Pr(>|t|)")])

  expected_fixed_effects <- data.frame(
    Estimate = c(
      54.3624, 0.07187, 0.8587, -0.2784, 3.6532, 8.619, 16.5113,
      19.3103, 1.9357, 1.8384, -0.3365, 1.5316, -2.6048, -1.0245, -1.1721,
      2.5177
    ),
    df = c(
      411, 390, 311, 311, 365, 371, 366, 333, 363, 374, 358, 378,
      357, 370, 333, 350
    ),
    "Pr(>|t|)" = c(
      0, 0.299, 0.4337, 0.8064, 0.0016, 0, 0, 0, 0.2341, 0.263, 0.8465,
      0.3978, 0.1459, 0.5731, 0.5639, 0.2378
    ),
    row.names = c(
      "(Intercept)", "BMRKR1", "ARMCDARM A", "ARMCDARM C", "AVISITWEEK 2 DAY 15",
      "AVISITWEEK 3 DAY 22", "AVISITWEEK 4 DAY 29", "AVISITWEEK 5 DAY 36",
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
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[, c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      labels = c("ARM B", "ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L),
      labels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"),
    ),
    estimate = c(
      54.7786, 55.6373, 54.5002, 58.4318, 61.2261, 59.9917, 63.3977,
      63.9199, 64.6508, 71.2899, 69.5438, 69.987, 74.089, 73.7756,
      76.3282
    ),
    lower_cl = c(
      53.2579, 54.11, 52.8644, 56.7255, 59.5276, 58.338, 61.5672,
      62.2407, 62.7936, 69.3391, 67.6101, 68.0361, 71.6637, 71.4754,
      73.7687
    ),
    upper_cl = c(
      56.2994, 57.1647, 56.136, 60.1381, 62.9247, 61.6455, 65.2281,
      65.599, 66.5081, 73.2408, 71.4775, 71.9378, 76.5142, 76.0758,
      78.8876
    )
  )
  expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tol = 0.00001
  )

  lsmeans_contrasts <-
    mmrm_results$lsmeans$contrasts[, c("ARMCD", "AVISIT", "estimate", "df", "lower_cl", "upper_cl", "p_value")]
  expected_lsmeans_contrasts <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      labels = c("ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L),
      labels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36")
    ),
    estimate = c(
      0.8587, -0.2784, 2.7943, 1.5599, 0.5222, 1.2532, -1.7461, -1.303,
      -0.3134, 2.2392
    ),
    df = c(311, 311, 327, 327, 330, 330, 327, 326, 319, 320),
    lower_cl = c(
      -1.2969, -2.5123, 0.3867, -0.8163, -1.9619, -1.3544, -4.4937,
      -4.0616, -3.656, -1.2869
    ),
    upper_cl = c(
      3.0143, 1.9554, 5.202, 3.9361, 3.0063, 3.8607, 1.0014, 1.4556,
      3.0292, 5.7654
    ),
    p_value = c(
      0.4337, 0.8064, 0.0231, 0.1975, 0.6795, 0.3451, 0.2121, 0.3535,
      0.8538, 0.2124
    )
  )
  expect_equal_result_tables(
    lsmeans_contrasts,
    expected_lsmeans_contrasts,
    pval_name = "p_value"
  )

  # Covariance matrix estimate.
  cov_estimate <- mmrm_results$cov_estimate
  expected_cov_estimate <- matrix(
    c(
      65.795, 2.0168, -7.5115, -1.5288, -1.0866, 2.0168,
      80.6114, 1.3116, 6.8224, 4.6945, -7.5115, 1.3116, 91.1282, -2.9056,
      -7.5785, -1.5288, 6.8224, -2.9056, 107.34, 9.3734, -1.0866, 4.6945,
      -7.5785, 9.3734, 162.82
    ),
    nrow = 5L,
    ncol = 5L
  )
  expect_equal(
    cov_estimate,
    expected_cov_estimate,
    check.attributes = FALSE,
    tol = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(12003.9, 12033.9, 12034.2, 12093.8)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tol = 0.00001,
    check.attributes = FALSE
  )
})

test_that("s_mmrm works with compound symmetry covariance structure", {
  adqs_f <- get_adqs(version = "B")
  stopifnot(identical(
    nrow(na.omit(adqs_f)),
    469L
  ))

  mmrm_results <- s_mmrm(
    vars = list(
      response = "AVAL",
      covariates = "BMRKR1",
      id = "USUBJID",
      arm = "ARMCD",
      visit = "AVISIT"
    ),
    data = adqs_f,
    cor_struct = "compound-symmetry",
    weights_emmeans = "equal"
  )

  # Compare vs. SAS results calculated with the following statements:
  #
  # PROC MIXED DATA = ana.dat cl method=reml;
  # CLASS USUBJID ARMCD(ref='ARM B') AVISIT(ref='WEEK 1 DAY 8');
  # MODEL AVAL = BMRKR1 ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
  # REPEATED AVISIT / subject=USUBJID type=cs r rcorr;
  # LSMEANS AVISIT*ARMCD / pdiff=all cl alpha=0.05 slice=AVISIT;
  # RUN;
  #
  # See https://github.roche.com/sabanesd/allinR/blob/master/mmrm/comparison/test_mmrm_3.R
  # for reproducing the numbers below.

  # REML criterion value.
  expect_equal(
    lme4::REMLcrit(mmrm_results$fit),
    12088.3,
    tol = 0.0001
  )

  # Fixed effects estimates.
  summary_table <- summary(mmrm_results$fit, ddf = "Satterthwaite")
  fixed_effects <- as.data.frame(summary_table$coefficients[, c("Estimate", "df", "Pr(>|t|)")])

  expected_fixed_effects <- data.frame(
    Estimate = c(
      54.6769, 0.01057, 0.9314, -0.1213, 3.6915, 8.632, 16.4946,
      19.3054, 1.837, 1.6848, -0.3634, 1.3077, -2.609, -1.1365, -1.1479,
      2.331
    ),
    df = c(
      1489, 394, 1610, 1610, 1351, 1352, 1355, 1341, 1348, 1363,
      1333, 1362, 1341, 1360, 1340, 1369
    ),
    "Pr(>|t|)" = c(
      0, 0.8855, 0.4943, 0.9316, 0.007, 0, 0, 0, 0.3418, 0.3889,
      0.8486, 0.5103, 0.1746, 0.5622, 0.5478, 0.2432
    ),
    row.names = c(
      "(Intercept)", "BMRKR1", "ARMCDARM A", "ARMCDARM C", "AVISITWEEK 2 DAY 15",
      "AVISITWEEK 3 DAY 22", "AVISITWEEK 4 DAY 29", "AVISITWEEK 5 DAY 36",
      "ARMCDARM A:AVISITWEEK 2 DAY 15", "ARMCDARM C:AVISITWEEK 2 DAY 15",
      "ARMCDARM A:AVISITWEEK 3 DAY 22", "ARMCDARM C:AVISITWEEK 3 DAY 22",
      "ARMCDARM A:AVISITWEEK 4 DAY 29", "ARMCDARM C:AVISITWEEK 4 DAY 29",
      "ARMCDARM A:AVISITWEEK 5 DAY 36", "ARMCDARM C:AVISITWEEK 5 DAY 36"
    ),
    check.names = FALSE  # Necessary to get right p-value column name.
  )

  expect_equal_result_tables(
    subset(fixed_effects),
    subset(expected_fixed_effects)
  )

  # Now compare LS means and their contrasts.
  lsmeans_estimates <- mmrm_results$lsmeans$estimates[, c("ARMCD", "AVISIT", "estimate", "lower_cl", "upper_cl")]
  expected_lsmeans_estimates <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      labels = c("ARM B", "ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L),
      labels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"),
    ),
    estimate = c(
      54.7381, 55.6694, 54.6168, 58.4295, 61.1979, 59.9931, 63.3701,
      63.938, 64.5566, 71.2327, 69.555, 69.9749, 74.0435, 73.827, 76.2532
    ),
    lower_cl = c(
      52.8532, 53.776, 52.5886, 56.5186, 59.2956, 58.1415, 61.4409,
      62.1698, 62.5992, 69.3389, 67.6778, 68.0813, 72.1323, 72.0149,
      74.2355
    ),
    upper_cl = c(
      56.623, 57.5629, 56.645, 60.3405, 63.1002, 61.8448, 65.2992,
      65.7062, 66.5139, 73.1264, 71.4322, 71.8685, 75.9546, 75.639,
      78.2709
    )
  )
  expect_equal(
    lsmeans_estimates,
    expected_lsmeans_estimates,
    tol = 0.00001
  )

  lsmeans_contrasts <-
    mmrm_results$lsmeans$contrasts[, c("ARMCD", "AVISIT", "estimate", "df", "lower_cl", "upper_cl", "p_value")]
  expected_lsmeans_contrasts <- data.frame(
    ARMCD = factor(
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      labels = c("ARM A", "ARM C"),
    ),
    AVISIT = factor(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L),
      labels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36")
    ),
    estimate = c(
      0.9314, -0.1213, 2.7684, 1.5636, 0.5679, 1.1865, -1.6776, -1.2577,
      -0.2165, 2.2097
    ),
    df = c(1610, 1610, 1610, 1610, 1610, 1610, 1610, 1610, 1610, 1610),
    lower_cl = c(
      -1.7406, -2.8904, 0.07199, -1.0973, -2.0492, -1.5616, -4.345,
      -3.9353, -2.8502, -0.5697
    ),
    upper_cl = c(
      3.6033, 2.6479, 5.4647, 4.2245, 3.185, 3.9346, 0.9897, 1.4199,
      2.4172, 4.9892
    ),
    p_value = c(
      0.4943, 0.9316, 0.0442, 0.2493, 0.6704, 0.3972, 0.2175, 0.357,
      0.8719, 0.1191
    )
  )
  expect_equal_result_tables(
    lsmeans_contrasts,
    expected_lsmeans_contrasts,
    pval_name = "p_value"
  )

  # Covariance matrix estimate.
  cov_estimate <- mmrm_results$cov_estimate
  expected_cov_estimate <- matrix(
    c(
      101.5635, 0.4235, 0.4235, 0.4235, 0.4235, 0.4235,
      101.5635, 0.4235, 0.4235, 0.4235, 0.4235, 0.4235, 101.5635, 0.4235,
      0.4235, 0.4235, 0.4235, 0.4235, 101.5635, 0.4235, 0.4235, 0.4235,
      0.4235, 0.4235, 101.5635
    ),
    nrow = 5L,
    ncol = 5L
  )
  expect_equal(
    cov_estimate,
    expected_cov_estimate,
    check.attributes = FALSE,
    tol = 0.001
  )

  # Diagnostics.
  diagnostics <- mmrm_results$diagnostics
  diagnostics_values <- unlist(diagnostics)
  expected_diagnostics_values <- c(12088.3, 12092.3, 12092.4, 12100.3)
  expect_equal(
    diagnostics_values,
    expected_diagnostics_values,
    tol = 0.00001,
    check.attributes = FALSE
  )
})
