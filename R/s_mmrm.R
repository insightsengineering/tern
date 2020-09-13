#' MMRM
#'
#' Helper function to check the MMRM variables
#'
#' @param vars variable list as per \code{\link{s_mmrm}}
#' @param data data frame that includes the variables
#'
#' @return a corresponding list of variable labels.
#'   For the covariates, the element \code{parts} contains a character vector with one element
#'   per covariates part (i.e. occurring variable in the original covariates specification) is
#'   included.
#'   If a label is not found, the variable name is instead used.
check_mmrm_vars <- function(vars,
                            data) {
  stopifnot(is.list(vars))
  stopifnot(is.data.frame(data))
  labels <- list()

  # Check whether variables are specified and in data.
  is_specified <- function(var) {
    !is.null(vars[[var]])
  }
  is_specified_and_in_data <- function(var) {
    is_specified(var) && all(vars[[var]] %in% names(data))
  }
  check_and_get_label <- function(var) {
    stopifnot(is_specified_and_in_data(var))
    res <- NULL
    for (v in vars[[var]]) {
      label <- attr(data[[v]], "label")
      string <- ifelse(!is.null(label), label, v)
      res <- c(res, setNames(string, v))
    }
    labels[[var]] <<- res  # Saves the result under the `var` element of `labels`.
  }
  check_and_get_label("response")
  check_and_get_label("id")
  check_and_get_label("arm")
  check_and_get_label("visit")
  if (is_specified("covariates")) {
    vars$parts <- unique(unlist(strsplit(vars$covariates, split = "\\*|:")))
    check_and_get_label("parts")
  }

  # Subset data to observations with complete regressors.
  regressor_vars <- c(vars$arm, vars$visit, vars$parts)
  data_complete_regressors <- data %>%
    dplyr::filter(stats::complete.cases(data[, regressor_vars])) %>%
    droplevels()

  # Check variable values in this complete data set.
  response_values <- data_complete_regressors[[vars$response]]
  stopifnot(is.numeric(response_values))

  arm_values <- data_complete_regressors[[vars$arm]]
  stopifnot(is.factor(arm_values) && nlevels(arm_values) >= 2)

  visit_values <- data_complete_regressors[[vars$visit]]
  stopifnot(is.factor(visit_values))

  # Remove all entries where response is NA, droplevels as well.
  data_complete <- data_complete_regressors %>%
    dplyr::filter(!is.na(data_complete_regressors[, vars$response])) %>%
    droplevels()

  # Check all arms will still be present after NA filtering.
  stopifnot(nlevels(arm_values) == nlevels(data_complete[[vars$arm]]))

  # Each arm should have at least have 5 records.
  if (!all(table(data_complete[[vars$arm]]) > 5)) {
    stop(paste("Each group / arm should have at least 5 records with non-missing", vars$response))
  }

  return(labels)
}

#' Helper function to build the MMRM formula
#'
#' @param vars variable list as per \code{\link{s_mmrm}}
#' @param cor_struct correlation structure as per \code{\link{s_mmrm}}
#'
#' @importFrom magrittr %>%
#'
#'
#' @return the complete MMRM formula to use with \code{lmer}
build_mmrm_formula <- function(
  vars,
  cor_struct = c(
    "unstructured",
    "random-quadratic",
    "random-slope",
    "compound-symmetry"
  )
) {
  cor_struct <- match.arg(cor_struct)

  covariates_part <- paste(
    vars$covariates,
    collapse = " + "
  )

  arm_visit_part <- paste0(
    vars$arm,
    "*",
    vars$visit
  )

  random_effects_part <- cor_struct %>%
    switch(
      "unstructured" = "(0 + visit_var | id_var)",
      "random-quadratic" = "(poly(as.numeric(visit_var), df=2) | id_var)",
      "random-slope" = "(as.numeric(visit_var) | id_var)",
      "compound-symmetry" = "(1 | id_var)"
    ) %>%
    gsub("visit_var", vars$visit, x = .) %>% #nolint
    gsub("id_var", vars$id, x = .) #nolint

  rhs_formula <- paste(
    arm_visit_part,
    "+",
    random_effects_part
  )
  if (covariates_part != "") {
    rhs_formula <- paste(
      covariates_part,
      "+",
      rhs_formula
    )
  }

  full_formula <- as.formula(paste(
    vars$response,
    "~",
    rhs_formula
  ))

  return(full_formula)
}

#' Covariance
#'
#' Extract the covariance matrix estimate from a lme4 fit.
#'
#' @param fit the \code{merMod} object (e.g. coming from \code{\link{fit_lme4_single_optimizer}}).
#'
#' @return a matrix containing the covariance matrix estimate.
#'   The following additional attributes are attached:
#'   \describe{
#'     \item{id}{which subject has been used (this is one subject with the maximum number of observations)}
#'     \item{n_parameters}{number of covariance parameters that were used in the fit}
#'   }
#'
#' @details This is adapted from \url{https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q1/000558.html}.
#'   Note that the order of rows/columns in the returned matrix corresponds to the order of the
#'   \code{id}'s observations in the original data set.
#'
#' @importFrom lme4 getME
#' @importFrom stats sigma
get_lme4_cov_estimate <- function(fit) {
  stopifnot(is(fit, "merMod"))
  # A list of the grouping variables (factors) involved in the random effect terms.
  grouping_factors <- lme4::getME(fit, "flist")
  # We only have one here (id).
  stopifnot(identical(length(grouping_factors), 1L))
  id_var <- grouping_factors[[1L]]
  # Obtain one id with the maximum number of time points.
  id1 <- names(which.max(table(id_var)))
  id1_indices <- which(id_var == id1)
  # A is the scaled sparse model matrix (class "dgCMatrix") for the unit, orthogonal
  # random effects, U, equal to getME(.,"Zt") %*% getME(.,"Lambdat").
  # A = Z^T * Lambda^T, where
  # Z = random-effects model matrix and
  # Lambda = relative covariance factor Lambda of the random effects
  # (note "relative", i.e. it does not include the sigma yet, therefore it is multiplied
  # below in the last step)
  a_mat <- as.matrix(lme4::getME(fit, "A"))
  # Find out where in the row space id1 random effects are.
  a_mat_id1_cols <- a_mat[, id1_indices]
  row_indices_with_id1 <- which(apply(a_mat_id1_cols, 1L, function(x) any(x != 0)))
  id1_row_indices <- seq(from = min(row_indices_with_id1), to = max(row_indices_with_id1))
  # We can now extract the relevant submatrix of A.
  a_mat_id1 <- a_mat[id1_row_indices, id1_indices, drop = FALSE]  # Make sure we don't get vector here.
  # We account for the residual variance.
  id1_dim <- length(id1_indices)
  identity_id1 <- diag(1, id1_dim)
  cov_estimate <- stats::sigma(fit)^2 * (crossprod(a_mat_id1) + identity_id1)
  # Get the number of variance paramaters. Note: The sigma2 is not counted in "m",
  # and for the unstructured case we have effectively one parameter too much.
  # However, this does not have any effect on the covariance matrix estimate itself.
  n_parameters <- as.integer(min(lme4::getME(fit, "m") + 1, id1_dim * (id1_dim + 1) / 2))
  # Finally, we add the attributes.
  result <- structure(
    unname(cov_estimate),
    id = id1,
    n_parameters = n_parameters
  )
  return(result)
}

#' Linear mixed model
#'
#' Compute the model diagnostic statistics for a linear mixed model fit
#'
#' @param fit object of class \code{lmerModLmerTest} fit with REML using \code{\link[lmerTest]{lmer}}
#' @param cov_est the covariance estimate coming from \code{\link{get_lme4_cov_estimate}}
#'
#' @return a list with the REML criterion, the AIC, AICc and BIC.
#'
#' @details AICc is here defined as
#' \deqn{-2 * loglik + 2 * df * (m / (m - df - 1))}
#' where \code{loglik} is the restricted log likelihood value, \code{df} is the number of covariance parameters
#' and \code{m} is the number of observations minus the number of fixed effects, or \code{df + 2} if it is smaller
#' than that. The same \code{df} is used for AIC and BIC. Note that for BIC, the \code{n} used is the number of
#' subjects (instead of the number of observations as in AIC and AICc). This matches the definitions in SAS.
#'
#' @importFrom lme4 isREML getME
#' @importFrom stats logLik
get_lme4_diagnostics <- function(fit,
                                 cov_est = get_lme4_cov_estimate(fit)) {
  stopifnot(is(fit, "lmerModLmerTest"))
  stopifnot(lme4::isREML(fit))

  n_obs <- lme4::getME(fit, "n")
  df <- attr(cov_est, "n_parameters")
  m <- max(df + 2, n_obs - lme4::getME(fit, "p"))
  log_lik <- as.numeric(stats::logLik(fit))
  n_subjects <- as.numeric(lme4::getME(fit, "l_i"))

  result <- list(
    "REML criterion" = -2 * log_lik,
    AIC = -2 * log_lik + 2 * df,
    AICc = -2 * log_lik + 2 * df * (m / (m - df - 1)),
    BIC = -2 * log_lik + df * log(n_subjects)
  )
  return(result)
}

#' Fitting
#'
#' Internal helper function to fit an lme4 model with a single optimizer, while capturing messages and warnings.
#'
#' @param formula the lme4 formula
#' @param data the data set
#' @param optimizer the optimizer to use
#'
#' @return the fitted \code{lmerMerModTest} object, with additional attributes \code{messages} and \code{optimizer}.
#'
#' @note While we are not directly importing functions from \code{dfoptim} or \code{optimx} here, we rely on them
#'   being available to \code{lme4} (which has these packages only in "Suggests"). Therefore we note the imports below.
#'
#' @importFrom lmerTest lmer
#' @importFrom lme4 lmerControl
#' @importFrom purrr quietly
#' @importFrom dfoptim nmkb
#' @importFrom optimx optimx
fit_lme4_single_optimizer <- function(
  formula,
  data,
  optimizer = c(
    "automatic",
    "nloptwrap_neldermead",
    "nloptwrap_bobyqa",
    "nlminbwrap",
    "bobyqa",
    "neldermead",
    "nmkbw",
    "optimx_lbfgsb"
  )
) {
  optimizer <- match.arg(optimizer)

  if (optimizer == "automatic") {
    optimizer <- "nloptwrap_bobyqa"
  }
  control <- lme4::lmerControl(
    # We need this to be able to fit unstructured covariance matrix models.
    check.nobs.vs.nRE = "ignore",
    optimizer = switch(
      optimizer,
      "nloptwrap_neldermead" = "nloptwrap",
      "nloptwrap_bobyqa" = "nloptwrap",
      "nlminbwrap" = "nlminbwrap",
      "bobyqa" = "bobyqa",
      "neldermead" = "Nelder_Mead",
      "nmkbw" = "nmkbw",
      "optimx_lbfgsb" = "optimx"
    ),
    optCtrl = switch(
      optimizer,
      "nloptwrap_neldermead" = list(algorithm = "NLOPT_LN_NELDERMEAD"),
      "nloptwrap_bobyqa" = list(algorithm = "NLOPT_LN_BOBYQA"),
      "optimx_lbfgsb" = list(method = "L-BFGS-B"),
      list()
    )
  )
  quiet_lmer <- purrr::quietly(lmerTest::lmer)
  quiet_fit <- quiet_lmer(
    formula = formula,
    REML = TRUE,
    data = data,
    control = control
  )
  result <- structure(
    quiet_fit$result,
    messages = c(quiet_fit$warnings, quiet_fit$messages),
    optimizer = optimizer
  )
  return(result)
}

#' Helper function to extract summaries from a list of lme4 fit objects.
#'
#' This is inspired by the internal unexported method \code{summary.allFit} from \code{lme4}.
#'
#' @param all_fits the list with fits from \code{\link{fit_lme4_single_optimizer}}.
#'
#' @return a list with elements \code{messages} (list of all messages), \code{fixef} (list of all fixed effects),
#'   \code{llik} (vector of all log-likelihood values), \code{feval} (vector of number of function evaluations).
#'
#' @importFrom lme4 fixef
#' @importFrom stats logLik
summary_all_fits <- function(all_fits) {
  messages <- lapply(all_fits, function(x) attr(x, "messages"))
  fixef <- lapply(all_fits, lme4::fixef)
  llik <- vapply(all_fits, stats::logLik, numeric(1))
  feval <- vapply(all_fits, function(x) x@optinfo$feval, numeric(1))
  res <- list(
    messages = messages,
    fixef = fixef,
    llik = llik,
    feval = feval
  )
  return(res)
}

#' Re-Fitting
#'
#' Refit an lme4 model with all possible optimizers and return the best result.
#'
#' @param original_fit The original model fit coming from \code{\link{fit_lme4_single_optimizer}}.
#' @param n_cores positive integer specifying the number of cores which could in principle be used for
#'   parallelizing the computations on Linux or Mac machines.
#'
#' @return The "best" model fit, defined as a converging fit without any warnings or messages, leading
#'   to the highest log-likelihood. If no optimizer succeeds, then an error is thrown.
#'
#' @note Currently there are 7 optimizers in total. Since 1 optimizer is already used in the original fit,
#'   only 6 additional optimizer runs need to be done. Thus the maximum number of parallel runs is 6.
#'   For Windows, no parallelization is currently implemented.
#'
#' @importFrom lme4 allFit
#' @importFrom parallel mclapply
#' @importFrom purrr quietly
refit_lme4_all_optimizers <- function(original_fit,
                                      n_cores = 1L) {
  stopifnot(is.integer(n_cores), n_cores > 0, identical(length(n_cores), 1L))

  # Extract the components of the original fit.
  formula <- formula(original_fit)
  data <- original_fit@frame
  optimizer <- attr(original_fit, "optimizer")

  # Which optimizers we want to try here.
  all_optimizers <- setdiff(
    c(
      "nloptwrap_neldermead",
      "nloptwrap_bobyqa",
      "nlminbwrap",
      "bobyqa",
      "neldermead",
      "nmkbw",
      "optimx_lbfgsb"
    ),
    optimizer
  )

  n_cores_used <- ifelse(
    .Platform$OS.type == "windows",
    1L,
    min(
      length(all_optimizers),
      n_cores
    )
  )
  quiet_mclapply <- purrr::quietly(parallel::mclapply)
  all_fits <- quiet_mclapply(
    X = all_optimizers,
    FUN = function(opt) {
      fit_lme4_single_optimizer(
        formula = formula,
        data = data,
        optimizer = opt
      )
    },
    mc.cores = n_cores_used,
    mc.silent = TRUE
  )$result
  names(all_fits) <- all_optimizers
  all_fits_summary <- summary_all_fits(all_fits)

  # Find the results that are ok:
  is_ok <- sapply(all_fits_summary$messages, identical, y = character(0))
  if (!any(is_ok)) {
    stop(
      "No optimizer led to a successful model fit. ",
      "Please try to use a different covariance structure or other covariates."
    )
  }

  # Return the best result in terms of log-likelihood.
  log_liks <- all_fits_summary$llik
  best_optimizer <- names(which.max(log_liks[is_ok]))
  best_fit <- all_fits[[best_optimizer]]
  return(best_fit)
}

#' Fitting
#'
#' Helper function to fit the MMRM with lme4 and lmerTest
#'
#' @param formula the MMRM formula (it could also be another lme4 formula)
#' @param data the data frame
#' @param optimizer the optimizer to use
#' @param n_cores positive integer for number of cores to parallelize over the "automatic" optimizer search
#'
#' @return the \code{lmerModLmerTest} object
fit_lme4 <- function(
  formula,
  data,
  optimizer = "automatic",
  n_cores = 1L
) {
  # First fit.
  fit <- fit_lme4_single_optimizer(
    formula = formula,
    data = data,
    optimizer = optimizer
  )

  # Check that the model converged without messages.
  messages <- attr(fit, "messages")
  if (identical(messages, character(0))) {
    # If so, return this one.
    return(fit)
  } else if (optimizer != "automatic") {
    # We fail if this optimizer was specified deliberately.
    stop(paste0(
      "Chosen optimizer '", optimizer, "' led to problems during model fit:\n",
      paste(messages, collapse = "; "), "\n",
      "Consider using the 'automatic' optimizer."
    ))
  } else {
    # Refit with all possible optimizers and get the best one.
    refit_lme4_all_optimizers(fit, n_cores = n_cores)
  }
}

#' Extract LS means
#'
#' Helper function to extract the LS means from an MMRM fit.
#'
#' @param fit result of \code{\link{fit_lme4}}
#' @param vars variable list as per \code{\link{s_mmrm}} input
#' @param conf_level confidence level
#' @param weights string specifying the type of weights to be used for the LS means,
#'   see \code{\link[emmeans]{emmeans}} for details.
#'
#' @return A list with the LS means `estimates` and `contrasts` results between the treatment
#'   and control arm groups at the different visits.
#'
#' @importFrom emmeans emmeans contrast
#' @importFrom dplyr filter group_by_at left_join mutate n summarise rename ungroup
#' @importFrom rlang := !!
#' @importFrom rtables var_labels
#' @importFrom rlang .data
#' @importFrom stats confint
#'
get_mmrm_lsmeans <- function(fit,
                             vars,
                             conf_level,
                             weights) {
  data_complete <- fit@frame
  lsmeans <- emmeans::emmeans(
    fit,
    mode = "satterthwaite",
    specs = as.formula(paste("~ ", vars$arm, "|", vars$visit)),
    weights = weights,
    data = data_complete
  )

  # Relative Reduction (in change from baseline) is calculated using model based
  # LS means as 100*(LS mean change from baseline in Control Pooled group –
  # LS mean change from baseline in Treatment Group)/LS mean change from
  # baseline in Control Pooled group.

  # Adjusted estimate for each arm.
  estimates <- confint(lsmeans, level = conf_level) %>%
    as.data.frame() %>%
    dplyr::rename(
      estimate = .data$emmean,
      se = .data$SE,
      lower_cl = .data$lower.CL,
      upper_cl = .data$upper.CL
    )

  data_n <- data_complete %>%
    dplyr::group_by_at(.vars = c(vars$visit, vars$arm)) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup()

  estimates <- suppressWarnings(
    # We don't have labels in `estimates`, which triggers a warning.
    estimates %>%
      dplyr::left_join(data_n, by = c(vars$visit, vars$arm))
  )

  # Get LS means for reference group to join into full dataframe so that relative reduction in
  # LS mean (mean of response variable) can be computed with respect to reference level (e.g. ARM A).
  arm_levels <- levels(data_complete[[vars$arm]])
  reference_level <- arm_levels[1]
  means_at_ref <- estimates %>%
    dplyr::filter(!!as.symbol(vars$arm) == reference_level) %>%
    dplyr::select(c(vars$visit, "estimate")) %>%
    dplyr::rename(ref = .data$estimate)

  relative_reduc <- estimates %>%
    dplyr::filter(!!as.symbol(vars$arm) != reference_level) %>%
    dplyr::left_join(means_at_ref, by = c(vars$visit)) %>%
    dplyr::mutate(relative_reduc = (.data$ref - .data$estimate) / .data$ref) %>%
    dplyr::select(c(vars$visit, vars$arm, "relative_reduc"))

  # Start with the differences between LS means.
  sum_fit_diff <- summary(
    emmeans::contrast(lsmeans, method = "trt.vs.ctrl", parens = NULL),
    level = conf_level,
    infer = c(TRUE, TRUE),
    adjust = "none"
  )

  # Get the comparison group name from "contrast" column,
  # e.g. "ARMB - ARMA" shall return "ARMB", i.e. remove " - ARMA" part.
  contrasts <- sum_fit_diff %>%
    dplyr::mutate(
      col_by = factor(
        gsub(paste0("\\s-\\s", reference_level), "", contrast),
        levels = arm_levels
      )
    ) %>%
    dplyr::select(-contrast) %>%
    dplyr::rename(!!as.symbol(vars$arm) := .data$col_by) %>%
    dplyr::left_join(relative_reduc, by = c(vars$visit, vars$arm)) %>%
    dplyr::mutate(!!as.symbol(vars$arm) := droplevels(!!as.symbol(vars$arm), exclude = reference_level)) %>%
    dplyr::rename(
      se = .data$SE,
      lower_cl = .data$lower.CL,
      upper_cl = .data$upper.CL,
      t_stat = .data$t.ratio,
      p_value = .data$p.value
    )

  results <- list(
    estimates = estimates,
    contrasts = contrasts
  )
  return(results)
}

#' Summary
#'
#' Summary function for an MMRM analysis
#'
#' @param vars a \code{list} specifying the variables in the MMRM. The following elements need
#'   to be included as character vectors and match corresponding columns in \code{data}:
#'   \describe{
#'   \item{response}{the response variable}
#'   \item{covariates}{the additional covariate terms (might also include interactions)}
#'   \item{id}{the subject ID variable}
#'   \item{arm}{the treatment group variable (factor)}
#'   \item{visit}{the visit variable (factor)}
#'   }
#'   Note that the main effects and interaction of `arm` and `visit` are by default included in the model.
#' @param data a \code{data.frame} with all the variables specified in
#'   \code{vars}. Records with missing values in any independent variables
#'   will be excluded.
#' @param conf_level confidence level. Must be number greater than 0 and less
#'   than 1. (Default: 0.95)
#' @param cor_struct a string specifying the correlation structure, defaults to
#'   \code{"unstructured"}. See the details.
#' @param weights_emmeans argument from \code{\link[emmeans]{emmeans}}, "proportional" by default.
#' @param optimizer a string specifying the optimization algorithm which should be used. By default, "automatic"
#'   will (if necessary) try all possible optimization algorithms and choose the best result. If another algorithm
#'   is chosen and does not give a valid result, an error will occur.
#' @param parallel flag that controls whether "automatic" optimizer search can use available free cores on the
#'   machine (not default).
#'
#' @details Only Satterthwaite adjusted degrees of freedom (d.f.) are supported, because they
#'   match the results obtained in SAS (confirmed for unstructured and compound symmetry correlation structures).
#'
#'   For the correlation structure (\code{cor_struct}), the user can choose among the following options, sorted
#'   in descending number of variance parameters:
#'   \describe{
#'   \item{unstructured}{Unstructured covariance matrix. This is the most flexible choice and default.
#'      If there are \code{T} visits, then \code{T * (T+1) / 2} variance parameters are used.
#'      Note: the current actual implementation uses one more variance parameter, which does not have any
#'      effect of the results. Therefore we report here the actually relevant number of parameters.}
#'   \item{random-quadratic}{Random quadratic spline for the random effects of the time variable.
#'      7 variance parameters are used.}
#'   \item{random-slope}{Random slope for the random effects of the time variable. 4 variance parameters are used.}
#'   \item{compound-symmetry}{Constant correlation between visits. 2 variance parameters are used.}
#'   }
#'
#'   For the \code{optimizer}, the user can choose among the following alternatives to the recommended "automatic":
#'   \describe{
#'   \item{nloptwrap_neldermead}{NLopt version of the Nelder-Mead algorithm (via package \code{nloptr})}
#'   \item{nloptwrap_bobyqa}{NLopt version of the BOBYQA algorithm (via package \code{nloptr})}
#'   \item{bobyqa}{BOBYQA algorithm (via package \code{minqa})}
#'   \item{nlminbwrap}{nlminb algorithm (wrapper for \code{\link[stats]{nlminb})}}
#'   \item{neldermead}{lme4 version of the Nelder-Mead algorithm with box constraints (via package \code{lme4})}
#'   \item{nmkbw}{Nelder-Mead algorithm (via package \code{dfoptim})}
#'   \item{optimx_lbfgsb}{L-BFGS-B algorithm (via package \code{optimx})}
#'   }
#'
#' @return An \code{mmrm} object which is a list with MMRM results:
#' \describe{
#'   \item{fit}{The \code{lmerModLmerTest} object which was fitted to the data. Note that the attribute \code{optimizer}
#'     contains the finally used optimization algorithm, which can be useful for refitting the model later on.}
#'   \item{cov_estimate}{The matrix with the covariance matrix estimate.}
#'   \item{diagnostics}{A list with model diagnostic statistics (REML criterion, AIC, corrected AIC, BIC).}
#'   \item{lsmeans}{This is a list with data frames \code{estimates} and \code{contrasts}.}
#'   \item{vars}{The variable list.}
#'   \item{labels}{Corresponding list with variable labels extracted from \code{data}.}
#'   \item{ref_level}{The reference level for the arm variable, which is always the first level.}
#'   \item{conf_level}{The confidence level which was used to construct the confidence intervals.}
#' }
#'
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' \code{lme4}. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
#'
#' @export
#' @importFrom utils.nest get_free_cores
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#' adqs_f <- adqs %>%
#'   dplyr::filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM A", "ARM C"))) %>%
#'   dplyr::mutate(
#'     AVISITN = rank(AVISITN) %>%
#'     as.factor() %>%
#'     as.numeric() %>%
#'     as.factor()
#'   )
#' var_labels(adqs_f) <- var_labels(adqs)
#'
#' \dontrun{
#' # sometimes results in failure to converge with 1 negative eigenvalue
#' mmrm_results <- s_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARMCD",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "unstructured",
#'   weights_emmeans = "equal",
#'   optimizer = "nloptwrap_neldermead"  # Only to speed up this example.
#' )
#' }
s_mmrm <- function(
  vars = list(
    response = "AVAL",
    covariates = c(),
    id = "USUBJID",
    arm = "ARM",
    visit = "AVISIT"
  ),
  data,
  conf_level = 0.95,
  cor_struct = "unstructured",
  weights_emmeans = "proportional",
  optimizer = "automatic",
  parallel = FALSE
) {
  labels <- check_mmrm_vars(vars, data)
  check_conf_level(conf_level)
  stopifnot(is.logical(parallel), identical(length(parallel), 1L))

  formula <- build_mmrm_formula(vars, cor_struct)

  fit <- fit_lme4(
    formula = formula,
    data = data,
    optimizer = optimizer,
    n_cores = ifelse(parallel, utils.nest::get_free_cores(), 1L)
  )

  lsmeans <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights_emmeans
  )

  cov_estimate <- get_lme4_cov_estimate(fit)
  id_rows <- which(fit@frame[[vars$id]] == attr(cov_estimate, "id"))
  visits <- fit@frame[[vars$visit]][id_rows]
  rownames(cov_estimate) <- colnames(cov_estimate) <- as.character(visits)

  diagnostics <- get_lme4_diagnostics(fit, cov_est = cov_estimate)

  results <- list(
    fit = fit,
    cov_estimate = cov_estimate,
    diagnostics = diagnostics,
    lsmeans = lsmeans,
    vars = vars,
    labels = labels,
    ref_level = levels(data[[vars$arm]])[1],
    conf_level = conf_level
  )
  class(results) <- "mmrm"
  return(results)
}
