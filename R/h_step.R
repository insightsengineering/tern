#' @include control_step.R
NULL

#' Helper Functions for Subgroup Treatment Effect Pattern (STEP) Calculations
#'
#' @description`r lifecycle::badge("stable")`
#'
#' Helper functions that are used internally for the STEP calculations.
#'
#' @inheritParams argument_convention
#' @name h_step
#'
NULL

#' @describeIn h_step creates the windows for STEP, based on the control settings
#'   provided. Returns a list containing the window-selection matrix `sel`
#'   and the interval information matrix `interval`.
#' @param x (`numeric`) biomarker value(s) to use (without `NA`).
#' @param control (named `list`) from `control_step()`.
#'
#' @export
h_step_window <- function(x,
                          control = control_step()) {
  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)
  checkmate::assert_list(control, names = "named")

  sel <- matrix(FALSE, length(x), control$num_points)
  out <- matrix(0, control$num_points, 3)
  colnames(out) <- paste("Interval", c("Center", "Lower", "Upper"))
  if (control$use_percentile) {
    # Create windows according to percentile cutoffs.
    out <- cbind(out, out)
    colnames(out)[1:3] <- paste("Percentile", c("Center", "Lower", "Upper"))
    xs <- seq(0, 1, length = control$num_points + 2)[-1]
    for (i in seq_len(control$num_points)) {
      out[i, 2:3] <- c(
        max(xs[i] - control$bandwidth, 0),
        min(xs[i] + control$bandwidth, 1)
      )
      out[i, 5:6] <- stats::quantile(x, out[i, 2:3])
      sel[, i] <- x >= out[i, 5] & x <= out[i, 6]
    }
    # Center is the middle point of the percentile window.
    out[, 1] <- xs[-control$num_points - 1]
    out[, 4] <- stats::quantile(x, out[, 1])
  } else {
    # Create windows according to cutoffs.
    m <- c(min(x), max(x))
    xs <- seq(m[1], m[2], length = control$num_points + 2)[-1]
    for (i in seq_len(control$num_points)) {
      out[i, 2:3] <- c(
        max(xs[i] - control$bandwidth, m[1]),
        min(xs[i] + control$bandwidth, m[2])
      )
      sel[, i] <- x >= out[i, 2] & x <= out[i, 3]
    }
    # Center is the same as the point for predicting.
    out[, 1] <- xs[-control$num_points - 1]
  }
  list(sel = sel, interval = out)
}

#' @describeIn h_step calculates the estimated treatment effect estimate
#'   on the linear predictor scale and corresponding standard error from a STEP `model` fitted
#'   on `data` given `variables` specification, for a single biomarker value `x`.
#'   This works for both `coxph` and `glm` models, i.e. for calculating log hazard ratio or log odds
#'   ratio estimates. It returns a vector with elements `est` and `se`.
#' @param model the regression model object.
#'
#' @export
h_step_trt_effect <- function(data,
                              model,
                              variables,
                              x) {
  assertthat::assert_that(
    inherits(model, "coxph") || inherits(model, "glm"),
    assertthat::is.number(x)
  )
  assert_df_with_variables(data, variables)
  arm_lvls <- levels(data[[variables$arm]])
  assertthat::assert_that(
    identical(length(arm_lvls), 2L)
  )
  newdata <- data[c(1, 1), ]
  newdata[, variables$biomarker] <- x
  newdata[, variables$arm] <- arm_lvls
  model_terms <- stats::delete.response(stats::terms(model))
  model_frame <- stats::model.frame(model_terms, data = newdata, xlev = model$xlevels)
  mat <- stats::model.matrix(model_terms, data = model_frame, contrasts.arg = model$contrasts)
  coefs <- stats::coef(model)
  # Note: It is important to use the coef subset from matrix, otherwise intercept and
  # strata are included for coxph() models.
  mat <- mat[, names(coefs)]
  mat_diff <- diff(mat)
  est <- mat_diff %*% coefs
  var <- mat_diff %*% stats::vcov(model) %*% t(mat_diff)
  se <- sqrt(var)
  c(
    est = est,
    se = se
  )
}

#' @describeIn h_step builds the model formula used in survival STEP calculations.
#'
#' @export
h_step_survival_formula <- function(variables,
                                    control = control_step()) {
  assertthat::assert_that(
    is.null(variables$covariates) || is.character(variables$covariates)
  )
  assert_list_of_variables(variables[c("arm", "biomarker", "event", "time")])
  form <- paste0("Surv(", variables$time, ", ", variables$event, ") ~ ", variables$arm)
  if (control$degree > 0) {
    form <- paste0(form, " * stats::poly(", variables$biomarker, ", degree = ", control$degree, ", raw = TRUE)")
  }
  if (!is.null(variables$covariates)) {
    form <- paste(form, "+", paste(variables$covariates, collapse = "+"))
  }
  if (!is.null(variables$strata)) {
    form <- paste0(form, " + strata(", paste0(variables$strata, collapse = ", "), ")")
  }
  stats::as.formula(form)
}

#' @describeIn h_step estimates the model with `formula` built based on
#'   `variables` in `data` for a given `subset` and `control` parameters for the
#'   Cox regression, and returns a matrix of number of observations `n`,
#'   `events` as well as log hazard ratio estimates `loghr`, standard error `se`
#'   and Wald confidence interval bounds `ci_lower` and `ci_upper`. One row is
#'   included here for each biomarker value in `x`.
#' @param formula (`formula`)\cr the regression model formula.
#' @param subset (`logical`)\cr subset vector.
#'
#' @export
h_step_survival_est <- function(formula,
                                data,
                                variables,
                                x,
                                subset = rep(TRUE, nrow(data)),
                                control = control_coxph()) {
  assertthat::assert_that(
    inherits(formula, "formula")
  )
  assert_df_with_variables(data, variables)
  checkmate::assert_logical(subset, min.len = 1, any.missing = FALSE)
  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)
  checkmate::assert_list(control, names = "named")
  # Note: `subset` in `coxph` needs to be an expression referring to `data` variables.
  data$.subset <- subset
  coxph_warnings <- NULL
  tryCatch(
    withCallingHandlers(
      expr = {
        fit <- survival::coxph(
          formula = formula,
          data = data,
          subset = .subset,
          ties = control$ties
        )
      },
      warning = function(w) {
        coxph_warnings <<- c(coxph_warnings, w)
        invokeRestart("muffleWarning")
      }
    ),
    finally = {
    }
  )
  if (!is.null(coxph_warnings)) {
    warning(paste(
      "Fit warnings occurred, please consider using a simpler model, or",
      "larger `bandwidth`, less `num_points` in `control_step()` settings"
    ))
  }
  # Produce a matrix with one row per `x` and columns `est` and `se`.
  estimates <- t(vapply(
    X = x,
    FUN = h_step_trt_effect,
    FUN.VALUE = c(1, 2),
    data = data,
    model = fit,
    variables = variables
  ))
  q_norm <- stats::qnorm((1 + control$conf_level) / 2)
  cbind(
    n = fit$n,
    events = fit$nevent,
    loghr = estimates[, "est"],
    se = estimates[, "se"],
    ci_lower = estimates[, "est"] - q_norm * estimates[, "se"],
    ci_upper = estimates[, "est"] + q_norm * estimates[, "se"]
  )
}

#' @describeIn h_step builds the model formula used in response STEP calculations.
#'
#' @export
h_step_rsp_formula <- function(variables,
                               control = c(control_step(), control_logistic())) {
  assertthat::assert_that(
    is.null(variables$covariates) || is.character(variables$covariates)
  )
  assert_list_of_variables(variables[c("arm", "biomarker", "response")])
  response_definition <- sub(
    pattern = "response",
    replacement = variables$response,
    x = control$response_definition,
    fixed = TRUE
  )
  form <- paste0(response_definition, " ~ ", variables$arm)
  if (control$degree > 0) {
    form <- paste0(form, " * stats::poly(", variables$biomarker, ", degree = ", control$degree, ", raw = TRUE)")
  }
  if (!is.null(variables$covariates)) {
    form <- paste(form, "+", paste(variables$covariates, collapse = "+"))
  }
  if (!is.null(variables$strata)) {
    strata_arg <- if (length(variables$strata) > 1) {
      paste0("I(interaction(", paste0(variables$strata, collapse = ", "), "))")
    } else {
      variables$strata
    }
    form <- paste0(form, "+ strata(", strata_arg, ")")
  }
  stats::as.formula(form)
}

#' @describeIn h_step estimates the model with `formula` built based on
#'   `variables` in `data` for a given `subset` and `control` parameters for the
#'   logistic regression, and returns a matrix of number of observations `n`
#'   as well as log odds ratio estimates `logor`, standard error `se`
#'   and Wald confidence interval bounds `ci_lower` and `ci_upper`. One row is
#'   included here for each biomarker value in `x`.
#' @param formula (`formula`)\cr the regression model formula.
#' @param subset (`logical`)\cr subset vector.
#'
#' @export
h_step_rsp_est <- function(formula,
                           data,
                           variables,
                           x,
                           subset = rep(TRUE, nrow(data)),
                           control = control_logistic()) {
  assertthat::assert_that(
    inherits(formula, "formula")
  )
  assert_df_with_variables(data, variables)
  checkmate::assert_logical(subset, min.len = 1, any.missing = FALSE)
  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)
  checkmate::assert_list(control, names = "named")
  # Note: `subset` in `glm` needs to be an expression referring to `data` variables.
  data$.subset <- subset
  fit_warnings <- NULL
  tryCatch(
    withCallingHandlers(
      expr = {
        fit <- if (is.null(variables$strata)) {
          stats::glm(
            formula = formula,
            data = data,
            subset = .subset,
            family = stats::binomial("logit")
          )
        } else {
          # clogit needs coxph and strata imported
          survival::clogit(
            formula = formula,
            data = data,
            subset = .subset
          )
        }
      },
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, w)
        invokeRestart("muffleWarning")
      }
    ),
    finally = {
    }
  )
  if (!is.null(fit_warnings)) {
    warning(paste(
      "Fit warnings occurred, please consider using a simpler model, or",
      "larger `bandwidth`, less `num_points` in `control_step()` settings"
    ))
  }
  # Produce a matrix with one row per `x` and columns `est` and `se`.
  estimates <- t(vapply(
    X = x,
    FUN = h_step_trt_effect,
    FUN.VALUE = c(1, 2),
    data = data,
    model = fit,
    variables = variables
  ))
  q_norm <- stats::qnorm((1 + control$conf_level) / 2)
  cbind(
    n = length(fit$y),
    logor = estimates[, "est"],
    se = estimates[, "se"],
    ci_lower = estimates[, "est"] - q_norm * estimates[, "se"],
    ci_upper = estimates[, "est"] + q_norm * estimates[, "se"]
  )
}
