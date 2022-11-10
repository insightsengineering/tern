#' Control Function for `CoxPH` Model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for `CoxPH` model, typically used internally to specify
#' details of `CoxPH` model for [s_coxph_pairwise]. `conf_level` refers to Hazard Ratio estimation.
#'
#' @md
#' @param pval_method (`string`) \cr p-value method for testing hazard ratio = 1.
#'   Default method is \code{"log-rank"}, can also be set to \code{"wald"} or \code{"likelihood"}.
#' @param ties 	(`string`) \cr specifying the method for tie handling. Default is \code{"efron"},
#'   can also be set to \code{"breslow"} or \code{"exact"}. see more in [survival::coxph()]
#' @inheritParams argument_convention
#' @return A list of components with the same names as the arguments
#' @export
#'
control_coxph <- function(pval_method = c("log-rank", "wald", "likelihood"),
                          ties = c("efron", "breslow", "exact"),
                          conf_level = 0.95) {
  pval_method <- match.arg(pval_method)
  ties <- match.arg(ties)
  assert_proportion_value(conf_level)

  list(pval_method = pval_method, ties = ties, conf_level = conf_level)
}

#' Control Function for `survfit` Model for Survival Time
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for `survfit` model, typically used internally to specify
#' details of `survfit` model for [s_surv_time]. `conf_level` refers to survival time estimation.
#'
#' @md
#' @inheritParams argument_convention
#' @param conf_type (`string`) \cr "plain" (default), "log", "log-log" for confidence interval type, \cr
#'    see more in [survival::survfit()]. Note that the option "none" is no longer supported.
#' @param quantiles (`numeric`) \cr of length two to specify the quantiles of survival time.
#' @return A list of components with the same names as the arguments
#' @export
#'
control_surv_time <- function(conf_level = 0.95,
                              conf_type = c("plain", "log", "log-log"),
                              quantiles = c(0.25, 0.75)) {
  conf_type <- match.arg(conf_type)
  checkmate::assert_numeric(quantiles, lower = 0, upper = 1, len = 2, unique = TRUE, sorted = TRUE)
  nullo <- lapply(quantiles, assert_proportion_value)
  assert_proportion_value(conf_level)
  list(conf_level = conf_level, conf_type = conf_type, quantiles = quantiles)
}

#' Control Function for `survfit` Model for Patient's Survival Rate at time point
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for `survfit` model, typically used internally to specify
#' details of `survfit` model for [s_surv_timepoint]. `conf_level` refers to patient risk estimation at a time point.
#'
#' @md
#' @inheritParams argument_convention
#' @inheritParams control_surv_time
#' @return A list of components with the same names as the arguments
#' @export
#'
control_surv_timepoint <- function(conf_level = 0.95,
                                   conf_type = c("plain", "log", "log-log")) {
  conf_type <- match.arg(conf_type)
  assert_proportion_value(conf_level)
  list(
    conf_level = conf_level,
    conf_type = conf_type
  )
}
