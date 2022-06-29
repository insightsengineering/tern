#' Custom Tidy Methods for Cox Regression Results
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Custom tidy methods used to tidy Cox regression summary results
#'
#' @inheritParams argument_convention
#'
#' @name tidy_cox
NULL

#' @describeIn tidy_cox Custom tidy method for [survival::coxph()] summary results.
#'
#' Tidy the [survival::coxph()] results into a `data.frame` to extract model results.
#'
#' @inheritParams argument_convention
#' @method tidy summary.coxph
#'
#' @examples
#' library(survival)
#' library(broom)
#' library(rtables)
#'
#' set.seed(1, kind = "Mersenne-Twister")
#'
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum),
#'     covar2 = factor(
#'       sample(as.factor(enum)),
#'       levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' labels <- c("armcd" = "ARM", "covar1" = "A Covariate Label", "covar2" = "Sex (F/M)")
#' formatters::var_labels(dta_bladder)[names(labels)] <- labels
#' dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' formula <- "survival::Surv(time, status) ~ armcd + covar1"
#' msum <- summary(coxph(stats::as.formula(formula), data = dta_bladder))
#' tidy(msum)
#'
#' @export
tidy.summary.coxph <- function(x, # nolint
                               ...) {
  assertthat::assert_that(
    class(x) == "summary.coxph"
  )

  pval <- x$coefficients
  confint <- x$conf.int
  levels <- rownames(pval)

  pval <- tibble::as_tibble(pval)
  confint <- tibble::as_tibble(confint)

  ret <- cbind(pval[, grepl("Pr", names(pval))], confint)
  ret$level <- levels
  ret$n <- x[["n"]]
  ret
}

#' @describeIn tidy_cox Custom tidy method for a Univariate Cox Regression
#'
#' Tidy up the result of a Cox regression model fitted by [`fit_coxreg_univar()`].
#'
#' @inheritParams argument_convention
#' @param x (`list`)\cr Result of the Cox regression model fitted by [`fit_coxreg_univar()`].
#' @method tidy coxreg.univar
#'
#' @examples
#' library(broom)
#'
#' ## Cox regression: arm + 1 covariate.
#' mod1 <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = "covar1"
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91)
#' )
#'
#' ## Cox regression: arm + 1 covariate + interaction, 2 candidate covariates.
#' mod2 <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91, interaction = TRUE)
#' )
#'
#' tidy(mod1)
#' tidy(mod2)
#'
#' @export
tidy.coxreg.univar <- function(x, # nolint
                               ...) {
  assertthat::assert_that(
    class(x) == "coxreg.univar"
  )

  mod <- x$mod
  vars <- c(x$vars$arm, x$vars$covariates)
  has_arm <- "arm" %in% names(x$vars)

  result <- if (!has_arm) {
    Map(
      mod = mod, vars = vars,
      f = function(mod, vars) {
        h_coxreg_multivar_extract(
          var = vars,
          data = x$data,
          mod = mod,
          control = x$control
        )
      }
    )
  } else if (x$control$interaction) {
    Map(
      mod = mod, covar = vars,
      f = function(mod, covar) {
        h_coxreg_extract_interaction(
          effect = x$vars$arm, covar = covar, mod = mod, data = x$data,
          at = x$at, control = x$control
        )
      }
    )
  } else {
    Map(
      mod = mod, vars = vars,
      f = function(mod, vars) {
        h_coxreg_univar_extract(
          effect = x$vars$arm, covar = vars, data = x$data, mod = mod,
          control = x$control
        )
      }
    )
  }
  result <- do.call(rbind, result)

  result$ci <- Map(lcl = result$lcl, ucl = result$ucl, f = function(lcl, ucl) c(lcl, ucl))
  result$n <- lapply(result$n, empty_vector_if_na)
  result$ci <- lapply(result$ci, empty_vector_if_na)
  result$hr <- lapply(result$hr, empty_vector_if_na)
  if (x$control$interaction) {
    result$pval_inter <- lapply(result$pval_inter, empty_vector_if_na)
    # Remove interaction p-values due to change in specifications.
    result$pval[result$effect != "Treatment:"] <- NA
  }
  result$pval <- lapply(result$pval, empty_vector_if_na)
  attr(result, "conf_level") <- x$control$conf_level
  result
}

#' @describeIn tidy_cox Custom tidy method for a Multi-variable Cox Regression
#'
#' Tidy up the result of a Cox regression model fitted by [`fit_coxreg_multivar()`].
#'
#' @inheritParams argument_convention
#' @param x (`list`)\cr Result of the Cox regression model fitted by [`fit_coxreg_multivar()`].
#' @method tidy coxreg.multivar
#'
#' @examples
#' library(broom)
#'
#' ## Cox regression: multivariate Cox regression.
#' multivar_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#'
#' broom::tidy(multivar_model)
#'
#' @export
tidy.coxreg.multivar <- function(x, # nolint
                                 ...) {
  checkmate::assert_class(x, "coxreg.multivar")
  vars <- c(x$vars$arm, x$vars$covariates)

  # Convert the model summaries to data.
  result <- Map(
    vars = vars,
    f = function(vars) {
      h_coxreg_multivar_extract(
        var = vars, data = x$data,
        mod = x$mod, control = x$control
      )
    }
  )
  result <- do.call(rbind, result)

  result$ci <- Map(lcl = result$lcl, ucl = result$ucl, f = function(lcl, ucl) c(lcl, ucl))
  result$ci <- lapply(result$ci, empty_vector_if_na)
  result$hr <- lapply(result$hr, empty_vector_if_na)
  result$pval <- lapply(result$pval, empty_vector_if_na)
  result <- result[, names(result) != "n"]
  attr(result, "conf_level") <- x$control$conf_level

  result
}
