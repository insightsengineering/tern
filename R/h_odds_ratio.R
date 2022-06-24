#' Helper Functions for Odds Ratio Estimation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions to calculate odds ratios in [estimate_odds_ratio()]
#'
#' @inheritParams odds_ratio
#'
#' @name h_odds_ratio
NULL

#' @describeIn h_odds_ratio estimates the odds ratio based on [stats::glm()]. Note that there must be
#'   exactly 2 groups in `data` as specified by the `grp` variable.
#'
#' @inheritParams argument_convention
#'
#' @examples
#' # Data with 2 groups.
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
#'   strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' # Odds ratio based on glm.
#' or_glm(data, conf_level = 0.95)
#'
#' @export
or_glm <- function(data, conf_level) {
  assertthat::assert_that(
    is.logical(data$rsp),
    is_proportion(conf_level)
  )
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp"))
  assert_character_or_factor(data$grp)

  data$grp <- as_factor_keep_attributes(data$grp)
  assertthat::assert_that(
    is_df_with_nlevels_factor(data, variable = "grp", n_levels = 2)
  )
  formula <- stats::as.formula("rsp ~ grp")
  model_fit <- stats::glm(
    formula = formula, data = data,
    family = stats::binomial(link = "logit")
  )

  # Note that here we need to discard the intercept.
  or <- exp(stats::coef(model_fit)[-1])
  or_ci <- exp(
    stats::confint.default(model_fit, level = conf_level)[-1, , drop = FALSE]
  )

  values <- stats::setNames(c(or, or_ci), c("est", "lcl", "ucl"))
  n_tot <- stats::setNames(nrow(model_fit$model), "n_tot")

  list(or_ci = values, n_tot = n_tot)
}

#' @describeIn h_odds_ratio estimates the odds ratio based on [survival::clogit()]. This is done for
#'   the whole data set including all groups, since the results are not the same as when doing
#'   pairwise comparisons between the groups.
#'
#' @inheritParams argument_convention
#'
#' @examples
#' # Data with 3 groups.
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
#'   strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' # Odds ratio based on stratified estimation by conditional logistic regression.
#' or_clogit(data, conf_level = 0.95)
#'
#' @export
or_clogit <- function(data, conf_level) {
  assertthat::assert_that(
    is.logical(data$rsp),
    is_proportion(conf_level)
  )
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp", strata = "strata"))
  assert_character_or_factor(data$grp)
  assert_character_or_factor(data$strata)

  data$grp <- as_factor_keep_attributes(data$grp)
  data$strata <- as_factor_keep_attributes(data$strata)

  # Deviation from convention: `survival::strata` must be simply `strata`.
  formula <- stats::as.formula("rsp ~ grp + strata(strata)")
  model_fit <- clogit_with_tryCatch(formula = formula, data = data)

  # Create a list with one set of OR estimates and CI per coefficient, i.e.
  # comparison of one group vs. the reference group.
  coef_est <- stats::coef(model_fit)
  ci_est <- stats::confint(model_fit, level = conf_level)
  or_ci <- list()
  for (coef_name in names(coef_est)) {
    grp_name <- gsub("^grp", "", x = coef_name)
    or_ci[[grp_name]] <- stats::setNames(
      object = exp(c(coef_est[coef_name], ci_est[coef_name, , drop = TRUE])),
      nm = c("est", "lcl", "ucl")
    )
  }
  list(or_ci = or_ci, n_tot = c(n_tot = model_fit$n))
}
