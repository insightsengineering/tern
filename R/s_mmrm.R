
#' Helper function to check the MMRM variables
#'
#' @param vars variable list as per \code{\link{s_mmrm}}
#' @param data data frame that includes the variables
#'
#' @return nothing, this function will stop in case of problems, and pass when everything is ok.
check_mmrm_vars <- function(vars,
                            data) {
  stopifnot(is.list(vars))
  stopifnot(is.data.frame(data))

  # Check whether variables are specified and in data.
  is_specified <- function(var) {
    !is.null(vars[[var]])
  }
  is_specified_and_in_data <- function(var){
    is_specified(var) && all(vars[[var]] %in% names(data))
  }
  stopifnot(is_specified_and_in_data("response"))
  stopifnot(is_specified_and_in_data("id"))
  stopifnot(is_specified_and_in_data("arm"))
  stopifnot(is_specified_and_in_data("visit"))
  if (is_specified("covariates")) {
    vars$parts <- unique(unlist(strsplit(vars$covariates, split = "\\*|:")))
    stopifnot(is_specified_and_in_data("parts"))
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
}

#' Helper function to build the MMRM formula
#'
#' @param vars variable list as per \code{\link{s_mmrm}}
#' @param cor_struct correlation structure as per \code{\link{s_mmrm}}
#'
#' @return the complete MMRM formula to use with \code{lmer}
build_mmrm_formula <- function(vars,
                               cor_struct) {
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

#' Helper function to fit the MMRM with lme4 and lmerTest
#'
#' @param formula the MMRM formula (it could also be another lme4 formula)
#' @param data the data frame
#'
#' @return the \code{lmerModLmerTest} object
#'
#' @importFrom lmerTest lmer
#' @importFrom lme4 lmerControl
fit_lme4 <- function(formula,
                     data) {
  warning_message <- NULL
  withCallingHandlers(
    fit <- lmerTest::lmer(
      formula = formula,
      REML = TRUE,
      data = data,
      control = lme4::lmerControl(
        optimizer = "nlminbwrap",
        check.nobs.vs.nRE = "ignore"
      )
    ),
    # If a warning occurs, save this outside and process below.
    warning = function(w) {
      warning_message <<- w
      invokeRestart("muffleWarning")
    }
  )

  # Check that the model converged without warnings.
  conv_info <- fit@optinfo$conv$lme4
  has_some_conv_info <- length(conv_info) > 0 && !identical(conv_info$code, 0L)
  has_warning <- !is.null(warning_message)

  # If there was some problems, check additional gradient criterion.
  if (has_warning || has_some_conv_info) {
    hessian <- fit@optinfo$derivs$Hessian
    gradient <- fit@optinfo$derivs$gradient
    rel_grad <- solve(hessian, gradient)
    if (max(abs(rel_grad)) > 1e-4) {
      error_message <- paste(
        c(warning_message, conv_info$messages),
        collapse = "; "
      )
      stop(error_message)
    }
  }

  return(fit)
}

#' Helper function to extract the LS means from an MMRM fit.
#'
#' @param fit result of \code{\link{fit_lme4}}
#' @param vars variable list as per \code{\link{s_mmrm}} input
#' @param conf_level confidence level
#' @param weights string specifying the type of weights to be used for the LS means,
#'   see \code{\link[emmeans]{emmeans}} for details.
#'
#' @return A list with the LS means `estimate` and `contrast` results between the treatment
#'   and control arm groups at the different visits.
#'
#' @importFrom emmeans emmeans contrast
#' @importFrom dplyr filter group_by_at left_join mutate n summarise rename ungroup
#' @importFrom rlang := !!
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
  estimate <- confint(lsmeans, level = conf_level) %>%
    as.data.frame()

  data_n <- data_complete %>%
    dplyr::group_by_at(.vars = c(vars$visit, vars$arm)) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup()

  estimate <- estimate %>%
    dplyr::left_join(data_n, by = c(vars$visit, vars$arm))

  # Get LS means for reference group to join into full dataframe so that relative reduction in
  # LS mean (mean of response variable) can be computed with respect to reference level (e.g. ARM A).
  arm_levels <- levels(data_complete[[vars$arm]])
  reference_level <- arm_levels[1]
  means_at_ref <- estimate %>%
    dplyr::filter(!!as.symbol(vars$arm) == reference_level) %>%
    dplyr::select(c(vars$visit, "emmean")) %>%
    dplyr::rename(ref = .data$emmean)

  relative_reduc <- estimate %>%
    dplyr::filter(!!as.symbol(vars$arm) != reference_level) %>%
    dplyr::left_join(means_at_ref, by = c(vars$visit)) %>%
    dplyr::mutate(relative_reduc = (.data$ref - .data$emmean) / .data$ref) %>%
    dplyr::select(c(vars$visit, vars$arm, "relative_reduc"))

  # Start with the differences between LS means.
  sum_fit_diff <- summary(
    emmeans::contrast(lsmeans, method = "trt.vs.ctrl"),
    level = conf_level,
    infer = c(TRUE, TRUE),
    adjust = "none"
  )

  # Get the comparison group name from "contrast" column,
  # e.g. "ARMB - ARMA" shall return "ARMB", i.e. remove " - ARMA" part.
  contrast <- sum_fit_diff %>%
    dplyr::mutate(
      col_by = factor(
        gsub(paste0("\\s-\\s", reference_level), "", contrast),
        levels = arm_levels
      )
    ) %>%
    dplyr::select(-contrast) %>%
    dplyr::rename(!!as.symbol(vars$arm) := .data$col_by) %>%
    dplyr::left_join(relative_reduc, by = c(vars$visit, vars$arm)) %>%
    dplyr::mutate(!!as.symbol(vars$arm) := droplevels(!!as.symbol(vars$arm), exclude = reference_level))

  results <- list(
    estimate = estimate,
    contrast = contrast
  )
  return(results)
}

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
#'
#' @details
#'   Only Satterthwaite adjusted degrees of freedom (d.f.) are supported, because they
#'   match the results obtained in SAS (so far confirmed for unstructured covariance matrix).
#'   For the correlation structure (\code{cor_struct}), the user can choose among the following options, sorted
#'   in descending number of variance parameters:
#'   \describe{
#'   \item{unstructured}{Unstructured covariance matrix. This is the most flexible choice and default.
#'      If there are \code{T} visits, then \code{T * (T-1) / 2} variance parameters are used.}
#'   \item{random-quadratic}{Random quadratic spline for the random effects of the time variable.
#'      6 variance parameters are used.}
#'   \item{random-slope}{Random slope for the random effects of the time variable. 3 variance parameters are used.}
#'   \item{compound-symmetry}{Constant correlation between visits. 2 variance parameters are used.}
#'   }
#'
#' @return A list with MMRM results:
#' \describe{
#'   \item{fit}{The \code{lmerModLmerTest} object which was fitted to the data.}
#'   \item{lsmeans}{This is a list with data frames \code{estimate} and \code{contrast}.}
#'   \item{ref_level}{The reference level for the arm variable, which is always the first level.}
#'   \item{conf_level}{The confidence level which was used to construct the confidence intervals.}
#' }
#'
#' @export
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
#'
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
#'   weights_emmeans = "equal"
#' )
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
  cor_struct = c(
    "unstructured",
    "random-quadratic",
    "random-slope",
    "compound-symmetry"
  ),
  weights_emmeans = "proportional"
) {

  check_mmrm_vars(vars, data)
  check_conf_level(conf_level)
  cor_struct <- match.arg(cor_struct)

  formula <- build_mmrm_formula(vars, cor_struct)
  fit <- fit_lme4(formula, data)
  lsmeans <- get_mmrm_lsmeans(
    fit = fit,
    vars = vars,
    conf_level = conf_level,
    weights = weights_emmeans
  )

  results <- list(
    fit = fit,
    lsmeans = lsmeans,
    ref_level = levels(data[[vars$arm]])[1],
    conf_level = conf_level
  )
  return(results)
}
