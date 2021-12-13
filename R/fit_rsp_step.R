#' Subgroup Treatment Effect Pattern (STEP) Fit for Binary (Response) Outcome
#'
#' This fits the Subgroup Treatment Effect Pattern logistic regression models for a binary
#' (response) outcome. The treatment arm variable must have exactly 2 levels,
#' where the first one is taken as reference and the estimated odds ratios are
#' for the comparison of the second level vs. the first one.
#'
#' The (conditional) logistic regression model which is fit is:\cr
#' `response ~ arm * poly(biomarker, degree) + covariates + strata(strata)`\cr
#' where `degree` is specified by `control_step()`.
#' Note that for the default degree 0 the `biomarker` variable is not included in the model.
#'
#' @inheritParams argument_convention
#' @param variables (named `list` of `character`)\cr list of analysis variables:
#'   needs `response`, `arm`, `biomarker`, and optional `covariates` and
#'   `strata`.
#' @param control (named `list`)\cr combined control list from [control_step()]
#'   and [control_logistic()].
#' @return a matrix of class `step`. The first part of the columns describe the
#'   subgroup intervals used for the biomarker variable, including where the
#'   center of the intervals are and their bounds. The second part of the
#'   columns contain the estimates for the treatment arm comparison.
#' @seealso [control_step()] and [control_logistic()] for the available
#'   customization options.
#' @export
#' @examples
#'
#' # Testing dataset with just two treatment arms.
#' library(survival)
#' library(dplyr)
#' library(scda)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#' adrs_f <- adrs %>%
#'   filter(
#'     PARAMCD == "BESRSPI",
#'     ARM %in% c("B: Placebo", "A: Drug X")
#'   ) %>%
#'   mutate(
#'     # Reorder levels of ARM to have Placebo as reference arm for Odds Ratio calculations.
#'     ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
#'     RSP = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     SEX = factor(SEX)
#'   )
#'
#' variables <- list(
#'   arm = "ARM",
#'   biomarker = "BMRKR1",
#'   covariates = "AGE",
#'   response = "RSP"
#' )
#'
#' # Fit default STEP models: Here a constant treatment effect is estimated in each subgroup.
#' # We use a large enough bandwidth to avoid too small subgroups and linear separation in those.
#' step_matrix <- fit_rsp_step(
#'   variables = variables,
#'   data = adrs_f,
#'   control = c(control_logistic(), control_step(bandwidth = 0.5))
#' )
#' dim(step_matrix)
#' head(step_matrix)
#'
#' # Specify different polynomial degree for the biomarker interaction to use more flexible local
#' # models. Or specify different logistic regression options, including confidence level.
#' step_matrix2 <- fit_rsp_step(
#'   variables = variables,
#'   data = adrs_f,
#'   control = c(control_logistic(conf_level = 0.9), control_step(bandwidth = 0.6, degree = 1))
#' )
#'
#' # Use a global constant model. This is helpful as a reference for the subgroup models.
#' step_matrix3 <- fit_rsp_step(
#'   variables = variables,
#'   data = adrs_f,
#'   control = c(control_logistic(), control_step(bandwidth = NULL, num_points = 2L))
#' )
#'
#' # It is also possible to use strata, i.e. use conditional logistic regression models.
#' variables2 <- list(
#'   arm = "ARM",
#'   biomarker = "BMRKR1",
#'   covariates = "AGE",
#'   response = "RSP",
#'   strata = c("STRATA1", "STRATA2")
#' )
#'
#' step_matrix4 <- fit_rsp_step(
#'   variables = variables2,
#'   data = adrs_f,
#'   control = c(control_logistic(), control_step(bandwidth = 0.6))
#' )
#'
fit_rsp_step <- function(variables,
                         data,
                         control = c(control_step(), control_logistic())) {
  assertthat::assert_that(
    is_df_with_variables(data, variables),
    utils.nest::is_fully_named_list(control)
  )
  data <- data[!is.na(data[[variables$biomarker]]), ]
  window_sel <- h_step_window(x = data[[variables$biomarker]], control = control)
  interval_center <- window_sel$interval[, "Interval Center"]
  form <- h_step_rsp_formula(variables = variables, control = control)
  estimates <- if (is.null(control$bandwidth)) {
    h_step_rsp_est(
      formula = form,
      data = data,
      variables = variables,
      x = interval_center,
      control = control
    )
  } else {
    tmp <- mapply(
      FUN = h_step_rsp_est,
      x = interval_center,
      subset = as.list(as.data.frame(window_sel$sel)),
      MoreArgs = list(
        formula = form,
        data = data,
        variables = variables,
        control = control
      )
    )
    # Maybe we find a more elegant solution than this.
    rownames(tmp) <- c("n", "logor", "se", "ci_lower", "ci_upper")
    t(tmp)
  }
  result <- cbind(window_sel$interval, estimates)
  structure(
    result,
    class = c("step", "matrix"),
    variables = variables,
    control = control
  )
}
