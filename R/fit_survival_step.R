#' Subgroup Treatment Effect Pattern (STEP) Fit for Survival Outcome
#'
#' This fits the Subgroup Treatment Effect Pattern models for a survival outcome. The treatment arm
#' variable must have exactly 2 levels, where the first one is taken as reference and the estimated
#' hazard ratios are for the comparison of the second level vs. the first one.
#'
#' The model which is fit is:\cr
#' `Surv(time, event) ~ arm * poly(biomarker, degree) + covariates + strata(strata)`\cr
#' where `degree` is specified by `control_step()`.
#' Note that for the default degree 0 the `biomarker` variable is not included in the model.
#'
#' @inheritParams argument_convention
#' @param variables (named `list` of `character`)\cr list of analysis variables: needs `time`, `event`,
#'   `arm`, `biomarker`, and optional `covariates` and `strata`.
#' @param control (named `list`)\cr combined control list from [control_step()] and [control_coxph()].
#' @return a matrix of class `step`. The first part of the columns describe the subgroup intervals used
#'   for the biomarker variable, including where the center of the intervals are and their bounds. The
#'   second part of the columns contain the estimates for the treatment arm comparison.
#' @seealso [control_step()] and [control_coxph()] for the available customization options.
#' @export
#' @examples
#'
#' # Testing dataset with just two treatment arms.
#' library(scda)
#' library(dplyr)
#'
#' adtte <- synthetic_cdisc_data("latest")$adtte
#'
#' adtte_f <- adtte %>%
#'   filter(
#'     PARAMCD == "OS",
#'     ARM %in% c("B: Placebo", "A: Drug X")
#'   ) %>%
#'   mutate(
#'     # Reorder levels of ARM to display reference arm before treatment arm.
#'     ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
#'     is_event = CNSR == 0
#'   )
#' columns <- c("ARM", "is_event")
#' labels <- c("Treatment Arm", "Event Flag")
#' for (i in seq_along(columns)) {
#'   attr(adtte_f[[columns[i]]], "label") <- labels[i]
#' }
#'
#' variables <- list(
#'   arm = "ARM",
#'   biomarker = "BMRKR1",
#'   covariates = c("AGE", "BMRKR2"),
#'   event = "is_event",
#'   time = "AVAL"
#' )
#'
#' # Fit default STEP models: Here a constant treatment effect is estimated in each subgroup.
#' step_matrix <- fit_survival_step(
#'   variables = variables,
#'   data = adtte_f
#' )
#' dim(step_matrix)
#' head(step_matrix)
#'
#' # Specify different polynomial degree for the biomarker interaction to use more flexible local
#' # models. Or specify different Cox regression options.
#' step_matrix2 <- fit_survival_step(
#'   variables = variables,
#'   data = adtte_f,
#'   control = c(control_coxph(conf_level = 0.9), control_step(degree = 2))
#' )
#'
#' # Use a global model with cubic interaction and only 5 points.
#' step_matrix3 <- fit_survival_step(
#'   variables = variables,
#'   data = adtte_f,
#'   control = c(control_coxph(), control_step(bandwidth = NULL, degree = 3, num_points = 5L))
#' )
fit_survival_step <- function(variables,
                              data,
                              control = c(control_step(), control_coxph())) {
  assertthat::assert_that(
    is_df_with_variables(data, variables),
    is.list(control)
  )
  data <- data[!is.na(data[[variables$biomarker]]), ]
  window_sel <- h_step_window(x = data[[variables$biomarker]], control = control)
  interval_center <- window_sel$interval[, "Interval Center"]
  form <- h_step_survival_formula(variables = variables, control = control)
  estimates <- if (is.null(control$bandwidth)) {
    h_step_survival_est(
      formula = form,
      data = data,
      variables = variables,
      x = interval_center,
      control = control
    )
  } else {
    tmp <- mapply(
      FUN = h_step_survival_est,
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
    rownames(tmp) <- c("n", "events", "loghr", "se", "ci_lower", "ci_upper")
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
