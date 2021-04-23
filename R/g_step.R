#' Custom Tidy Method for STEP Results
#'
#' Tidy the STEP results into a `tibble` to format them ready for plotting.
#'
#' @param x (`step` matrix)\cr results from [fit_survival_step()].
#' @param ... not used here.
#' @return A `tibble` with one row per STEP subgroup. The estimates and CIs are on the HR or OR scale,
#'   respectively. Additional attributes carry meta data also used for plotting.
#' @seealso [g_step()] which consumes the result from this function.
#' @importFrom tibble as_tibble
#' @method tidy step
#' @export
#' @examples
#' library(survival)
#' lung$sex <- factor(lung$sex)
#' vars <- list(
#'   time = "time",
#'   event = "status",
#'   arm = "sex",
#'   biomarker = "age"
#' )
#' step_matrix <- fit_survival_step(
#'   variables = vars,
#'   data = lung,
#'   control = c(control_coxph(), control_step(num_points = 10, degree = 2))
#' )
#' broom::tidy(step_matrix)
#'
tidy.step <- function(x, ...) {  #nolint #nousage
  assert_that(is(x, "step"))
  dat <- as.data.frame(x)
  nams <- names(dat)
  is_surv <- "loghr" %in% names(dat)
  est_var <- ifelse(is_surv, "loghr", "logor")
  new_est_var <- ifelse(is_surv, "Hazard Ratio", "Odds Ratio")
  new_y_vars <- c(new_est_var, c("ci_lower", "ci_upper"))
  names(dat)[match(est_var, nams)] <- new_est_var
  dat[, new_y_vars] <- exp(dat[, new_y_vars])
  structure(
    tibble::as_tibble(dat),
    estimate = new_est_var,
    biomarker = attr(x, "variables")$biomarker,
    ci = f_conf_level(attr(x, "control")$conf_level)
  )
}

#' Create a STEP Graph
#'
#' Based on the STEP results, creates a `ggplot` graph showing the estimated HR or OR
#' along the continuous biomarker value subgroups.
#'
#' @param df (`tibble`)\cr result of [tidy.step()].
#' @param use_percentile (`flag`)\cr whether to use percentiles for the x axis or actual
#'   biomarker values.
#' @param est (named `list`)\cr `col` and `lty` settings for estimate line.
#' @param ci_ribbon (named `list` or `NULL`)\cr `fill` and `alpha` settings for the confidence interval
#'   ribbon area, or `NULL` to not plot a CI ribbon.
#'
#' @return The `ggplot2` object.
#' @importFrom tibble is_tibble
#' @importFrom scales percent
#' @export
#'
#' @examples
#' library(survival)
#' lung$sex <- factor(lung$sex)
#'
#' # Survival example.
#' vars <- list(
#'   time = "time",
#'   event = "status",
#'   arm = "sex",
#'   biomarker = "age"
#' )
#'
#' step_matrix <- fit_survival_step(
#'   variables = vars,
#'   data = lung,
#'   control = c(control_coxph(), control_step(num_points = 10, degree = 2))
#' )
#' step_data <- broom::tidy(step_matrix)
#'
#' # Default plot.
#' g_step(step_data)
#'
#' # Add the reference 1 horizontal line.
#' library(ggplot2)
#' g_step(step_data) +
#'   geom_hline(aes(yintercept = 1), linetype = 2)
#'
#' # Use actual values instead of percentiles, different color for estimate and no CI,
#' # use log scale for y axis.
#' g_step(
#'   step_data,
#'   use_percentile = FALSE,
#'   est = list(col = "blue", lty = 1),
#'   ci_ribbon = NULL
#' ) + scale_y_log10()
#'
#' # Adding another curve based on additional column.
#' step_data$extra <- exp(step_data$`Percentile Center`)
#' g_step(step_data) +
#'   geom_line(aes(y = extra), linetype = 2, color = "green")
#'
#' # Response example.
#' vars <- list(
#'   response = "status",
#'   arm = "sex",
#'   biomarker = "age"
#' )
#'
#' step_matrix <- fit_rsp_step(
#'   variables = vars,
#'   data = lung,
#'   control = c(
#'     control_logistic(response_definition = "I(response == 2)"),
#'     control_step()
#'   )
#' )
#' step_data <- broom::tidy(step_matrix)
#' g_step(step_data)
#'
g_step <- function(df,
                   use_percentile = "Percentile Center" %in% names(df),
                   est = list(col = "black", lty = 1),
                   ci_ribbon = list(fill = "lightblue", alpha = 0.5)) {
  assert_that(
    tibble::is_tibble(df),
    is.flag(use_percentile),
    is_fully_named_list(est),
    is.null(ci_ribbon) || is_fully_named_list(ci_ribbon)
  )
  x_var <- ifelse(use_percentile, "Percentile Center", "Interval Center")
  df$x <- df[[x_var]]
  attrs <- attributes(df)
  df$y <- df[[attrs$estimate]]
  p <- ggplot(df, aes_string(x = "x"))
  if (!is.null(ci_ribbon)) {
    p <- p + geom_ribbon(
      aes_string(ymin = "ci_lower", ymax = "ci_upper"),
      fill = ci_ribbon$fill,
      alpha = ci_ribbon$alpha
    )
  }
  p <- p + geom_line(
    aes_string(y = "y"),
    color = est$col,
    linetype = est$lty
  )
  p <- p + labs(x = attrs$biomarker, y = attrs$estimate)
  if (use_percentile) {
    p <- p + scale_x_continuous(labels = scales::percent)
  }
  p
}
