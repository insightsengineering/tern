#' Tabulation of MMRM Results
#'
#' These functions can be used to produce tables from a fitted MMRM produced with
#' [fit_mmrm()].
#'
#' @name tabulate_mmrm
#'
NULL

#' @describeIn tabulate_mmrm Produce simple MMRM tables via the generic [as.rtable()].
#'
#' @param x (`mmrm`)\cr the original MMRM fit object.
#' @param type (`string`)\cr type of table which should be returned.
#' @param ... additional argument `format` for controlling the numeric format.
#'
#' @return [as.rtable.mmrm()] returns the fixed effects, covariance estimate or
#'   diagnostic statistics tables.
#' @export
#' @method as.rtable mmrm
#'
#' @examples
#' library(dplyr)
#' library(lme4)
#'
#' dat <- sleepstudy %>%
#'   mutate(
#'     group = factor(rep(c("A", "B"), length = nrow(sleepstudy))),
#'     days_grouped = cut(
#'       Days,
#'       breaks = quantile(Days, probs = seq(0, 1, length = 5)),
#'       include.lowest = TRUE
#'     )
#'   )
#' result <- fit_mmrm(
#'   vars = list(
#'     response = "Reaction",
#'     covariates = c(),
#'     id = "Subject",
#'     arm = "group",
#'     visit = "days_grouped"
#'   ),
#'   data = dat,
#'   cor_struct = "compound-symmetry",
#'   parallel = TRUE
#' )
#' as.rtable(result, type = "cov", format = "xx.x")
#'
as.rtable.mmrm <- function(x,  #nolint
                           type = c("fixed", "cov", "diagnostic"),
                           ...) {
  type <- match.arg(type)
  switch(
    type,
    fixed = h_mmrm_fixed(x, ...),
    cov = h_mmrm_cov(x, ...),
    diagnostic = h_mmrm_diagnostic(x, ...)
  )
}

#' @describeIn tabulate_mmrm Helper function to produce fixed effects table.
#' @param format (`string`)\cr format for the numbers in the table.
#' @importFrom stats coef
#' @export
#'
h_mmrm_fixed <- function(x, format = "xx.xxxx") {
  fixed_table <- as.data.frame(coef(summary(x$fit)))
  pvalue_column <- match("Pr(>|t|)", names(fixed_table))
  df_column <- match("df", names(fixed_table))
  pvalue_table <- as.rtable(fixed_table[, pvalue_column, drop = FALSE], format = "x.xxxx | (<0.0001)")
  df_table <- as.rtable(fixed_table[, df_column, drop = FALSE], format = "xx.")
  remaining_table <- as.rtable(fixed_table[, - c(df_column, pvalue_column), drop = FALSE], format = format)
  cbind_rtables(remaining_table, df_table, pvalue_table)
}

#' @describeIn tabulate_mmrm Helper function to produce covariance matrix table.
#' @export
#'
h_mmrm_cov <- function(x, format = "xx.xxxx") {
  cov_estimate <- x$cov_estimate
  as.rtable(as.data.frame(cov_estimate), format = format)
}

#' @describeIn tabulate_mmrm Helper function to produce diagnostic statistics table.
#' @export
#'
h_mmrm_diagnostic <- function(x, format = "xx.xxxx") {
  sfun <- function(df, ...) {
    as.list(df[c("REML.criterion", "AIC", "AICc", "BIC")])
  }
  afun <- make_afun(
    sfun,
    .labels = c(REML.criterion = "REML criterion")
  )
  diag_values <- x$diagnostics
  df <- as.data.frame(diag_values)
  lyt <- basic_table()
  lyt <- add_overall_col(lyt, label = "Diagnostic statistic value")
  lyt <- analyze(lyt, vars = "AIC", afun = afun, format = format)
  build_table(lyt, df)
}

#' @describeIn tabulate_mmrm Helper method (for [broom::tidy()]) to prepare a data frame from an
#'   `mmrm` object containing the LS means and contrasts.
#' @method tidy mmrm
#' @export
#' @importFrom broom tidy
#' @importFrom tibble as_tibble
#' @examples
#' library(broom)
#' df <- tidy(result)
#'
tidy.mmrm <- function(x) {  #nolint #nousage
  contrasts <- x$lsmeans$contrasts
  estimates <- x$lsmeans$estimates
  vars <- x$vars
  contrasts[[vars$arm]] <- factor(contrasts[[vars$arm]], levels = levels(estimates[[vars$arm]]))
  df <- merge(
    x = estimates,
    y = contrasts,
    by = c(vars$arm, vars$visit),
    suffixes = c("_est", "_contr"),
    all = TRUE
  )
  df$conf_level <- x$conf_level
  as_tibble(df)
}

#' @describeIn tabulate_mmrm Statistics function which is extracting estimates from a tidied LS means
#'   data frame.
#' @inheritParams argument_convention
#' @param show_relative should the "reduction" (`control - treatment`, default) or the "increase"
#'   (`treatment - control`) be shown for the relative change from baseline?
#' @export
#' @examples
#' s_mmrm_lsmeans(df[8, ], .in_ref_col = FALSE)
#'
s_mmrm_lsmeans <- function(df, .in_ref_col, show_relative = c("reduction", "increase")) {
  show_relative <- match.arg(show_relative)
  if_not_ref <- function(x) `if`(.in_ref_col, character(), x)
  list(
    n = df$n,
    adj_mean_se = c(df$estimate_est, df$se_est),
    adj_mean_ci = with_label(c(df$lower_cl_est, df$upper_cl_est), f_conf_level(df$conf_level)),
    diff_mean_se = if_not_ref(c(df$estimate_contr, df$se_contr)),
    diff_mean_ci = with_label(if_not_ref(c(df$lower_cl_contr, df$upper_cl_contr)), f_conf_level(df$conf_level)),
    change = switch(
      show_relative,
      reduction = with_label(if_not_ref(df$relative_reduc), "Relative Reduction (%)"),
      increase = with_label(if_not_ref(- df$relative_reduc), "Relative Increase (%)")
    ),
    p_value = if_not_ref(df$p_value)
  )
}

#' @describeIn tabulate_mmrm Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
a_mmrm_lsmeans <- make_afun(
  s_mmrm_lsmeans,
  .labels = c(
    adj_mean_se = "Adjusted Mean (SE)",
    diff_mean_se = "Difference in Adjusted Means (SE)",
    p_value = "p-value (MMRM)"
  ),
  .formats = c(
    n = "xx.",
    adj_mean_se = sprintf_format("%.3f (%.3f)"),
    adj_mean_ci = "(xx.xxx, xx.xxx)",
    diff_mean_se = sprintf_format("%.3f (%.3f)"),
    diff_mean_ci = "(xx.xxx, xx.xxx)",
    change = "xx.x%",
    p_value = "x.xxxx | (<0.0001)"
  ),
  .indent_mods = c(
    adj_mean_ci = 1L,
    diff_mean_ci = 1L,
    change = 1L,
    p_value = 1L
  ),
  .null_ref_cells = FALSE
)

#' @describeIn tabulate_mmrm Analyze function for tabulating LS means estimates from tidied `mmrm` results.
#' @inheritParams argument_convention
#' @export
#' @examples
#'
#' dat_adsl <- dat %>%
#'   select(Subject, group) %>%
#'   unique
#' basic_table() %>%
#'   split_cols_by("group", ref_group = result$ref_level) %>%
#'   add_colcounts() %>%
#'   split_rows_by("days_grouped") %>%
#'   summarize_lsmeans(show_relative = "increase") %>%
#'   build_table(
#'     df = tidy(result),
#'     alt_counts_df = dat_adsl
#'   )
#'
summarize_lsmeans <- function(lyt,
                              ...,
                              table_names = "lsmeans_summary",
                              .stats = NULL,
                              .formats = NULL,
                              .indent_mods = NULL,
                              .labels = NULL) {
  afun <- make_afun(
    a_mmrm_lsmeans,
    .stats = .stats,
    .formats = .formats,
    .indent_mods = .indent_mods,
    .labels = .labels
  )
  analyze(
    lyt = lyt,
    vars = "n",
    afun = afun,
    table_names = table_names,
    extra_args = list(...)
  )
}
