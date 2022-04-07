#' Tabulation of MMRM Results
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' These functions can be used to produce tables from a fitted MMRM produced with
#' [fit_mmrm()].
#'
#' @name tabulate_mmrm
#'
NULL

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Produce simple MMRM tables via the generic [as.rtable()].
#'
#' @param x (`mmrm`)\cr the original MMRM fit object.
#' @param type (`string`)\cr type of table which should be returned.
#' @param arms (`flag`)\cr  should treatment variable be considered when using
#' `summarize_lsmeans` layout generating function.
#' @param ... additional argument `format` for controlling the numeric format.
#' @return [as.rtable.mmrm()] returns the fixed effects, covariance estimate or
#'   diagnostic statistics tables.
#' @export
#' @method as.rtable mmrm
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(lme4)
#'
#' dat <- sleepstudy %>%
#'   mutate(
#'     group = factor(rep(c("A", "B"), length = nrow(sleepstudy))),
#'     days_grouped = cut(
#'       Days,
#'       breaks = stats::quantile(Days, probs = seq(0, 1, length = 5)),
#'       include.lowest = TRUE
#'     ),
#'     Subject = case_when(
#'       group == "A" ~ as.character(Subject),
#'       TRUE ~ as.character(as.numeric(as.character(Subject)) + 50)
#'     )
#'   ) %>%
#'   distinct_at(.vars = c("Subject", "days_grouped", "group"), .keep_all = TRUE)
#'
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
#' result_no_arm <- fit_mmrm(
#'   vars = list(
#'     response = "Reaction",
#'     covariates = c(),
#'     id = "Subject",
#'     visit = "days_grouped"
#'   ),
#'   data = dat,
#'   cor_struct = "compound-symmetry",
#'   parallel = TRUE
#' )
#' as.rtable(result_no_arm, type = "cov", format = "xx.x")
#' }
as.rtable.mmrm <- function(x, # nolint
                           type = c("fixed", "cov", "diagnostic"),
                           ...) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::as.rtable.mmrm()",
    with = "tern.mmrm::as.rtable.mmrm()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Helper function to produce fixed effects table.
#' @param format (`string`)\cr format for the numbers in the table.
#' @export
#'
h_mmrm_fixed <- function(x, format = "xx.xxxx") {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::h_mmrm_fixed()",
    with = "tern.mmrm::h_mmrm_fixed()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Helper function to produce covariance matrix table.
#' @export
#'
h_mmrm_cov <- function(x, format = "xx.xxxx") {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::h_mmrm_fixed()",
    with = "tern.mmrm::h_mmrm_fixed()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Helper function to produce diagnostic statistics table.
#' @export
#'
h_mmrm_diagnostic <- function(x, format = "xx.xxxx") {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::h_mmrm_diagnostic()",
    with = "tern.mmrm::h_mmrm_diagnostic()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Helper method (for [broom::tidy()]) to prepare a data frame from an
#'   `mmrm` object containing the LS means and contrasts.
#' @method tidy mmrm
#' @export
#' @examples
#' \dontrun{
#' library(broom)
#' df <- tidy(result)
#' df_no_arm <- tidy(result_no_arm)
#' }
tidy.mmrm <- function(x) { # nolint
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::tidy.mmrm()",
    with = "tern.mmrm::tidy.mmrm()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Statistics function which is extracting estimates from a tidied LS means
#'   data frame.
#' @inheritParams argument_convention
#' @param show_relative should the "reduction" (`control - treatment`, default) or the "increase"
#'   (`treatment - control`) be shown for the relative change from baseline?
#' @export
#' @examples
#' \dontrun{
#' s_mmrm_lsmeans(df[8, ], .in_ref_col = FALSE)
#' }
s_mmrm_lsmeans <- function(df, .in_ref_col, show_relative = c("reduction", "increase")) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::s_mmrm_lsmeans()",
    with = "tern.mmrm::s_mmrm_lsmeans()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Statistics function which is extracting estimates from a tidied LS means
#' data frame when `ARM` is not considered in the model.
#' @inheritParams argument_convention
#' @export
#' @examples
#' \dontrun{
#' s_mmrm_lsmeans_single(df_no_arm[4, ])
#' }
s_mmrm_lsmeans_single <- function(df) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::s_mmrm_lsmeans_single()",
    with = "tern.mmrm::s_mmrm_lsmeans_single()"
  )
}

#' @describeIn tabulate_mmrm Deprecated by `tern.mmrm::tabulate_mmrm`: Analyze function for tabulating LS means estimates from tidied `mmrm` results.
#' @inheritParams argument_convention
#' @export
#' @examples
#' \dontrun{
#' dat_adsl <- dat %>%
#'   select(Subject, group) %>%
#'   unique()
#'
#' basic_table() %>%
#'   split_cols_by("group", ref_group = result$ref_level) %>%
#'   add_colcounts() %>%
#'   split_rows_by("days_grouped") %>%
#'   summarize_lsmeans(show_relative = "increase") %>%
#'   build_table(
#'     df = broom::tidy(result),
#'     alt_counts_df = dat_adsl
#'   )
#'
#' basic_table() %>%
#'   split_rows_by("days_grouped") %>%
#'   summarize_lsmeans(arms = FALSE) %>%
#'   build_table(
#'     df = broom::tidy(result_no_arm),
#'     alt_counts_df = dat_adsl
#'   )
#' }
summarize_lsmeans <- function(lyt,
                              arms = TRUE,
                              ...,
                              table_names = "lsmeans_summary",
                              .stats = NULL,
                              .formats = NULL,
                              .indent_mods = NULL,
                              .labels = NULL) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::summarize_lsmeans()",
    with = "tern.mmrm::summarize_lsmeans()"
  )
}
