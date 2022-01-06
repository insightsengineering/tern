#' Tabulation of RBMI Results
#'
#' These functions can be used to produce tables from a fitted RBMI produced
#'
#' @name tabulate_rbmi
#'
NULL

#' @describeIn tabulate_rbmi Helper function to produce data frame with results of rbmi::pool
#'   for a single visit
#' @param x (`list`)
#' @export
#'
h_tidy_pool <- function(x) {
  contr <- x[[grep("trt_", names(x))]]
  ref <- x[[grep("lsm_ref_", names(x))]]
  alt <- x[[grep("lsm_alt_", names(x))]]

  df_ref <- data.frame(
    group = "ref",
    est = ref$est,
    se_est = ref$se,
    lower_cl_est = ref$ci[1],
    upper_cl_est = ref$ci[2],
    est_contr = NA_real_,
    se_contr = NA_real_,
    lower_cl_contr = NA_real_,
    upper_cl_contr = NA_real_,
    p_value = NA_real_,
    relative_reduc = NA_real_,
    stringsAsFactors = FALSE
  )

  df_alt <- data.frame(
    group = "alt",
    est = alt$est,
    se_est = alt$se,
    lower_cl_est = alt$ci[1],
    upper_cl_est = alt$ci[2],
    est_contr = contr$est,
    se_contr = contr$se,
    lower_cl_contr = contr$ci[1],
    upper_cl_contr = contr$ci[2],
    p_value = contr$pvalue,
    relative_reduc = contr$est / df_ref$est,
    stringsAsFactors = FALSE
  )

  result <- rbind(
    df_ref,
    df_alt
  )

  result
}

#' @describeIn tabulate_rbmi Helper method (for [broom::tidy()]) to prepare a data frame from an
#'   `pool` rbmi object containing the LS means and contrasts and multiple visits
#' @method tidy pool
#' @param x (`pool`)
#' @export
#' @return A dataframe
#'
tidy.pool <- function(x) { # nolint #nousage

  ls_raw <- x$pars

  visit_raw_names <- names(ls_raw)[grep("trt_", names(ls_raw))]
  l_visit_names <- strsplit(visit_raw_names, "trt_")
  visit_names <- vapply(l_visit_names, `[`, 2, FUN.VALUE = character(1))

  spl <- rep(visit_names, each = 3)

  ls_split <- split(ls_raw, spl)

  ls_df <- lapply(ls_split, h_tidy_pool)

  result <- do.call(rbind, unname(ls_df))

  result$visit <- factor(rep(visit_names, each = 2))
  result$group <- factor(result$group, levels = c("ref", "alt"))
  result$conf_level <- x$conf.level

  result

}

#' @describeIn tabulate_rbmi Statistics function which is extracting estimates from a tidied LS means
#'   data frame.
#' @inheritParams argument_convention
#' @param show_relative should the "reduction" (`control - treatment`, default) or the "increase"
#'   (`treatment - control`) be shown for the relative change from baseline?
#' @export
#' @examples
#' \dontrun{
#' s_rbmi_lsmeans(df[8, ], .in_ref_col = FALSE)
#' }
s_rbmi_lsmeans <- function(df, .in_ref_col, show_relative = c("reduction", "increase")) {
  show_relative <- match.arg(show_relative)
  if_not_ref <- function(x) `if`(.in_ref_col, character(), x)
  list(
    adj_mean_se = c(df$est, df$se_est),
    adj_mean_ci = with_label(c(df$lower_cl_est, df$upper_cl_est), tern:::f_conf_level(df$conf_level)),
    diff_mean_se = if_not_ref(c(df$est_contr, df$se_contr)),
    diff_mean_ci = with_label(if_not_ref(c(df$lower_cl_contr, df$upper_cl_contr)), tern:::f_conf_level(df$conf_level)),
    change = switch(
      show_relative,
      reduction = with_label(if_not_ref(df$relative_reduc), "Relative Reduction (%)"),
      increase = with_label(if_not_ref(- df$relative_reduc), "Relative Increase (%)")
    ),
    p_value = if_not_ref(df$p_value)
  )
}

#' @describeIn tabulate_rbmi Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
a_rbmi_lsmeans <- make_afun(
  s_rbmi_lsmeans,
  .labels = c(
    adj_mean_se = "Adjusted Mean (SE)",
    diff_mean_se = "Difference in Adjusted Means (SE)",
    p_value = "p-value (RBMI)"
  ),
  .formats = c(
    #n = "xx.", # note we don't have N from rbmi result
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

#' @describeIn tabulate_rbmi Analyze function for tabulating LS means estimates from tidied
#'   rbmi `pool` results.
#' @inheritParams argument_convention
#' @export
#' @examples
#'
#' \dontrun{
#' library(rtables)
#' library(dplyr)
#' library(broom)
#' library(rbmi)
#'
#' path <- "~/NEST/rbmi_rds-export"
#' pool_obj <- readRDS(file.path(path, "pool_obj.RDS"))
#'
#' h_tidy_pool(pool_obj$pars[1:3])
#' df <- tidy(pool_obj)
#' df
#'
#' s_rbmi_lsmeans(df[2, ], .in_ref_col = FALSE)
#'
#' afun <- make_afun(a_rbmi_lsmeans)
#' afun(df[2, ], .in_ref_col = FALSE)
#'
#' basic_table() %>%
#'   split_cols_by("group", ref_group = levels(df$group)[1]) %>%
#'   split_rows_by("visit", split_label = "Visit", label_pos = "topleft") %>%
#'   summarize_rbmi() %>%
#'   build_table(df)
#' }
#'
summarize_rbmi <- function(lyt,
                           ...,
                           table_names = "rbmi_summary",
                           .stats = NULL,
                           .formats = NULL,
                           .indent_mods = NULL,
                           .labels = NULL) {
  afun <- make_afun(
    a_rbmi_lsmeans,
    .stats = .stats,
    .formats = .formats,
    .indent_mods = .indent_mods,
    .labels = .labels
  )
  analyze(
    lyt = lyt,
    vars = "est",
    afun = afun,
    table_names = table_names,
    extra_args = list(...)
  )
}
