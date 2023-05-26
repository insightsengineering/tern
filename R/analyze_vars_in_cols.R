#' Summary numeric variables in columns
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Layout-creating function which can be used for creating column-wise summary tables, primarily
#' used for PK data sets. This function is a wrapper for [rtables::analyze_colvars()].
#'
#' @inheritParams argument_convention
#' @inheritParams rtables::analyze_colvars
#' @param do_row_groups (`flag`)\cr defaults to `FALSE` and applies the analysis to the current
#'   label rows. This is a wrapper of [rtables::summarize_row_groups()] and it can accept `labelstr`
#'   to define row labels. This behavior is not supported as we never need to overload row labels.
#' @param split_col_vars (`flag`)\cr defaults to `TRUE` and puts the analysis results onto the columns.
#'   This option allows you to add multiple instances of this functions, also in a nested fashion,
#'   without adding more splits.
#'
#' @return
#' A layout object suitable for passing to further layouting functions, or to [rtables::build_table()].
#' Adding this function to an `rtable` layout will summarize the given variables, arrange the output
#' in columns, and add it to the table layout.
#'
#' @note This is an experimental implementation of [rtables::summarize_row_groups()] and
#'   [rtables::analyze_colvars()] that may be subjected to changes as `rtables` extends its
#'   support to more complex analysis pipelines on the column space. For the same reasons,
#'   we encourage to read the examples carefully and file issues for cases that differ from
#'   them.
#'
#' @seealso [summarize_vars()], [rtables::analyze_colvars()].
#'
#' @examples
#' library(dplyr)
#'
#' adpp <- tern_ex_adpp %>% h_pkparam_sort()
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   analyze_vars_in_cols(vars = "AGE")
#' result <- build_table(lyt = lyt, df = adpp)
#' result
#'
#' # By selecting just some statistics and ad-hoc labels
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   analyze_vars_in_cols(
#'     vars = "AGE",
#'     .stats = c("n", "cv", "geom_mean", "mean_ci", "median", "min", "max"),
#'     .labels = c(
#'       n = "myN",
#'       cv = "myCV",
#'       geom_mean = "myGeomMean",
#'       mean_ci = "Mean (95%CI)",
#'       median = "Median",
#'       min = "Minimum",
#'       max = "Maximum"
#'     )
#'   )
#' result <- build_table(lyt = lyt, df = adpp)
#' result
#'
#' lyt <- basic_table() %>%
#'   analyze_vars_in_cols(
#'     vars = "AGE",
#'     labelstr = "some custom label"
#'   )
#' result <- build_table(lyt, df = adpp)
#' result
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "TLG_DISPLAY", split_label = "PK Parameter", label_pos = "topleft") %>%
#'   analyze_vars_in_cols(
#'     vars = "AVAL",
#'     .stats = c("n", "mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
#'     .labels = c(
#'       n = "n",
#'       mean = "Mean",
#'       sd = "SD",
#'       cv = "CV (%)",
#'       geom_mean = "Geometric Mean",
#'       geom_cv = "CV % Geometric Mean",
#'       median = "Median",
#'       min = "Minimum",
#'       max = "Maximum"
#'     )
#'   )
#' result <- build_table(lyt, df = adpp)
#' result
#'
#' # Multiple calls
#'
#' @export
analyze_vars_in_cols <- function(lyt,
                                 vars,
                                 ...,
                                 .stats = c(
                                   "n",
                                   "mean",
                                   "sd",
                                   "se",
                                   "cv",
                                   "geom_cv"
                                 ),
                                 .labels = c(
                                   n = "n",
                                   mean = "Mean",
                                   sd = "SD",
                                   se = "SE",
                                   cv = "CV (%)",
                                   geom_cv = "CV % Geometric Mean"
                                 ),
                                 labelstr = " ",
                                 do_row_groups = FALSE,
                                 split_col_vars = TRUE,
                                 nested = TRUE,
                                 na_level = NULL,
                                 .formats = NULL) {
  checkmate::assert_string(na_level, null.ok = TRUE)
  checkmate::assert_string(labelstr)
  checkmate::assert_flag(nested)
  checkmate::assert_flag(do_row_groups)
  checkmate::assert_flag(split_col_vars)

  # Automatic assignment of formats
  if (is.null(.formats)) {
    # General values
    sf_numeric <- summary_formats("numeric")
    sf_counts <- summary_formats("counts")[-1]
    formats_v <- c(sf_numeric, sf_counts)
  } else {
    formats_v <- .formats
  }

  if (length(labelstr) == 1L) {
    labelstr <- rep(labelstr, length(.stats))
  }

  afun_list <- Map(
    function(stat) {
      function(u, .spl_context, labelstr, ...) {
        res <- s_summary(u, ...)[[stat]]
        lbl <- ifelse(do_row_groups, labelstr, .spl_context$value[nrow(.spl_context)])
        rcell(res,
              label = lbl,
              format = formats_v[names(formats_v) == stat][[1]],
              format_na_str = na_level)
      }
    },
    stat = .stats
  )

  # Check for vars in the case that one or more are used
  if (length(vars) == 1) {
    vars <- rep(vars, length(.stats))
  } else if (length(vars) != length(.stats)) {
    stop(
      "Analyzed variables (vars) does not have the same ",
      "number of elements of specified statistics (.stats)."
    )
  }

  if (split_col_vars) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = vars,
      varlabels = .labels[.stats]
    )
  }

  if (do_row_groups) {
    summarize_row_groups(
      lyt = lyt,
      var = vars[1],
      cfun = afun_list,
      extra_args = list(...)
    )
  } else {
    analyze_colvars(lyt,
      afun = afun_list,
      nested = nested,
      extra_args = list(...)
    )
  }
}
