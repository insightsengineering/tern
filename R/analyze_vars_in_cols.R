#' Summary numeric variables in columns
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function can be used to produce summary tables for PK datasets where
#' the relevant statistic is on the columns instead of on the rows.
#'
#' @name analyze_vars_in_cols
#'
NULL

#' @describeIn analyze_vars_in_cols Layout creating
#' function which can be used for creating summary tables in columns, primarily used for PK data sets.
#'
#' @inheritParams argument_convention
#' @inheritParams rtables::analyze_colvars
#'
#' @seealso [summarize_vars], [rtables::analyze_colvars].
#'
#' @export
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
#' # PKPT03
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
                                  
                                           mean="Mean",
                                   sd = "SD",
                                   
                                   se = "SE",
                                   cv = "CV (%)",
                                   geom_cv = "CV % Geometric Mean"
                                 ),
                                 labelstr = " ",
                                 nested = TRUE,
                                 na_level = NULL,
                                 .formats = NULL) {
  checkmate::assert_string(na_level, null.ok = TRUE)
  checkmate::assert_string(labelstr)
  checkmate::assert_flag(nested)

  # Automatic assignment of formats
  if (is.null(.formats)) {
    # General values
    sf_numeric <- summary_formats("numeric")
    sf_counts <- summary_formats("counts")[-1]
    formats_v <- c(sf_numeric, sf_counts)
  } else {
    formats_v <- .formats
  }

  afun_list <- Map(
    function(stat) {
      make_afun(
        s_summary,
        .labels = labelstr,
        .stats = stat,
        .format_na_strs = na_level,
        .formats = formats_v[names(formats_v) == stat]
      )
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

  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = vars,
    varlabels = .labels[.stats]
  )

  analyze_colvars(lyt,
    afun = afun_list,
    nested = nested,
    extra_args = list(...)
  )
}
