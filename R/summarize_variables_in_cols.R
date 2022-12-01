#' Summary numeric variables in columns
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions can be used to produce summary tables for PK datasets.
#'
#' @name summarize_variables_in_columns
#'
NULL

#' @describeIn summarize_variables_in_columns a wrapper of [s_summary.numeric()]
#'  function that produces a named list of statistics to include as columns.
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.numeric()].
#' See [s_summary.numeric()] to be aware of all available statistics.
#'
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' adpp <- synthetic_cdisc_dataset("latest", "adpp") %>% h_pkparam_sort()
#' summary_in_cols(adpp$AGE, custom_label = "stats")
summary_in_cols.numeric <- function(x,
                                    labelstr = "",
                                    custom_label = NULL,
                                    ...) {
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Statistics"
  }

  # Calling s_summary.numeric
  results <- s_summary.numeric(x)

  lapply(results, formatters::with_label, row_label)
}

#' @describeIn summarize_variables_in_columns a wrapper of
#' [s_summary.factor()] function that produces a named list of statistics to include as columns.
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.factor()].
#' See [s_summary.factor()] to be aware of all available statistics.
#'
#' @export
summary_in_cols.factor <- function(x,
                                   labelstr = "",
                                   custom_label = NULL,
                                   ...) {
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Statistics"
  }

  # Calling s_summary.factor
  results <- s_summary.factor(x)

  lapply(results, formatters::with_label, row_label)
}

#' @describeIn summarize_variables_in_columns a wrapper of [s_summary.character()]
#'  function that produces a named list of statistics to include as columns.
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.character()].
#' See [s_summary.character()] to be aware of all available statistics.
#'
#' @export
summary_in_cols.character <- function(x,
                                      labelstr = "",
                                      custom_label = NULL,
                                      ...) {
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Statistics"
  }

  # Calling s_summary.character
  results <- s_summary.factor(as.factor(x))

  lapply(results, formatters::with_label, row_label)
}

#' @describeIn summarize_variables_in_columns a wrapper of [s_summary.logical()]
#'  function that produces a named list of statistics to include as columns.
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.logical()].
#' See [s_summary.logical()] to be aware of all available statistics.
#'
#' @export
summary_in_cols.logical <- function(x,
                                    labelstr = "",
                                    custom_label = NULL,
                                    ...) {
  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Statistics"
  }

  # Calling s_summary.logical
  results <- s_summary.logical(x)

  lapply(results, formatters::with_label, row_label)
}

#' @inheritParams argument_convention
#'
#' @describeIn summarize_variables_in_columns `summary_in_cols` is a S3 generic
#' function to produce an object description.
#'
#' @export
#' @order 2
summary_in_cols <- function(x,
                            labelstr = "",
                            custom_label = NULL) {
  UseMethod("summary_in_cols", x)
}

#' @describeIn summarize_variables_in_columns Layout creating
#' function which can be used for creating summary tables in columns, primarily used for PK data sets.
#'
#' @inheritParams argument_convention
#'
#' @seealso [summarize_vars].
#'
#' @export
#' @examples
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   summarize_vars_in_cols(vars = "AGE")
#' result <- build_table(lyt = lyt, df = adpp)
#' result
#'
#' # By selecting just some statistics and ad-hoc labels
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   summarize_vars_in_cols(
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
#'   summarize_vars_in_cols(
#'     vars = "AGE",
#'     col_split = TRUE,
#'     custom_label = "some custom label"
#'   )
#' result <- build_table(lyt, df = adpp)
#' result
#'
#' # PKPT03
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "TLG_DISPLAY", split_label = "PK Parameter", label_pos = "topleft") %>%
#'   summarize_vars_in_cols(
#'     var = "AVAL",
#'     col_split = TRUE,
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
summarize_vars_in_cols <- function(lyt,
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
                                   .formats = NULL,
                                   .indent_mods = NULL,
                                   col_split = TRUE,
                                   na_str = NULL) {

  checkmate::assert_string(na_str, null.ok = TRUE)

  # Automatic assignment of formats
  if (is.null(.formats)) {
    # Init
    formats_v <- rep("", length(.stats))
    names(formats_v) <- rep(" ", length(.stats))
    # General values
    sf_numeric <- summary_formats("numeric")
    sf_counts <- summary_formats("counts")[-1]
    is_num_format <- .stats %in% names(sf_numeric)
    is_count_format <- .stats %in% names(sf_counts)
    # Assignment
    if (any(is_num_format)) {
      formats_v[is_num_format] <- sf_numeric[names(sf_numeric) %in% .stats]
      names(formats_v)[is_num_format] <- names(sf_numeric[names(sf_numeric) %in% .stats])
    }
    if (any(is_count_format)) {
      formats_v[is_count_format] <- sf_counts[names(sf_counts) %in% .stats]
      names(formats_v)[is_count_format] <- names(sf_counts[names(sf_counts) %in% .stats])
    }
    if (!all(is_num_format | is_count_format)) {
      stop("Specified statistic has not a default format")
    }
  }

  afun_list <- Map(
    function(stat, ff) {
      make_afun(
        summary_in_cols,
        .labels = " ",
        .stats = stat,
        .format_na_strs = na_str,
        .formats = ff
      )
    },
    stat = .stats,
    ff = formats_v
  )

  # Check for vars in the case that one or more are used
  if (length(vars) == 1) {
    vars <- rep(vars, length(.stats))
  } else if (length(vars) != length(.stats)) {
    stop("Analyzed variables (vars) does not have the same ",
         "number of elements of specified statistics (.stats).")
  }

  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = vars,
    varlabels = .labels[.stats]
  )

  analyze_colvars(lyt,
                  afun = afun_list,
                  extra_args = list(...))
}
