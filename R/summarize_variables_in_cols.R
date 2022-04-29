#' Summary numeric variables in columns
#'
#' These functions can be used to produce summary tables for PK datasets.
#'
#' @name summarize_variables_in_columns
#'
NULL

#' @describeIn summarize_variables_in_columns a wrapper of [s_summary.numeric()]
#'  function that produces a named list of statistics to include as columns.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.numeric()].
#' See [s_summary.numeric()] to be aware of all available statistics.
#'
#' @export
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' ADPP <- scda::synthetic_cdisc_data("latest")$adpp %>% h_pkparam_sort()
#' summary_in_cols(ADPP$AGE, custom_label = "stats")
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
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams argument_convention
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is
#'  empty then this will be used as the row label.
#'
#' @return A named list of all statistics returned by [s_summary.factor()].
#' See [s_summary.factor()] to be aware of all available statistics.
#'
#' @export
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' ADPC <- scda::synthetic_cdisc_data("latest")$adpc
#' summary_in_cols(as.factor(ADPC$AVALC), custom_label = "stats")
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
#' @examples
#'
#' library(scda)
#' library(dplyr)
#' ADPC <- scda::synthetic_cdisc_data("latest")$adpc
#' summary_in_cols.character(ADPC$AVALC, custom_label = "stats")
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



#' @inheritParams argument_convention
#' @param control a (`list`) of parameters for descriptive statistics details, specified by using \cr
#'    the helper function [control_summarize_vars()]. Some possible parameter options are: \cr
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for mean and median.
#' * `quantiles`: numeric vector of length two to specify the quantiles.
#' * `quantile_type` (`numeric`) \cr between 1 and 9 selecting quantile algorithms to be used. \cr
#'   See more about `type` in [stats::quantile()].
#'
#' @describeIn summarize_variables_in_columns `summary_in_cols` is a S3 generic
#' function to produce an object description.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @export
#' @order 2
#'
summary_in_cols <- function(x,
                            labelstr = "",
                            custom_label = NULL) {
  UseMethod("summary_in_cols", x)
}


#' @describeIn summarize_variables_in_columns Layout creating
#' function which can be used for creating summary tables in columns, primarily used for PK data sets.
#'
#'  `r lifecycle::badge("experimental")`
#'
#' @inheritParams argument_convention
#' @param col_split (`flag`)\cr whether the columns should be split.
#'
#' @seealso [summarize_vars].
#'
#' @export
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   summarize_vars_in_cols(var = "AGE", col_split = TRUE)
#' result <- build_table(lyt = lyt, df = ADPP)
#' result
#'
#' # By selecting just some statistics and ad-hoc labels
#' lyt <- basic_table() %>%
#'   split_rows_by(var = "ARM", label_pos = "topleft") %>%
#'   split_rows_by(var = "SEX", label_pos = "topleft") %>%
#'   summarize_vars_in_cols(
#'     var = "AGE",
#'     .stats = c("n", "cv", "geom_mean", "mean_ci", "median", "min", "max"),
#'     .labels = c(
#'       n = "myN",
#'       cv = "myCV",
#'       geom_mean = "myGeomMean",
#'       mean_ci = "Mean (95%CI)",
#'       median = "Median",
#'       min = "Minimum",
#'       max = "Maximum"
#'     ),
#'     col_split = TRUE
#'   )
#' result <- build_table(lyt = lyt, df = ADPP)
#' result
#'
#' lyt <- basic_table() %>%
#'   summarize_vars_in_cols(
#'     var = "AGE",
#'     col_split = TRUE,
#'     custom_label = "some custom label"
#'   )
#' result <- build_table(lyt, df = ADPP)
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
#' result <- build_table(lyt, df = ADPP)
#' result
summarize_vars_in_cols <- function(lyt,
                                           var,
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
                                           .indent_mods = NULL,
                                           col_split = TRUE) {
  afun_list <- Map(
    function(stat) {
      make_afun(
        summary_in_cols,
        .stats = stat,
        .formats = summary_formats()[names(summary_formats()) == stat]
      )
    },
    stat = .stats
  )

  if (col_split) {
    vars <- rep(var, length(.stats))

    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = vars,
      varlabels = .labels[.stats]
    )
  }

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = afun_list,
    extra_args = list(...)
  )
}
