#' Standard Arguments
#'
#' The documentation to this function lists all the arguments in `tern`
#' that are used repeatedly to express an analysis.
#'
#' @details Although this function just returns `NULL` it has two uses, for
#' the `tern` users it provides a documentation of arguments that are
#' commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the `roxygen` argument description with:
#' `@inheritParams argument_convention`
#'
#' @md
#' @param ... additional arguments for the lower level functions.
#' @param .in_ref_col (`logical`)\cr `TRUE` when working with the reference level, `FALSE` otherwise.
#' @param .N_col (`count`)\cr row-wise N (row group count) for the group of observations being analyzed
#'   (i.e. with no column-based subsetting) that is passed by `rtables`.
#' @param .N_row (`count`)\cr column-wise N (column count) for the full column that is passed by `rtables`.
#' @param .ref_group (`data frame` or `vector`)\cr the data corresponding to the reference group.
#' @param .stats (`character`)\cr statistics to select for the table.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels.
#' @param .formats (named `character` or `list`)\cr formats for the statistics.
#' @param .labels (named `character`)\cr labels for the statistics (without indent).
#' @param .var (`string`)\cr single variable name that is passed by `rtables` when requested
#'   by a statistics function.
#' @param col_by (`factor`)\cr defining column groups.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param df (`data frame`)\cr data set containing all analysis variables.
#' @param id (`string`) \cr subject variable.
#' @param is_event (`logical`)\cr `TRUE` if event, `FALSE` if time to event is censored.
#' @param labelstr (`character`)\cr label of the level of the parent split currently being summarized
#'   (must be present as second argument in Content Row Functions).
#' @param lyt (`layout`)\cr input layout where analyses will be added to.
#' @param na.rm (`flag`)\cr whether `NA` values should be removed from `x` prior to analysis.
#' @param na_rm (`flag`)\cr (deprecated) whether `NA` values should be removed from `x` prior to analysis.
#' @param prune_zero_rows (`flag`)\cr whether to prune all zero rows.
#' @param rsp (`logical`)\cr whether each subject is a responder or not.
#' @param show_labels label visibility: one of "default", "visible" and "hidden".
#' @param tte (`\code{numeric}`)\cr contains time-to-event duration values.
#' @param var_labels character for label.
#' @param variables (named `list` of `strings`)\cr list of additional analysis variables.
#' @param vars (`character`)\cr variable names for the primary analysis variable to be iterated over.
#' @param x (`numeric`)\cr vector of numbers we want to analyze.
#'
#' @name argument_convention
#'
NULL
