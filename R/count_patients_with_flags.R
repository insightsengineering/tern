#' Count the Number of Patients with Particular Flags
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The primary analysis variable `.var` denotes the unique patient identifier.
#'
#' @inheritParams argument_convention
#' @param flag_variables (`character`)\cr a character vector specifying the names of `logical`
#'   variables from analysis dataset used for counting the number of unique identifiers.
#' @param flag_labels (`character`)\cr vector of labels to use for flag variables.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("count_patients_with_flags")`
#'   to see available statistics for this function.
#'
#' @seealso [count_patients_with_event]
#'
#' @name count_patients_with_flags
#' @order 1
NULL

#' @describeIn count_patients_with_flags Statistics function which counts the number of patients for which
#'   a particular flag variable is `TRUE`.
#'
#' @inheritParams analyze_variables
#' @param .var (`character`)\cr name of the column that contains the unique identifier.
#'
#' @note If `flag_labels` is not specified, variables labels will be extracted from `df`. If variables are not
#'   labeled, variable names will be used instead. Alternatively, a named `vector` can be supplied to
#'   `flag_variables` such that within each name-value pair the name corresponds to the variable name and the value is
#'   the label to use for this variable.
#'
#' @return
#' * `s_count_patients_with_flags()` returns the count and the fraction of unique identifiers with each particular
#'   flag as a list of statistics `n`, `count`, `count_fraction`, and `n_blq`, with one element per flag.
#'
#' @examples
#' # `s_count_patients_with_flags()`
#'
#' s_count_patients_with_flags(
#'   adae,
#'   "SUBJID",
#'   flag_variables = c("fl1", "fl2", "fl3", "fl4"),
#'   denom = "N_col",
#'   .N_col = 1000
#' )
#'
#' @export
s_count_patients_with_flags <- function(df,
                                        .var,
                                        flag_variables,
                                        flag_labels = NULL,
                                        .N_col, # nolint
                                        .N_row, # nolint
                                        denom = c("n", "N_row", "N_col")) {
  checkmate::assert_character(flag_variables)
  if (!is.null(flag_labels)) {
    checkmate::assert_character(flag_labels, len = length(flag_variables), any.missing = FALSE)
    flag_names <- flag_labels
  } else {
    if (is.null(names(flag_variables))) {
      flag_names <- formatters::var_labels(df[flag_variables], fill = TRUE)
    } else {
      flag_names <- unname(flag_variables)
      flag_variables <- names(flag_variables)
    }
  }

  checkmate::assert_subset(flag_variables, colnames(df))
  temp <- sapply(flag_variables, function(x) {
    tmp <- Map(function(y) which(df[[y]]), x)
    position_satisfy_flags <- Reduce(intersect, tmp)
    id_satisfy_flags <- as.character(unique(df[position_satisfy_flags, ][[.var]]))
    s_count_values(
      as.character(unique(df[[.var]])),
      id_satisfy_flags,
      denom = denom,
      .N_col = .N_col,
      .N_row = .N_row
    )
  })
  colnames(temp) <- flag_names
  temp <- data.frame(t(temp))
  result <- temp %>% as.list()
  if (length(flag_variables) == 1) {
    for (i in 1:3) names(result[[i]]) <- flag_names[1]
  }
  result
}

#' @describeIn count_patients_with_flags Formatted analysis function which is used as `afun`
#'   in `count_patients_with_flags()`.
#'
#' @return
#' * `a_count_patients_with_flags()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' #  We need to ungroup `count_fraction` first so that the `rtables` formatting
#' # function `format_count_fraction()` can be applied correctly.
#'
#' # `a_count_patients_with_flags()`
#'
#' afun <- make_afun(a_count_patients_with_flags,
#'   .stats = "count_fraction",
#'   .ungroup_stats = "count_fraction"
#' )
#' afun(
#'   adae,
#'   .N_col = 10L,
#'   .N_row = 10L,
#'   .var = "USUBJID",
#'   flag_variables = c("fl1", "fl2", "fl3", "fl4")
#' )
#'
#' @export
a_count_patients_with_flags <- make_afun(
  s_count_patients_with_flags,
  .formats = c("count_fraction" = format_count_fraction_fixed_dp)
)

#' @describeIn count_patients_with_flags Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_patients_with_flags()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_patients_with_flags()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' # Add labelled flag variables to analysis dataset.
#' adae <- tern_ex_adae %>%
#'   mutate(
#'     fl1 = TRUE %>% with_label("Total AEs"),
#'     fl2 = (TRTEMFL == "Y") %>%
#'       with_label("Total number of patients with at least one adverse event"),
#'     fl3 = (TRTEMFL == "Y" & AEOUT == "FATAL") %>%
#'       with_label("Total number of patients with fatal AEs"),
#'     fl4 = (TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y") %>%
#'       with_label("Total number of patients with related fatal AEs")
#'   )
#'
#' # `count_patients_with_flags()`
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_patients_with_flags(
#'     "SUBJID",
#'     flag_variables = c("fl1", "fl2", "fl3", "fl4"),
#'     denom = "N_col"
#'   )
#'
#' build_table(lyt2, adae, alt_counts_df = tern_ex_adsl)
#'
#' @export
#' @order 2
count_patients_with_flags <- function(lyt,
                                      var,
                                      flag_variables,
                                      flag_labels = NULL,
                                      var_labels = var,
                                      show_labels = "hidden",
                                      riskdiff = FALSE,
                                      na_str = default_na_str(),
                                      nested = TRUE,
                                      ...,
                                      table_names = paste0("tbl_flags_", var),
                                      .stats = "count_fraction",
                                      .formats = NULL,
                                      .indent_mods = NULL) {
  checkmate::assert_flag(riskdiff)

  s_args <- list(flag_variables = flag_variables, flag_labels = flag_labels, ...)

  afun <- make_afun(
    a_count_patients_with_flags,
    .stats = .stats,
    .formats = .formats,
    .indent_mods = .indent_mods,
    .ungroup_stats = .stats
  )

  extra_args <- if (isFALSE(riskdiff)) {
    s_args
  } else {
    list(
      afun = list("s_count_patients_with_flags" = afun),
      .stats = .stats,
      .indent_mods = .indent_mods,
      s_args = s_args
    )
  }

  lyt <- analyze(
    lyt = lyt,
    vars = var,
    var_labels = var_labels,
    show_labels = show_labels,
    afun = ifelse(isFALSE(riskdiff), afun, afun_riskdiff),
    table_names = table_names,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )

  lyt
}
