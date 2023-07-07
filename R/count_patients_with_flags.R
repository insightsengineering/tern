#' Count the Number of Patients with Particular Flags
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The primary analysis variable `.var` denotes the unique patient identifier.
#'
#' @inheritParams argument_convention
#'
#' @seealso [count_patients_with_event]
#'
#' @name count_patients_with_flags
NULL

#' @describeIn count_patients_with_flags Statistics function which counts the number of patients for which
#'   a particular flag variable is `TRUE`.
#'
#' @inheritParams analyze_variables
#' @param .var (`character`)\cr name of the column that contains the unique identifier.
#' @param flag_variables (`character`)\cr a character vector specifying the names of `logical`
#'   variables from analysis dataset used for counting the number of unique identifiers.
#'
#' @return
#' * `s_count_patients_with_flags()` returns the count and the fraction of unique identifiers with each particular
#'   flag as a list of statistics `n`, `count`, `count_fraction`, and `n_blq`, with one element per flag.
#'
#' @examples
#' library(dplyr)
#'
#' # `s_count_patients_with_flags()`
#'
#' # Add labelled flag variables to analysis dataset.
#' adae <- tern_ex_adae %>%
#'   mutate(
#'     fl1 = TRUE,
#'     fl2 = TRTEMFL == "Y",
#'     fl3 = TRTEMFL == "Y" & AEOUT == "FATAL",
#'     fl4 = TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y"
#'   )
#' labels <- c(
#'   "fl1" = "Total AEs",
#'   "fl2" = "Total number of patients with at least one adverse event",
#'   "fl3" = "Total number of patients with fatal AEs",
#'   "fl4" = "Total number of patients with related fatal AEs"
#' )
#' formatters::var_labels(adae)[names(labels)] <- labels
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
                                        .N_col, # nolint
                                        .N_row, # nolint
                                        denom = c("n", "N_row", "N_col")) {
  checkmate::assert_character(flag_variables)
  if (is.null(names(flag_variables)))
    flag_variables <- stats::setNames(flag_variables, flag_variables)
  flag_names <- unname(flag_variables)
  flag_variables <- names(flag_variables)

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
#' # `count_patients_with_flags()`
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_patients_with_flags(
#'     "SUBJID",
#'     flag_variables = formatters::var_labels(adae[, c("fl1", "fl2", "fl3", "fl4")]),
#'     denom = "N_col"
#'   )
#' build_table(lyt2, adae, alt_counts_df = tern_ex_adsl)
#'
#' @export
count_patients_with_flags <- function(lyt,
                                      var,
                                      var_labels = var,
                                      show_labels = "hidden",
                                      ...,
                                      table_names = paste0("tbl_flags_", var),
                                      .stats = "count_fraction",
                                      .formats = NULL,
                                      .indent_mods = NULL) {
  afun <- make_afun(
    a_count_patients_with_flags,
    .stats = .stats,
    .formats = .formats,
    .indent_mods = .indent_mods,
    .ungroup_stats = .stats
  )

  lyt <- analyze(
    lyt = lyt,
    vars = var,
    var_labels = var_labels,
    show_labels = show_labels,
    afun = afun,
    table_names = table_names,
    extra_args = list(...)
  )

  lyt
}
