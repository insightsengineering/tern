#' Patient Counts with Abnormal Range Values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Primary analysis variable `.var` indicates the abnormal range result (`character` or `factor`)
#' and additional analysis variables are `id` (`character` or `factor`) and `baseline` (`character` or
#' `factor`). For each direction specified in `abnormal` (e.g. high or low) count patients in the
#' numerator and denominator as follows:
#'   * `num` : The number of patients with this abnormality recorded while on treatment.
#'   * `denom`: The number of patients with at least one post-baseline assessment.
#'
#' @inheritParams argument_convention
#' @param abnormal (named `list`)\cr list identifying the abnormal range level(s) in `var`. Defaults to
#'   `list(Low = "LOW", High = "HIGH")` but you can also group different levels into the named list,
#'   for example, `abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH"))`.
#' @param exclude_base_abn (`flag`)\cr whether to exclude subjects with baseline abnormality
#'   from numerator and denominator.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("abnormal")`
#'   to see available statistics for this function.
#'
#' @note
#' * `count_abnormal()` only works with a single variable containing multiple abnormal levels.
#' * `df` should be filtered to include only post-baseline records.
#' * the denominator includes patients that might have other abnormal levels at baseline,
#'   and patients with missing baseline. Patients with these abnormalities at
#'   baseline can be optionally excluded from numerator and denominator.
#'
#' @name abnormal
#' @include formatting_functions.R
#' @order 1
NULL

#' @describeIn abnormal Statistics function which counts patients with abnormal range values
#'   for a single `abnormal` level.
#'
#' @return
#' * `s_count_abnormal()` returns the statistic `fraction` which is a vector with `num` and `denom` counts of patients.
#'
#' @keywords internal
s_count_abnormal <- function(df,
                             .var,
                             abnormal = list(Low = "LOW", High = "HIGH"),
                             variables = list(id = "USUBJID", baseline = "BNRIND"),
                             exclude_base_abn = FALSE) {
  checkmate::assert_list(abnormal, types = "character", names = "named", len = 2, any.missing = FALSE)
  checkmate::assert_true(any(unlist(abnormal) %in% levels(df[[.var]])))
  checkmate::assert_factor(df[[.var]])
  checkmate::assert_flag(exclude_base_abn)
  assert_df_with_variables(df, c(range = .var, variables))
  checkmate::assert_multi_class(df[[variables$baseline]], classes = c("factor", "character"))
  checkmate::assert_multi_class(df[[variables$id]], classes = c("factor", "character"))

  count_abnormal_single <- function(abn_name, abn) {
    # Patients in the denominator fulfill:
    # - have at least one post-baseline visit
    # - their baseline must not be abnormal if `exclude_base_abn`.
    if (exclude_base_abn) {
      denom_select <- !(df[[variables$baseline]] %in% abn)
    } else {
      denom_select <- TRUE
    }
    denom <- length(unique(df[denom_select, variables$id, drop = TRUE]))

    # Patients in the numerator fulfill:
    # - have at least one post-baseline visit with the required abnormality level
    # - are part of the denominator patients.
    num_select <- (df[[.var]] %in% abn) & denom_select
    num <- length(unique(df[num_select, variables$id, drop = TRUE]))

    formatters::with_label(c(num = num, denom = denom), abn_name)
  }

  # This will define the abnormal levels theoretically possible for a specific lab parameter
  # within a split level of a layout.
  abnormal_lev <- lapply(abnormal, intersect, levels(df[[.var]]))
  abnormal_lev <- abnormal_lev[vapply(abnormal_lev, function(x) length(x) > 0, logical(1))]

  result <- sapply(names(abnormal_lev), function(i) count_abnormal_single(i, abnormal_lev[[i]]), simplify = FALSE)
  result <- list(fraction = result)
  result
}

#' @describeIn abnormal Formatted analysis function which is used as `afun` in `count_abnormal()`.
#'
#' @return
#' * `a_count_abnormal()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_abnormal <- make_afun(
  s_count_abnormal,
  .formats = c(fraction = format_fraction)
)

#' @describeIn abnormal Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_abnormal()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_abnormal()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 2, 2)),
#'   ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
#'   BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
#'   ONTRTFL = c("", "Y", "", "Y"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Select only post-baseline records.
#' df <- df %>%
#'   filter(ONTRTFL == "Y")
#'
#' # Layout creating function.
#' basic_table() %>%
#'   count_abnormal(var = "ANRIND", abnormal = list(high = "HIGH", low = "LOW")) %>%
#'   build_table(df)
#'
#' # Passing of statistics function and formatting arguments.
#' df2 <- data.frame(
#'   ID = as.character(c(1, 1, 2, 2)),
#'   RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
#'   BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
#'   ONTRTFL = c("", "Y", "", "Y"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Select only post-baseline records.
#' df2 <- df2 %>%
#'   filter(ONTRTFL == "Y")
#'
#' basic_table() %>%
#'   count_abnormal(
#'     var = "RANGE",
#'     abnormal = list(low = "LOW", high = "HIGH"),
#'     variables = list(id = "ID", baseline = "BL_RANGE")
#'   ) %>%
#'   build_table(df2)
#'
#' @export
#' @order 2
count_abnormal <- function(lyt,
                           var,
                           abnormal = list(Low = "LOW", High = "HIGH"),
                           variables = list(id = "USUBJID", baseline = "BNRIND"),
                           exclude_base_abn = FALSE,
                           na_str = default_na_str(),
                           nested = TRUE,
                           ...,
                           table_names = var,
                           .stats = NULL,
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {
  extra_args <- list(abnormal = abnormal, variables = variables, exclude_base_abn = exclude_base_abn, ...)

  afun <- make_afun(
    a_count_abnormal,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "fraction"
  )

  checkmate::assert_string(var)

  analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    na_str = na_str,
    nested = nested,
    table_names = table_names,
    extra_args = extra_args,
    show_labels = "hidden"
  )
}
