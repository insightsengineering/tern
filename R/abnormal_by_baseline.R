#' Count patients with abnormal analysis range values by baseline status
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_abnormal_by_baseline()] creates a layout element to count patients with abnormal
#' analysis range values, categorized by baseline status.
#'
#' This function analyzes primary analysis variable `var` which indicates abnormal range results. Additional
#' analysis variables that can be supplied as a list via the `variables` parameter are `id` (defaults to
#' `USUBJID`), a variable to indicate unique subject identifiers, and `baseline` (defaults to `BNRIND`), a
#' variable to indicate baseline reference ranges.
#'
#' For each direction specified via the `abnormal` parameter (e.g. High or Low), we condition on baseline
#' range result and count patients in the numerator and denominator as follows for each of the following
#' categories:
#'   * `Not <abnormality>`
#'     * `num`:  The number of patients without abnormality at baseline (excluding those with missing baseline)
#'       and with at least one abnormality post-baseline.
#'     * `denom`: The number of patients without abnormality at baseline (excluding those with missing baseline).
#'   * `<Abnormality>`
#'     * `num`: The number of patients with abnormality as baseline and at least one abnormality post-baseline.
#'     * `denom`: The number of patients with abnormality at baseline.
#'   * `Total`
#'     * `num`: The number of patients with at least one post-baseline record and at least one abnormality
#'       post-baseline.
#'     * `denom`: The number of patients with at least one post-baseline record.
#'
#' This function assumes that `df` has been filtered to only include post-baseline records.
#'
#' @inheritParams argument_convention
#' @param abnormal (`character`)\cr values identifying the abnormal range level(s) in `.var`.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("abnormal_by_baseline"), type = "sh")``
#'
#' @note
#' * `df` should be filtered to include only post-baseline records.
#' * If the baseline variable or analysis variable contains `NA` records, it is expected that `df` has been
#'   pre-processed using [df_explicit_na()] or [explicit_na()].
#'
#' @seealso Relevant description function [d_count_abnormal_by_baseline()].
#'
#' @name abnormal_by_baseline
#' @order 1
NULL

#' Description function for `s_count_abnormal_by_baseline()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Description function that produces the labels for [s_count_abnormal_by_baseline()].
#'
#' @inheritParams abnormal_by_baseline
#'
#' @return Abnormal category labels for [s_count_abnormal_by_baseline()].
#'
#' @examples
#' d_count_abnormal_by_baseline("LOW")
#'
#' @export
d_count_abnormal_by_baseline <- function(abnormal) {
  not_abn_name <- paste("Not", tolower(abnormal))
  abn_name <- paste0(toupper(substr(abnormal, 1, 1)), tolower(substring(abnormal, 2)))
  total_name <- "Total"

  list(
    not_abnormal = not_abn_name,
    abnormal = abn_name,
    total = total_name
  )
}

#' @describeIn abnormal_by_baseline Statistics function for a single `abnormal` level.
#'
#' @param na_str (`string`)\cr the explicit `na_level` argument you used in the pre-processing steps (maybe with
#'   [df_explicit_na()]). The default is `"<Missing>"`.
#'
#' @return
#' * `s_count_abnormal_by_baseline()` returns statistic `fraction` which is a named list with 3 labeled elements:
#'   `not_abnormal`, `abnormal`, and `total`. Each element contains a vector with `num` and `denom` patient counts.
#'
#' @keywords internal
s_count_abnormal_by_baseline <- function(df,
                                         .var,
                                         abnormal,
                                         na_str = "<Missing>",
                                         variables = list(id = "USUBJID", baseline = "BNRIND"),
                                         ...) {
  checkmate::assert_string(.var)
  checkmate::assert_string(abnormal)
  checkmate::assert_string(na_str)
  assert_df_with_variables(df, c(range = .var, variables))
  checkmate::assert_subset(names(variables), c("id", "baseline"))
  checkmate::assert_multi_class(df[[variables$id]], classes = c("factor", "character"))
  checkmate::assert_multi_class(df[[variables$baseline]], classes = c("factor", "character"))
  checkmate::assert_multi_class(df[[.var]], classes = c("factor", "character"))

  # If input is passed as character, changed to factor
  df[[.var]] <- as_factor_keep_attributes(df[[.var]], na_level = na_str)
  df[[variables$baseline]] <- as_factor_keep_attributes(df[[variables$baseline]], na_level = na_str)

  assert_valid_factor(df[[.var]], any.missing = FALSE)
  assert_valid_factor(df[[variables$baseline]], any.missing = FALSE)

  # Keep only records with valid analysis value.
  df <- df[df[[.var]] != na_str, ]

  anl <- data.frame(
    id = df[[variables$id]],
    var = df[[.var]],
    baseline = df[[variables$baseline]],
    stringsAsFactors = FALSE
  )

  # Total:
  #  - Patients in denominator: have at least one valid measurement post-baseline.
  #  - Patients in numerator: have at least one abnormality.
  total_denom <- length(unique(anl$id))
  total_num <- length(unique(anl$id[anl$var == abnormal]))

  # Baseline NA records are counted only in total rows.
  anl <- anl[anl$baseline != na_str, ]

  # Abnormal:
  #   - Patients in denominator: have abnormality at baseline.
  #   - Patients in numerator: have abnormality at baseline AND
  #     have at least one abnormality post-baseline.
  abn_denom <- length(unique(anl$id[anl$baseline == abnormal]))
  abn_num <- length(unique(anl$id[anl$baseline == abnormal & anl$var == abnormal]))

  # Not abnormal:
  #   - Patients in denominator: do not have abnormality at baseline.
  #   - Patients in numerator: do not have abnormality at baseline AND
  #     have at least one abnormality post-baseline.
  not_abn_denom <- length(unique(anl$id[anl$baseline != abnormal]))
  not_abn_num <- length(unique(anl$id[anl$baseline != abnormal & anl$var == abnormal]))

  labels <- d_count_abnormal_by_baseline(abnormal)
  list(fraction = list(
    not_abnormal = formatters::with_label(c(num = not_abn_num, denom = not_abn_denom), labels$not_abnormal),
    abnormal = formatters::with_label(c(num = abn_num, denom = abn_denom), labels$abnormal),
    total = formatters::with_label(c(num = total_num, denom = total_denom), labels$total)
  ))
}

#' @describeIn abnormal_by_baseline Formatted analysis function which is used as `afun`
#'   in `count_abnormal_by_baseline()`.
#'
#' @return
#' * `a_count_abnormal_by_baseline()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_abnormal_by_baseline <- function(df,
                                         ...,
                                         .stats = NULL,
                                         .formats = NULL,
                                         .labels = NULL,
                                         .indent_mods = NULL) {
  # Check for additional parameters to the s_* function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  # Apply s_* function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_abnormal_by_baseline,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("abnormal_by_baseline", stats_in = .stats)
  levels_per_stats <- lapply(x_stats, names)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats, d_count_abnormal_by_baseline(dots_extra_args$abnormal)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  x_stats <- x_stats[.stats]

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  in_rows(
    .list = x_stats %>% .unlist_keep_nulls(),
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn abnormal_by_baseline Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_abnormal_by_baseline()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_abnormal_by_baseline()` to the table layout.
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = as.character(c(1:6)),
#'   ANRIND = factor(c(rep("LOW", 4), "NORMAL", "HIGH")),
#'   BNRIND = factor(c("LOW", "NORMAL", "HIGH", NA, "LOW", "NORMAL"))
#' )
#' df <- df_explicit_na(df)
#'
#' # Layout creating function.
#' basic_table() %>%
#'   count_abnormal_by_baseline(var = "ANRIND", abnormal = c(High = "HIGH")) %>%
#'   build_table(df)
#'
#' # Passing of statistics function and formatting arguments.
#' df2 <- data.frame(
#'   ID = as.character(c(1, 2, 3, 4)),
#'   RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
#'   BLRANGE = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
#' )
#'
#' basic_table() %>%
#'   count_abnormal_by_baseline(
#'     var = "RANGE",
#'     abnormal = c(Low = "LOW"),
#'     variables = list(id = "ID", baseline = "BLRANGE"),
#'     .formats = c(fraction = "xx / xx"),
#'     .indent_mods = c(fraction = 2L)
#'   ) %>%
#'   build_table(df2)
#'
#' @export
#' @order 2
count_abnormal_by_baseline <- function(lyt,
                                       var,
                                       abnormal,
                                       variables = list(id = "USUBJID", baseline = "BNRIND"),
                                       na_str = "<Missing>",
                                       nested = TRUE,
                                       ...,
                                       table_names = abnormal,
                                       .stats = "fraction",
                                       .formats = list(fraction = format_fraction),
                                       .labels = NULL,
                                       .indent_mods = NULL) {
  checkmate::assert_character(abnormal, len = length(table_names), names = "named")
  checkmate::assert_string(var)

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(extra_args, "variables" = list(variables), ...)

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_abnormal_by_baseline) <- c(
    formals(a_count_abnormal_by_baseline), extra_args[[".additional_fun_parameters"]]
  )

  # Add a new table section with label for each value in abnormal
  for (i in seq_along(abnormal)) {
    extra_args[["abnormal"]] <- abnormal[i]

    lyt <- analyze(
      lyt = lyt,
      vars = var,
      afun = a_count_abnormal_by_baseline,
      var_labels = names(abnormal)[i],
      na_str = na_str,
      nested = nested,
      extra_args = extra_args,
      show_labels = "visible",
      table_names = table_names[i]
    )
  }

  lyt
}
