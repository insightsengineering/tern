#' Count patients with toxicity grades that have worsened from baseline by highest grade post-baseline
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_abnormal_lab_worsen_by_baseline()] creates a layout element to count patients with
#' analysis toxicity grades which have worsened from baseline, categorized by highest (worst) grade post-baseline.
#'
#' This function analyzes primary analysis variable `var` which indicates analysis toxicity grades. Additional
#' analysis variables that can be supplied as a list via the `variables` parameter are `id` (defaults to `USUBJID`),
#' a variable to indicate unique subject identifiers, `baseline_var` (defaults to `BTOXGR`), a variable to indicate
#' baseline toxicity grades, and `direction_var` (defaults to `GRADDIR`), a variable to indicate toxicity grade
#' directions of interest to include (e.g. `"H"` (high), `"L"` (low), or `"B"` (both)).
#'
#' For the direction(s) specified in `direction_var`, patient counts by worst grade for patients who have
#' worsened from baseline are calculated as follows:
#'   * `1` to `4`: The number of patients who have worsened from their baseline grades with worst
#'     grades 1-4, respectively.
#'   * `Any`: The total number of patients who have worsened from their baseline grades.
#'
#' Fractions are calculated by dividing the above counts by the number of patients who's analysis toxicity grades
#' have worsened from baseline toxicity grades during treatment.
#'
#' Prior to using this function in your table layout you must use [rtables::split_rows_by()] to create a row
#' split on variable `direction_var`.
#'
#' @inheritParams argument_convention
#' @param variables (named `list` of `string`)\cr list of additional analysis variables including:
#'   * `id` (`string`)\cr subject variable name.
#'   * `baseline_var` (`string`)\cr name of the data column containing baseline toxicity variable.
#'   * `direction_var` (`string`)\cr see `direction_var` for more details.
#' @param .stats (`character`)\cr statistics to select for the table.
#' @param table_names `r lifecycle::badge("deprecated")` this parameter has no effect.
#'
#'   Options are: ``r shQuote(get_stats("abnormal_lab_worsen_by_baseline"), type = "sh")``
#'
#' @seealso Relevant helper functions [h_adlb_worsen()] and [h_worsen_counter()] which are used within
#' [s_count_abnormal_lab_worsen_by_baseline()] to process input data.
#'
#' @name abnormal_lab_worsen_by_baseline
#' @order 1
NULL

#' Helper function to prepare ADLB with worst labs
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to prepare a `df` for generate the patient count shift table.
#'
#' @param adlb (`data.frame`)\cr ADLB data frame.
#' @param worst_flag_low (named `vector`)\cr worst low post-baseline lab grade flag variable. See how this is
#'   implemented in the following examples.
#' @param worst_flag_high (named `vector`)\cr worst high post-baseline lab grade flag variable. See how this is
#'   implemented in the following examples.
#' @param direction_var (`string`)\cr name of the direction variable specifying the direction of the shift table of
#'   interest. Only lab records flagged by `L`, `H` or `B` are included in the shift table.
#'   * `L`: low direction only
#'   * `H`: high direction only
#'   * `B`: both low and high directions
#'
#' @return `h_adlb_worsen()` returns the `adlb` `data.frame` containing only the
#'   worst labs specified according to `worst_flag_low` or `worst_flag_high` for the
#'   direction specified according to `direction_var`. For instance, for a lab that is
#'   needed for the low direction only, only records flagged by `worst_flag_low` are
#'   selected. For a lab that is needed for both low and high directions, the worst
#'   low records are selected for the low direction, and the worst high record are selected
#'   for the high direction.
#'
#' @seealso [abnormal_lab_worsen_by_baseline]
#'
#' @examples
#' library(dplyr)
#'
#' # The direction variable, GRADDR, is based on metadata
#' adlb <- tern_ex_adlb %>%
#'   mutate(
#'     GRADDR = case_when(
#'       PARAMCD == "ALT" ~ "B",
#'       PARAMCD == "CRP" ~ "L",
#'       PARAMCD == "IGA" ~ "H"
#'     )
#'   ) %>%
#'   filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")
#'
#' df <- h_adlb_worsen(
#'   adlb,
#'   worst_flag_low = c("WGRLOFL" = "Y"),
#'   worst_flag_high = c("WGRHIFL" = "Y"),
#'   direction_var = "GRADDR"
#' )
#'
#' @export
h_adlb_worsen <- function(adlb,
                          worst_flag_low = NULL,
                          worst_flag_high = NULL,
                          direction_var) {
  checkmate::assert_string(direction_var)
  checkmate::assert_subset(as.character(unique(adlb[[direction_var]])), c("B", "L", "H"))
  assert_df_with_variables(adlb, list("Col" = direction_var))

  if (any(unique(adlb[[direction_var]]) == "H")) {
    assert_df_with_variables(adlb, list("High" = names(worst_flag_high)))
  }

  if (any(unique(adlb[[direction_var]]) == "L")) {
    assert_df_with_variables(adlb, list("Low" = names(worst_flag_low)))
  }

  if (any(unique(adlb[[direction_var]]) == "B")) {
    assert_df_with_variables(
      adlb,
      list(
        "Low" = names(worst_flag_low),
        "High" = names(worst_flag_high)
      )
    )
  }

  # extract patients with worst post-baseline lab, either low or high or both
  worst_flag <- c(worst_flag_low, worst_flag_high)
  col_names <- names(worst_flag)
  filter_values <- worst_flag
  temp <- Map(
    function(x, y) which(adlb[[x]] == y),
    col_names,
    filter_values
  )
  position_satisfy_filters <- Reduce(union, temp)

  # select variables of interest
  adlb_f <- adlb[position_satisfy_filters, ]

  # generate subsets for different directionality
  adlb_f_h <- adlb_f[which(adlb_f[[direction_var]] == "H"), ]
  adlb_f_l <- adlb_f[which(adlb_f[[direction_var]] == "L"), ]
  adlb_f_b <- adlb_f[which(adlb_f[[direction_var]] == "B"), ]

  # for labs requiring both high and low, data is duplicated and will be stacked on top of each other
  adlb_f_b_h <- adlb_f_b
  adlb_f_b_l <- adlb_f_b

  # extract data with worst lab
  if (!is.null(worst_flag_high) && !is.null(worst_flag_low)) {
    # change H to High, L to Low
    adlb_f_h[[direction_var]] <- rep("High", nrow(adlb_f_h))
    adlb_f_l[[direction_var]] <- rep("Low", nrow(adlb_f_l))

    # change, B to High and Low
    adlb_f_b_h[[direction_var]] <- rep("High", nrow(adlb_f_b_h))
    adlb_f_b_l[[direction_var]] <- rep("Low", nrow(adlb_f_b_l))

    adlb_out_h <- adlb_f_h[which(adlb_f_h[[names(worst_flag_high)]] == worst_flag_high), ]
    adlb_out_b_h <- adlb_f_b_h[which(adlb_f_b_h[[names(worst_flag_high)]] == worst_flag_high), ]
    adlb_out_l <- adlb_f_l[which(adlb_f_l[[names(worst_flag_low)]] == worst_flag_low), ]
    adlb_out_b_l <- adlb_f_b_l[which(adlb_f_b_l[[names(worst_flag_low)]] == worst_flag_low), ]

    out <- rbind(adlb_out_h, adlb_out_b_h, adlb_out_l, adlb_out_b_l)
  } else if (!is.null(worst_flag_high)) {
    adlb_f_h[[direction_var]] <- rep("High", nrow(adlb_f_h))
    adlb_f_b_h[[direction_var]] <- rep("High", nrow(adlb_f_b_h))

    adlb_out_h <- adlb_f_h[which(adlb_f_h[[names(worst_flag_high)]] == worst_flag_high), ]
    adlb_out_b_h <- adlb_f_b_h[which(adlb_f_b_h[[names(worst_flag_high)]] == worst_flag_high), ]

    out <- rbind(adlb_out_h, adlb_out_b_h)
  } else if (!is.null(worst_flag_low)) {
    adlb_f_l[[direction_var]] <- rep("Low", nrow(adlb_f_l))
    adlb_f_b_l[[direction_var]] <- rep("Low", nrow(adlb_f_b_l))

    adlb_out_l <- adlb_f_l[which(adlb_f_l[[names(worst_flag_low)]] == worst_flag_low), ]
    adlb_out_b_l <- adlb_f_b_l[which(adlb_f_b_l[[names(worst_flag_low)]] == worst_flag_low), ]

    out <- rbind(adlb_out_l, adlb_out_b_l)
  }

  # label
  formatters::var_labels(out) <- formatters::var_labels(adlb_f, fill = FALSE)
  # NA
  out
}

#' Helper function to analyze patients for `s_count_abnormal_lab_worsen_by_baseline()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to count the number of patients and the fraction of patients according to
#' highest post-baseline lab grade variable `.var`, baseline lab grade variable `baseline_var`,
#' and the direction of interest specified in `direction_var`.
#'
#' @inheritParams argument_convention
#' @inheritParams h_adlb_worsen
#' @param baseline_var (`string`)\cr name of the baseline lab grade variable.
#'
#' @return The counts and fraction of patients
#'   whose worst post-baseline lab grades are worse than their baseline grades, for
#'   post-baseline worst grades "1", "2", "3", "4" and "Any".
#'
#' @seealso [abnormal_lab_worsen_by_baseline]
#'
#' @examples
#' library(dplyr)
#'
#' # The direction variable, GRADDR, is based on metadata
#' adlb <- tern_ex_adlb %>%
#'   mutate(
#'     GRADDR = case_when(
#'       PARAMCD == "ALT" ~ "B",
#'       PARAMCD == "CRP" ~ "L",
#'       PARAMCD == "IGA" ~ "H"
#'     )
#'   ) %>%
#'   filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")
#'
#' df <- h_adlb_worsen(
#'   adlb,
#'   worst_flag_low = c("WGRLOFL" = "Y"),
#'   worst_flag_high = c("WGRHIFL" = "Y"),
#'   direction_var = "GRADDR"
#' )
#'
#' # `h_worsen_counter`
#' h_worsen_counter(
#'   df %>% filter(PARAMCD == "CRP" & GRADDR == "Low"),
#'   id = "USUBJID",
#'   .var = "ATOXGR",
#'   baseline_var = "BTOXGR",
#'   direction_var = "GRADDR"
#' )
#'
#' @export
h_worsen_counter <- function(df, id, .var, baseline_var, direction_var) {
  checkmate::assert_string(id)
  checkmate::assert_string(.var)
  checkmate::assert_string(baseline_var)
  checkmate::assert_scalar(unique(df[[direction_var]]))
  checkmate::assert_subset(unique(df[[direction_var]]), c("High", "Low"))
  assert_df_with_variables(df, list(val = c(id, .var, baseline_var, direction_var)))

  # remove post-baseline missing
  df <- df[df[[.var]] != "<Missing>", ]

  # obtain directionality
  direction <- unique(df[[direction_var]])

  if (direction == "Low") {
    grade <- -1:-4
    worst_grade <- -4
  } else if (direction == "High") {
    grade <- 1:4
    worst_grade <- 4
  }

  if (nrow(df) > 0) {
    by_grade <- lapply(grade, function(i) {
      # filter baseline values that is less than i or <Missing>
      df_temp <- df[df[[baseline_var]] %in% c((i + sign(i) * -1):(-1 * worst_grade), "<Missing>"), ]
      # num: number of patients with post-baseline worst lab equal to i
      num <- length(unique(df_temp[df_temp[[.var]] %in% i, id, drop = TRUE]))
      # denom: number of patients with baseline values less than i or <missing> and post-baseline in the same direction
      denom <- length(unique(df_temp[[id]]))
      rm(df_temp)
      c(num = num, denom = denom)
    })
  } else {
    by_grade <- lapply(1, function(i) {
      c(num = 0, denom = 0)
    })
  }

  names(by_grade) <- as.character(seq_along(by_grade))

  # baseline grade less 4 or missing
  df_temp <- df[!df[[baseline_var]] %in% worst_grade, ]

  # denom: number of patients with baseline values less than 4 or <missing> and post-baseline in the same direction
  denom <- length(unique(df_temp[, id, drop = TRUE]))

  # condition 1: missing baseline and in the direction of abnormality
  con1 <- which(df_temp[[baseline_var]] == "<Missing>" & df_temp[[.var]] %in% grade)
  df_temp_nm <- df_temp[which(df_temp[[baseline_var]] != "<Missing>" & df_temp[[.var]] %in% grade), ]

  # condition 2: if post-baseline values are present then post-baseline values must be worse than baseline
  if (direction == "Low") {
    con2 <- which(as.numeric(as.character(df_temp_nm[[.var]])) < as.numeric(as.character(df_temp_nm[[baseline_var]])))
  } else {
    con2 <- which(as.numeric(as.character(df_temp_nm[[.var]])) > as.numeric(as.character(df_temp_nm[[baseline_var]])))
  }

  # number of patients satisfy either conditions 1 or 2
  num <- length(unique(df_temp[union(con1, con2), id, drop = TRUE]))

  list(fraction = c(by_grade, list("Any" = c(num = num, denom = denom))))
}

#' @describeIn abnormal_lab_worsen_by_baseline Statistics function for patients whose worst post-baseline
#'   lab grades are worse than their baseline grades.
#'
#' @return
#' * `s_count_abnormal_lab_worsen_by_baseline()` returns the counts and fraction of patients whose worst
#'   post-baseline lab grades are worse than their baseline grades, for post-baseline worst grades
#'   "1", "2", "3", "4" and "Any".
#'
#' @keywords internal
s_count_abnormal_lab_worsen_by_baseline <- function(df, # nolint
                                                    .var = "ATOXGR",
                                                    variables = list(
                                                      id = "USUBJID",
                                                      baseline_var = "BTOXGR",
                                                      direction_var = "GRADDR"
                                                    ),
                                                    ...) {
  checkmate::assert_string(.var)
  checkmate::assert_set_equal(names(variables), c("id", "baseline_var", "direction_var"))
  checkmate::assert_string(variables$id)
  checkmate::assert_string(variables$baseline_var)
  checkmate::assert_string(variables$direction_var)
  assert_df_with_variables(df, c(aval = .var, variables[1:3]))
  assert_list_of_variables(variables)

  h_worsen_counter(df, variables$id, .var, variables$baseline_var, variables$direction_var)
}

#' @describeIn abnormal_lab_worsen_by_baseline Formatted analysis function which is used as `afun`
#'   in `count_abnormal_lab_worsen_by_baseline()`.
#'
#' @return
#' * `a_count_abnormal_lab_worsen_by_baseline()` returns the corresponding list with
#'   formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_abnormal_lab_worsen_by_baseline <- function(df,
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
    default_stat_fnc = s_count_abnormal_lab_worsen_by_baseline,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("abnormal_lab_worsen_by_baseline", stats_in = .stats)
  levels_per_stats <- lapply(x_stats, names)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(.stats, .labels, levels_per_stats#, d_count_abnormal_by_baseline(dots_extra_args$abnormal)
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

#' @describeIn abnormal_lab_worsen_by_baseline Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_abnormal_lab_worsen_by_baseline()` returns a layout object suitable for passing to further layouting
#'   functions, or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted
#'   rows containing the statistics from `s_count_abnormal_lab_worsen_by_baseline()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' # The direction variable, GRADDR, is based on metadata
#' adlb <- tern_ex_adlb %>%
#'   mutate(
#'     GRADDR = case_when(
#'       PARAMCD == "ALT" ~ "B",
#'       PARAMCD == "CRP" ~ "L",
#'       PARAMCD == "IGA" ~ "H"
#'     )
#'   ) %>%
#'   filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")
#'
#' df <- h_adlb_worsen(
#'   adlb,
#'   worst_flag_low = c("WGRLOFL" = "Y"),
#'   worst_flag_high = c("WGRHIFL" = "Y"),
#'   direction_var = "GRADDR"
#' )
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   add_colcounts() %>%
#'   split_rows_by("PARAMCD") %>%
#'   split_rows_by("GRADDR") %>%
#'   count_abnormal_lab_worsen_by_baseline(
#'     var = "ATOXGR",
#'     variables = list(
#'       id = "USUBJID",
#'       baseline_var = "BTOXGR",
#'       direction_var = "GRADDR"
#'     )
#'   ) %>%
#'   append_topleft("Direction of Abnormality") %>%
#'   build_table(df = df, alt_counts_df = tern_ex_adsl)
#'
#' @export
#' @order 2
count_abnormal_lab_worsen_by_baseline <- function(lyt,
                                                  var,
                                                  variables = list(
                                                    id = "USUBJID",
                                                    baseline_var = "BTOXGR",
                                                    direction_var = "GRADDR"
                                                  ),
                                                  na_str = default_na_str(),
                                                  nested = TRUE,
                                                  ...,
                                                  table_names = lifecycle::deprecated(),
                                                  .stats = "fraction",
                                                  .formats = list(fraction = format_fraction),
                                                  .labels = NULL,
                                                  .indent_mods = NULL) {
  checkmate::assert_string(var)

  # Deprecated argument warning
  if (lifecycle::is_present(table_names)) {
    lifecycle::deprecate_warn(
      "0.9.8", "count_abnormal_lab_worsen_by_baseline(table_names)",
      details = "The argument has no effect on the output."
    )
  }

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(extra_args, "variables" = list(variables), ...)

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_abnormal_lab_worsen_by_baseline) <- c(
    formals(a_count_abnormal_lab_worsen_by_baseline), extra_args[[".additional_fun_parameters"]]
  )

  analyze(
    lyt = lyt,
    vars = var,
    afun = a_count_abnormal_lab_worsen_by_baseline,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = "hidden"
  )
}
