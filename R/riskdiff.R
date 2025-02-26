#' Split function to configure risk difference column
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Wrapper function for [rtables::add_combo_levels()] which configures settings for the risk difference
#' column to be added to an `rtables` object. To add a risk difference column to a table, this function
#' should be used as `split_fun` in calls to [rtables::split_cols_by()], followed by setting argument
#' `riskdiff` to `TRUE` in all following analyze function calls.
#'
#' @param arm_x (`string`)\cr name of reference arm to use in risk difference calculations.
#' @param arm_y (`character`)\cr names of one or more arms to compare to reference arm in risk difference
#'   calculations. A new column will be added for each value of `arm_y`.
#' @param col_label (`character`)\cr labels to use when rendering the risk difference column within the table.
#'   If more than one comparison arm is specified in `arm_y`, default labels will specify which two arms are
#'   being compared (reference arm vs. comparison arm).
#' @param pct (`flag`)\cr whether output should be returned as percentages. Defaults to `TRUE`.
#'
#' @return A closure suitable for use as a split function (`split_fun`) within [rtables::split_cols_by()]
#'   when creating a table layout.
#'
#' @seealso [stat_propdiff_ci()] for details on risk difference calculation.
#'
#' @examples
#' adae <- tern_ex_adae
#' adae$AESEV <- factor(adae$AESEV)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", split_fun = add_riskdiff(arm_x = "ARM A", arm_y = c("ARM B", "ARM C"))) %>%
#'   count_occurrences_by_grade(
#'     var = "AESEV",
#'     riskdiff = TRUE
#'   )
#'
#' tbl <- build_table(lyt, df = adae)
#' tbl
#'
#' @export
add_riskdiff <- function(arm_x,
                         arm_y,
                         col_label = paste0(
                           "Risk Difference (%) (95% CI)", if (length(arm_y) > 1) paste0("\n", arm_x, " vs. ", arm_y)
                         ),
                         pct = TRUE) {
  checkmate::assert_character(arm_x, len = 1)
  checkmate::assert_character(arm_y, min.len = 1)
  checkmate::assert_character(col_label, len = length(arm_y))

  combodf <- tibble::tribble(~valname, ~label, ~levelcombo, ~exargs)
  for (i in seq_len(length(arm_y))) {
    combodf <- rbind(
      combodf,
      tibble::tribble(
        ~valname, ~label, ~levelcombo, ~exargs,
        paste("riskdiff", arm_x, arm_y[i], sep = "_"), col_label[i], c(arm_x, arm_y[i]), list()
      )
    )
  }
  if (pct) combodf$valname <- paste0(combodf$valname, "_pct")
  add_combo_levels(combodf)
}

#' Analysis function to calculate risk difference column values
#'
#' In the risk difference column, this function uses the statistics function associated with `afun` to
#' calculates risk difference values from arm X (reference group) and arm Y. These arms are specified
#' when configuring the risk difference column which is done using the [add_riskdiff()] split function in
#' the previous call to [rtables::split_cols_by()]. For all other columns, applies `afun` as usual. This
#' function utilizes the [stat_propdiff_ci()] function to perform risk difference calculations.
#'
#' @inheritParams argument_convention
#' @param afun (named `list`)\cr a named list containing one name-value pair where the name corresponds to
#'   the name of the statistics function that should be used in calculations and the value is the corresponding
#'   analysis function.
#' @param s_args (named `list`)\cr additional arguments to be passed to the statistics function and analysis
#'   function supplied in `afun`.
#'
#' @return A list of formatted [rtables::CellValue()].
#'
#' @seealso
#' * [stat_propdiff_ci()] for details on risk difference calculation.
#' * Split function [add_riskdiff()] which, when used as `split_fun` within [rtables::split_cols_by()] with
#'   `riskdiff` argument set to `TRUE` in subsequent analyze functions calls, adds a risk difference column
#'   to a table layout.
#'
#' @keywords internal
afun_riskdiff <- function(df,
                          labelstr = "",
                          ...,
                          .stats = NULL,
                          .stat_names = NULL,
                          .formats = NULL,
                          .labels = NULL,
                          .indent_mods = NULL) {
  dots_extra_args <- list(...)

  # Check if there are user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$default_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Adding automatically extra parameters to the statistic function (see ?rtables::additional_fun_params)
  extra_afun_params <- retrieve_extra_afun_params(
    names(dots_extra_args$.additional_fun_parameters)
  )
  dots_extra_args$.additional_fun_parameters <- NULL # After extraction we do not need them anymore

  .spl_context <- extra_afun_params[[".spl_context"]]
  .N_col <- extra_afun_params[[".N_col"]]
  .all_col_counts <- extra_afun_params[[".all_col_counts"]]

  # Checking if the user has set up the levels to use in risk difference calculations
  if (!any(grepl("riskdiff", names(.spl_context)))) {
    stop(
      "Please set up levels to use in risk difference calculations using the `add_riskdiff` ",
      "split function within `split_cols_by`. See ?add_riskdiff for details."
    )
  }

  # Is this a summary content row? (label row with data summary)
  isc <- isTRUE(dots_extra_args$is_summary_content)
  args_list <- c(
    if(isc) {
      list(df = df)
    } else {
      list(x = df[[extra_afun_params$.var]])
    },
    extra_afun_params,
    dots_extra_args
  )

  dots_extra_args[["denom"]] <- NULL

  cur_split <- tail(.spl_context$cur_col_split_val[[1]], 1)
  if (!grepl("^riskdiff", cur_split)) {
    # Apply basic afun (no risk difference) in all other columns
    x_stats <- .apply_stat_functions(
      default_stat_fnc = if (isc) {
        s_num_patients_content
      } else {
        s_num_patients
      },
      custom_stat_fnc_list = custom_stat_functions,
      args_list = args_list
    )

    # Fill in with stats defaults if needed
    .stats <- c(
      get_stats("summarize_num_patients", stats_in = .stats),
      names(custom_stat_functions)
    )

    out_list <- x_stats[.stats]
  } else {
    arm_x <- strsplit(cur_split, "_")[[1]][2]
    arm_y <- strsplit(cur_split, "_")[[1]][3]
    if (length(.spl_context$cur_col_split[[1]]) > 1) { # Different split name for nested column splits
      arm_spl_x <- gsub("riskdiff", "", paste0(strsplit(.spl_context$cur_col_id[1], "_")[[1]][c(1, 2)], collapse = ""))
      arm_spl_y <- gsub("riskdiff", "", paste0(strsplit(.spl_context$cur_col_id[1], "_")[[1]][c(1, 3)], collapse = ""))
    } else {
      arm_spl_x <- arm_x
      arm_spl_y <- arm_y
    }

    N_col_x <- .all_col_counts[[arm_spl_x]] # nolint
    N_col_y <- .all_col_counts[[arm_spl_y]] # nolint
    cur_var <- tail(.spl_context$cur_col_split[[1]], 1)

    # Apply statistics function to arm X and arm Y data
    args_list[["x"]] <- NULL # It does not matter?
    if (!("df" %in% names(args_list))) {
      args_list <- c(list("df" = NULL), args_list)
    }
    args_list[["df"]] <- df[df[[cur_var]] == arm_x, ]
    extra_afun_params[[".N_col"]] <- N_col_x
    x_stats <- .apply_stat_functions(
      default_stat_fnc = s_num_patients_content, # why content?
      custom_stat_fnc_list = custom_stat_functions,
      args_list = args_list
    )
    extra_afun_params[[".N_col"]] <- N_col_y
    args_list[["df"]] <- df[df[[cur_var]] == arm_y, ]
    y_stats <- .apply_stat_functions(
      default_stat_fnc = s_num_patients_content, # why content?
      custom_stat_fnc_list = custom_stat_functions,
      args_list = args_list
    )

    # Fill in with stats defaults if needed
    .stats <- c(
      get_stats("summarize_num_patients", stats_in = .stats),
      names(custom_stat_functions)
    )

    # Forced types for risk differences
    if (!any(names(x_stats) %in% c("count_fraction", "unique"))) {
      stop("Risk difference calculations are supported only for count_fraction or unique statistics.")
    }
    .stats <- ifelse("count_fraction" %in% names(.stats), "count_fraction", "unique")
    x_stats <- x_stats[.stats]
    y_stats <- y_stats[.stats]
    if ("flag_variables" %in% names(dots_extra_args)) {
      var_nms <- dots_extra_args$flag_variables
    } else if (is.list(x_stats) && !is.null(names(x_stats))) {
      var_nms <- names(x_stats)
    } else {
      var_nms <- ""
      x_stats <- list(x_stats)
      y_stats <- list(y_stats)
    }

    # Calculate risk difference for each row, repeated if multiple statistics in table
    pct <- tail(strsplit(cur_split, "_")[[1]], 1) == "pct"

    x_first_value <- lapply(x_stats, `[`, 1)
    y_second_value <- lapply(y_stats, `[`, 1)
    out_list <- sapply(seq(.stats), function(stat_i) {
      stat_propdiff_ci(
        x_first_value[stat_i], y_second_value[stat_i],
        N_col_x, N_col_y,
        list_names = var_nms,
        pct = pct
      )
    })

    # It feels an imposition but here it is (TO ADD risk_diff_unique, etc)
    .formats  <- lapply(out_list, function(x) "xx.x (xx.x - xx.x)")
    # in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
  }


  # Fill in formats/indents/labels with custom input and defaults
  .formats <- get_formats_from_stats(.stats, .formats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)
  if (anyNA(.labels[names(out_list)])) {
    .labels <- setNames(.labels[names(out_list)], names(out_list))
    attr_labels <- sapply(out_list, attr, "label")
    attr_labels <- attr_labels[nzchar(attr_labels)]
    .labels[names(.labels) %in% names(attr_labels) & is.na(.labels)] <- attr_labels
    .labels <- .labels[!is.na(.labels)]
  }
  .labels <- get_labels_from_stats(.stats, .labels)

  # Auto format handling
  .formats <- apply_auto_formatting(
    .formats,
    out_list,
    extra_afun_params$.df_row,
    extra_afun_params$.var
  )

  # Get and check statistical names from defaults
  .stat_names <- get_stat_names(out_list, .stat_names)

  in_rows(
    .list = out_list,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' Control function for risk difference column
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets a list of parameters to use when generating a risk (proportion) difference column. Used as input to the
#' `riskdiff` parameter of [tabulate_rsp_subgroups()] and [tabulate_survival_subgroups()].
#'
#' @inheritParams add_riskdiff
#' @param format (`string` or `function`)\cr the format label (string) or formatting function to apply to the risk
#'   difference statistic. See the `3d` string options in [formatters::list_valid_format_labels()] for possible format
#'   strings. Defaults to `"xx.x (xx.x - xx.x)"`.
#'
#' @return A `list` of items with names corresponding to the arguments.
#'
#' @seealso [add_riskdiff()], [tabulate_rsp_subgroups()], and [tabulate_survival_subgroups()].
#'
#' @examples
#' control_riskdiff()
#' control_riskdiff(arm_x = "ARM A", arm_y = "ARM B")
#'
#' @export
control_riskdiff <- function(arm_x = NULL,
                             arm_y = NULL,
                             format = "xx.x (xx.x - xx.x)",
                             col_label = "Risk Difference (%) (95% CI)",
                             pct = TRUE) {
  checkmate::assert_character(arm_x, len = 1, null.ok = TRUE)
  checkmate::assert_character(arm_y, min.len = 1, null.ok = TRUE)
  checkmate::assert_character(format, len = 1)
  checkmate::assert_character(col_label)
  checkmate::assert_flag(pct)

  list(arm_x = arm_x, arm_y = arm_y, format = format, col_label = col_label, pct = pct)
}
