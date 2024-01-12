#' Split Function to Configure Risk Difference Column
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Wrapper function for [rtables::add_combo_levels()] which configures settings for the risk difference
#' column to be added to an `rtables` object. To add a risk difference column to a table, this function
#' should be used as `split_fun` in calls to [rtables::split_cols_by()], followed by setting argument
#' `riskdiff` to `TRUE` in all following analyze function calls.
#'
#' @param arm_x (`character`)\cr Name of reference arm to use in risk difference calculations.
#' @param arm_y (`character`)\cr Names of one or more arms to compare to reference arm in risk difference
#'   calculations. A new column will be added for each value of `arm_y`.
#' @param col_label (`character`)\cr Labels to use when rendering the risk difference column within the table.
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

#' Analysis Function to Calculate Risk Difference Column Values
#'
#' In the risk difference column, this function uses the statistics function associated with `afun` to
#' calculates risk difference values from arm X (reference group) and arm Y. These arms are specified
#' when configuring the risk difference column which is done using the [add_riskdiff()] split function in
#' the previous call to [rtables::split_cols_by()]. For all other columns, applies `afun` as usual. This
#' function utilizes the [stat_propdiff_ci()] function to perform risk difference calculations.
#'
#' @inheritParams argument_convention
#' @param afun (named `list`)\cr A named list containing one name-value pair where the name corresponds to
#'   the name of the statistics function that should be used in calculations and the value is the corresponding
#'   analysis function.
#' @param s_args (named `list`)\cr Additional arguments to be passed to the statistics function and analysis
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
                          .var,
                          .N_col, # nolint
                          .N_row, # nolint
                          .df_row,
                          .spl_context,
                          .all_col_counts,
                          .stats,
                          .formats = NULL,
                          .labels = NULL,
                          .indent_mods = NULL,
                          na_str = default_na_str(),
                          afun,
                          s_args = list()) {
  if (!any(grepl("riskdiff", names(.spl_context)))) {
    stop(
      "Please set up levels to use in risk difference calculations using the `add_riskdiff` ",
      "split function within `split_cols_by`. See ?add_riskdiff for details."
    )
  }
  checkmate::assert_list(afun, len = 1, types = "function")
  checkmate::assert_named(afun)
  afun_args <- list(
    .var = .var, .df_row = .df_row, .N_row = .N_row, denom = "N_col", labelstr = labelstr,
    .stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na_str = na_str
  )
  afun_args <- afun_args[intersect(names(afun_args), names(as.list(args(afun[[1]]))))]
  if ("denom" %in% names(s_args)) afun_args[["denom"]] <- NULL

  cur_split <- tail(.spl_context$cur_col_split_val[[1]], 1)
  if (!grepl("^riskdiff", cur_split)) {
    # Apply basic afun (no risk difference) in all other columns
    do.call(afun[[1]], args = c(list(df = df, .N_col = .N_col), afun_args, s_args))
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
    s_args <- c(s_args, afun_args[intersect(names(afun_args), names(as.list(args(names(afun)))))])
    s_x <- do.call(names(afun), args = c(list(df = df[df[[cur_var]] == arm_x, ], .N_col = N_col_x), s_args))
    s_y <- do.call(names(afun), args = c(list(df = df[df[[cur_var]] == arm_y, ], .N_col = N_col_y), s_args))

    # Get statistic name and row names
    stat <- ifelse("count_fraction" %in% names(s_x), "count_fraction", "unique")
    if ("flag_variables" %in% names(s_args)) {
      var_nms <- s_args$flag_variables
    } else if (!is.null(names(s_x[[stat]]))) {
      var_nms <- names(s_x[[stat]])
    } else {
      var_nms <- ""
      s_x[[stat]] <- list(s_x[[stat]])
      s_y[[stat]] <- list(s_y[[stat]])
    }

    # Calculate risk difference for each row, repeated if multiple statistics in table
    pct <- tail(strsplit(cur_split, "_")[[1]], 1) == "pct"
    rd_ci <- rep(stat_propdiff_ci(
      lapply(s_x[[stat]], `[`, 1), lapply(s_y[[stat]], `[`, 1),
      N_col_x, N_col_y,
      list_names = var_nms,
      pct = pct
    ), max(1, length(.stats)))

    in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
  }
}
