#' Count occurrences by grade
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_occurrences_by_grade()] creates a layout element to calculate occurrence counts by grade.
#'
#' This function analyzes primary analysis variable `var` which indicates toxicity grades. The `id` variable
#' is used to indicate unique subject identifiers (defaults to `USUBJID`). The user can also supply a list of
#' custom groups of grades to analyze via the `grade_groups` parameter. The `remove_single`  argument will
#' remove single grades from the analysis so that *only* grade groups are analyzed.
#'
#' If there are multiple grades recorded for one patient only the highest grade level is counted.
#'
#' The summarize function [summarize_occurrences_by_grade()] performs the same function as
#' [count_occurrences_by_grade()] except it creates content rows, not data rows, to summarize the current table
#' row/column context and operates on the level of the latest row split or the root of the table if no row splits have
#' occurred.
#'
#' @inheritParams count_occurrences
#' @inheritParams argument_convention
#' @param grade_groups (named `list` of `character`)\cr list containing groupings of grades.
#' @param remove_single (`flag`)\cr `TRUE` to not include the elements of one-element grade groups
#'   in the the output list; in this case only the grade groups names will be included in the output. If
#'   `only_grade_groups` is set to `TRUE` this argument is ignored.
#' @param only_grade_groups (`flag`)\cr whether only the specified grade groups should be
#'   included, with individual grade rows removed (`TRUE`), or all grades and grade groups
#'   should be displayed (`FALSE`).
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_occurrences_by_grade"), type = "sh")``
#'
#' @seealso Relevant helper function [h_append_grade_groups()].
#'
#' @name count_occurrences_by_grade
#' @order 1
NULL

#' Helper function for `s_count_occurrences_by_grade()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function for [s_count_occurrences_by_grade()] to insert grade groupings into list with
#' individual grade frequencies. The order of the final result follows the order of `grade_groups`.
#' The elements under any-grade group (if any), i.e. the grade group equal to `refs` will be moved to
#' the end. Grade groups names must be unique.
#'
#' @inheritParams count_occurrences_by_grade
#' @param refs (named `list` of `numeric`)\cr named list where each name corresponds to a reference grade level
#'   and each entry represents a count.
#'
#' @return Formatted list of grade groupings.
#'
#' @examples
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(1:5),
#'     "Grade 1-2" = c("1", "2"),
#'     "Grade 3-4" = c("3", "4")
#'   ),
#'   list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50)
#' )
#'
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(5:1),
#'     "Grade A" = "5",
#'     "Grade B" = c("4", "3")
#'   ),
#'   list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50)
#' )
#'
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(1:5),
#'     "Grade 1-2" = c("1", "2"),
#'     "Grade 3-4" = c("3", "4")
#'   ),
#'   list("1" = 10, "2" = 5, "3" = 0)
#' )
#'
#' @export
h_append_grade_groups <- function(grade_groups, refs, remove_single = TRUE, only_grade_groups = FALSE) {
  checkmate::assert_list(grade_groups)
  checkmate::assert_list(refs)
  refs_orig <- refs
  elements <- unique(unlist(grade_groups))

  ### compute sums in groups
  grp_sum <- lapply(grade_groups, function(i) do.call(sum, refs[i]))
  if (!checkmate::test_subset(elements, names(refs))) {
    padding_el <- setdiff(elements, names(refs))
    refs[padding_el] <- 0
  }
  result <- c(grp_sum, refs)

  ### order result while keeping grade_groups's ordering
  ordr <- grade_groups

  # elements of any-grade group (if any) will be moved to the end
  is_any <- sapply(grade_groups, setequal, y = names(refs))
  ordr[is_any] <- list(character(0)) # hide elements under any-grade group

  # groups-elements combined sequence
  ordr <- c(lapply(names(ordr), function(g) c(g, ordr[[g]])), recursive = TRUE, use.names = FALSE)
  ordr <- ordr[!duplicated(ordr)]

  # append remaining elements (if any)
  ordr <- union(ordr, unlist(grade_groups[is_any])) # from any-grade group
  ordr <- union(ordr, names(refs)) # from refs

  # remove elements of single-element groups, if any
  if (only_grade_groups) {
    ordr <- intersect(ordr, names(grade_groups))
  } else if (remove_single) {
    is_single <- sapply(grade_groups, length) == 1L
    ordr <- setdiff(ordr, unlist(grade_groups[is_single]))
  }

  # apply the order
  result <- result[ordr]

  # remove groups without any elements in the original refs
  # note: it's OK if groups have 0 value
  keep_grp <- vapply(grade_groups, function(x, rf) {
    any(x %in% rf)
  }, rf = names(refs_orig), logical(1))

  keep_el <- names(result) %in% names(refs_orig) | names(result) %in% names(keep_grp)[keep_grp]
  result <- result[keep_el]

  result
}

#' @describeIn count_occurrences_by_grade Statistics function which counts the
#'  number of patients by highest grade.
#'
#' @return
#' * `s_count_occurrences_by_grade()` returns a list of counts and fractions with one element per grade level or
#'   grade level grouping.
#'
#' @examples
#' s_count_occurrences_by_grade(
#'   df,
#'   .N_col = 10L,
#'   .var = "AETOXGR",
#'   id = "USUBJID",
#'   grade_groups = list("ANY" = levels(df$AETOXGR))
#' )
#'
#' @export
s_count_occurrences_by_grade <- function(df,
                                         labelstr = "",
                                         .var,
                                         .N_row, # nolint
                                         .N_col, # nolint
                                         ...,
                                         id = "USUBJID",
                                         grade_groups = list(),
                                         remove_single = TRUE,
                                         only_grade_groups = FALSE,
                                         denom = c("N_col", "n", "N_row")) {
  assert_valid_factor(df[[.var]])
  assert_df_with_variables(df, list(grade = .var, id = id))

  denom <- match.arg(denom) %>%
    switch(
      n = nlevels(factor(df[[id]])),
      N_row = .N_row,
      N_col = .N_col
    )

  if (nrow(df) < 1) {
    grade_levels <- levels(df[[.var]])
    l_count <- as.list(rep(0, length(grade_levels)))
    names(l_count) <- grade_levels
  } else {
    if (isTRUE(is.factor(df[[id]]))) {
      assert_valid_factor(df[[id]], any.missing = FALSE)
    } else {
      checkmate::assert_character(df[[id]], min.chars = 1, any.missing = FALSE)
    }
    checkmate::assert_count(.N_col)

    id <- df[[id]]
    grade <- df[[.var]]

    if (!is.ordered(grade)) {
      grade_lbl <- obj_label(grade)
      lvls <- levels(grade)
      if (sum(grepl("^\\d+$", lvls)) %in% c(0, length(lvls))) {
        lvl_ord <- lvls
      } else {
        lvls[!grepl("^\\d+$", lvls)] <- min(as.numeric(lvls[grepl("^\\d+$", lvls)])) - 1
        lvl_ord <- levels(grade)[order(as.numeric(lvls))]
      }
      grade <- formatters::with_label(factor(grade, levels = lvl_ord, ordered = TRUE), grade_lbl)
    }

    missing_lvl <- grepl("missing", tolower(levels(grade)))
    if (any(missing_lvl)) {
      grade <- factor(
        grade,
        levels = c(levels(grade)[!missing_lvl], levels(grade)[missing_lvl]),
        ordered = is.ordered(grade)
      )
    }
    df_max <- stats::aggregate(grade ~ id, FUN = max, drop = FALSE)
    l_count <- as.list(table(df_max$grade))
  }

  if (length(grade_groups) > 0) {
    l_count <- h_append_grade_groups(grade_groups, l_count, remove_single, only_grade_groups)
  }

  l_count_fraction <- lapply(
    l_count,
    function(i, denom) {
      if (i == 0 && denom == 0) {
        c(0, 0)
      } else {
        c(i, i / denom)
      }
    },
    denom = denom
  )

  list(
    count_fraction = l_count_fraction,
    count_fraction_fixed_dp = l_count_fraction
  )
}

#' @describeIn count_occurrences_by_grade Formatted analysis function which is used as `afun`
#'   in `count_occurrences_by_grade()`.
#'
#' @return
#' * `a_count_occurrences_by_grade()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_count_occurrences_by_grade(
#'   df,
#'   .N_col = 10L,
#'   .N_row = 10L,
#'   .var = "AETOXGR",
#'   id = "USUBJID",
#'   grade_groups = list("ANY" = levels(df$AETOXGR))
#' )
#'
#' @export
a_count_occurrences_by_grade <- function(df,
                                         labelstr = "",
                                         ...,
                                         .stats = NULL,
                                         .stat_names = NULL,
                                         .formats = NULL,
                                         .labels = NULL,
                                         .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  # Check for user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_occurrences_by_grade,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      labelstr = list(labelstr),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("count_occurrences_by_grade", stats_in = .stats, custom_stats_in = names(custom_stat_functions))
  x_stats <- x_stats[.stats]
  levels_per_stats <- lapply(x_stats, names)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(.stats, .labels, levels_per_stats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats %>% .unlist_keep_nulls(),
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn count_occurrences_by_grade Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_occurrences_by_grade()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_occurrences_by_grade()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1:6, 1)),
#'   ARM = factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B")),
#'   AETOXGR = factor(c(1, 2, 3, 4, 1, 2, 3), levels = c(1:5)),
#'   AESEV = factor(
#'     x = c("MILD", "MODERATE", "SEVERE", "MILD", "MILD", "MODERATE", "SEVERE"),
#'     levels = c("MILD", "MODERATE", "SEVERE")
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_adsl <- df %>%
#'   select(USUBJID, ARM) %>%
#'   unique()
#'
#' # Layout creating function with custom format.
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_occurrences_by_grade(
#'     var = "AESEV",
#'     .formats = c("count_fraction" = "xx.xx (xx.xx%)")
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
#'
#' # Define additional grade groupings.
#' grade_groups <- list(
#'   "-Any-" = c("1", "2", "3", "4", "5"),
#'   "Grade 1-2" = c("1", "2"),
#'   "Grade 3-5" = c("3", "4", "5")
#' )
#'
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_occurrences_by_grade(
#'     var = "AETOXGR",
#'     grade_groups = grade_groups,
#'     only_grade_groups = TRUE
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
#'
#' @export
#' @order 2
count_occurrences_by_grade <- function(lyt,
                                       var,
                                       id = "USUBJID",
                                       grade_groups = list(),
                                       remove_single = TRUE,
                                       only_grade_groups = FALSE,
                                       var_labels = var,
                                       show_labels = "default",
                                       riskdiff = FALSE,
                                       na_str = default_na_str(),
                                       nested = TRUE,
                                       ...,
                                       table_names = var,
                                       .stats = "count_fraction",
                                       .stat_names = NULL,
                                       .formats = list(count_fraction = format_count_fraction_fixed_dp),
                                       .indent_mods = NULL,
                                       .labels = NULL) {
  checkmate::assert_flag(riskdiff)
  afun <- if (isFALSE(riskdiff)) a_count_occurrences_by_grade else afun_riskdiff

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    id = id, grade_groups = list(grade_groups), remove_single = remove_single, only_grade_groups = only_grade_groups,
    if (!isFALSE(riskdiff)) list(afun = list("s_count_occurrences_by_grade" = a_count_occurrences_by_grade)),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(afun) <- c(formals(afun), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}

#' @describeIn count_occurrences_by_grade Layout-creating function which can take content function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @return
#' * `summarize_occurrences_by_grade()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted content rows
#'   containing the statistics from `s_count_occurrences_by_grade()` to the table layout.
#'
#' @examples
#' # Layout creating function with custom format.
#' basic_table() %>%
#'   add_colcounts() %>%
#'   split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
#'   summarize_occurrences_by_grade(
#'     var = "AESEV",
#'     .formats = c("count_fraction" = "xx.xx (xx.xx%)")
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
#'
#' basic_table() %>%
#'   add_colcounts() %>%
#'   split_rows_by("ARM", child_labels = "visible", nested = TRUE) %>%
#'   summarize_occurrences_by_grade(
#'     var = "AETOXGR",
#'     grade_groups = grade_groups
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
#'
#' @export
#' @order 3
summarize_occurrences_by_grade <- function(lyt,
                                           var,
                                           id = "USUBJID",
                                           grade_groups = list(),
                                           remove_single = TRUE,
                                           only_grade_groups = FALSE,
                                           riskdiff = FALSE,
                                           na_str = default_na_str(),
                                           ...,
                                           .stats = "count_fraction",
                                           .stat_names = NULL,
                                           .formats = list(count_fraction = format_count_fraction_fixed_dp),
                                           .indent_mods = NULL,
                                           .labels = NULL) {
  checkmate::assert_flag(riskdiff)
  afun <- if (isFALSE(riskdiff)) a_count_occurrences_by_grade else afun_riskdiff

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    id = id, grade_groups = list(grade_groups), remove_single = remove_single, only_grade_groups = only_grade_groups,
    if (!isFALSE(riskdiff)) list(afun = list("s_count_occurrences_by_grade" = a_count_occurrences_by_grade)),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(afun) <- c(formals(afun), extra_args[[".additional_fun_parameters"]])

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = afun,
    na_str = na_str,
    extra_args = extra_args
  )
}
