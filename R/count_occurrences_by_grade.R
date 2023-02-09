#' Occurrence Counts by Grade
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions for analyzing frequencies and fractions of occurrences by grade for patients
#' with occurrence data. Multiple occurrences within one individual are counted once at the
#' greatest intensity/highest grade level.
#'
#' @inheritParams argument_convention
#' @param grade_groups (named `list` of `character`)\cr containing groupings of grades.
#' @param remove_single (`logical`)\cr `TRUE` to not include the elements of one-element grade groups
#' in the the output list; in this case only the grade groups names will be included in the output.
#'
#' @seealso Relevant helper function [h_append_grade_groups()].
#'
#' @name count_occurrences_by_grade
#'
NULL

#' Helper function for [s_count_occurrences_by_grade()]
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function for [s_count_occurrences_by_grade()] to insert grade groupings into list with
#' individual grade frequencies. The order of the final result follows the order of `grade_groups`.
#' The elements under any-grade group (if any), i.e. the grade group equal to `refs` will be moved to
#' the end. Grade groups names must be unique.
#'
#' @inheritParams count_occurrences_by_grade
#' @param refs (named `list` of `numeric`)\cr where each name corresponds to a reference grade level
#'   and each entry represents a count.
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
h_append_grade_groups <- function(grade_groups, refs, remove_single = TRUE) {
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
  if (remove_single) {
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

#' @describeIn count_occurrences_by_grade Statistics function which given occurrence data counts the
#'  number of patients by highest grade. Returns a list of counts and fractions with one element
#'  per grade level or grade level grouping.
#'
#' @examples
#' library(dplyr)
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
#' df_adsl <- df %>%
#'   select(USUBJID, ARM) %>%
#'   unique()
#'
#' s_count_occurrences_by_grade(
#'   df,
#'   .N_col = 10L, # nolint
#'   .var = "AETOXGR",
#'   id = "USUBJID",
#'   grade_groups = list("ANY" = levels(df$AETOXGR))
#' )
#'
#' @export
s_count_occurrences_by_grade <- function(df,
                                         .var,
                                         .N_col, # nolint
                                         id = "USUBJID",
                                         grade_groups = list(),
                                         remove_single = TRUE,
                                         labelstr = "") {
  assert_valid_factor(df[[.var]])
  assert_df_with_variables(df, list(grade = .var, id = id))

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

    df_max <- stats::aggregate(grade ~ id, FUN = max, drop = FALSE)
    l_count <- as.list(table(df_max$grade))
  }

  if (length(grade_groups) > 0) {
    l_count <- h_append_grade_groups(grade_groups, l_count, remove_single)
  }

  l_count_fraction <- lapply(l_count, function(i, denom) c(i, i / denom), denom = .N_col)

  list(
    count_fraction = l_count_fraction
  )
}

#' @describeIn count_occurrences_by_grade Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' #  We need to ungroup `count_fraction` first so that the `rtables` formatting
#' # function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_occurrences_by_grade, .ungroup_stats = "count_fraction")
#' afun(
#'   df,
#'   .N_col = 10L, # nolint
#'   .var = "AETOXGR",
#'   id = "USUBJID",
#'   grade_groups = list("ANY" = levels(df$AETOXGR))
#' )
a_count_occurrences_by_grade <- make_afun(
  s_count_occurrences_by_grade,
  .formats = c("count_fraction" = format_count_fraction_fixed_dp)
)


#' @describeIn count_occurrences_by_grade Layout creating function which can be used for creating tables,
#'   which can take statistics function arguments and additional format arguments (see below).
#' @param var_labels (`character`)\cr labels to show in the result table.
#' @export
#' @examples
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
#'     grade_groups = grade_groups
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
count_occurrences_by_grade <- function(lyt,
                                       var,
                                       var_labels = var,
                                       show_labels = "default",
                                       riskdiff = NULL,
                                       ...,
                                       table_names = var,
                                       .stats = NULL,
                                       .formats = NULL,
                                       .indent_mods = NULL,
                                       .labels = NULL) {
  afun <- make_afun(
    a_count_occurrences_by_grade,
    .stats = .stats,
    .formats = .formats,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )

  afun_riskdiff <- function(df, .var, .N_col, .spl_context) { # nolint
    checkmate::assert_names(names(riskdiff), permutation.of = c("arm_x", "arm_y"))
    n_spl <- length(.spl_context$split)
    n_riskdiff_col <- sum(.spl_context[[riskdiff$arm_x]][[n_spl]], .spl_context[[riskdiff$arm_y]][[n_spl]])
    if (.spl_context$cur_col_n[n_spl] == n_riskdiff_col) {
      N_col_x <- round(.N_col / 2) # fix value after rtables#517 # nolint
      N_col_y <- round(.N_col / 2) # fix value after rtables#517 # nolint
      s_x <- s_count_occurrences_by_grade(df[df$ARM == riskdiff$arm_x, ], .var, N_col_x, ...)
      s_y <- s_count_occurrences_by_grade(df[df$ARM == riskdiff$arm_y, ], .var, N_col_y, ...)
      rd_ci <- stat_propdiff_ci(
        lapply(s_x["count_fraction"]$count_fraction, `[`, 2), lapply(s_y["count_fraction"]$count_fraction, `[`, 2),
        N_col_x, N_col_y, names(s_x$count_fraction)
      )
      in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
    } else {
      afun(df = df, .var = .var, .N_col = .N_col, ...)
    }
  }

  analyze(
    lyt = lyt,
    vars = var,
    var_labels = var_labels,
    show_labels = show_labels,
    afun = ifelse(is.null(riskdiff), afun, afun_riskdiff),
    table_names = table_names,
    extra_args = list(...)
  )
}

#' @describeIn count_occurrences_by_grade Layout creating function which adds content rows using the
#'   statistics function and additional format arguments (see below).
#' @export
#' @examples
#'
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
summarize_occurrences_by_grade <- function(lyt,
                                           var,
                                           ...,
                                           .stats = NULL,
                                           .formats = NULL,
                                           .indent_mods = NULL,
                                           .labels = NULL) {
  cfun <- make_afun(
    a_count_occurrences_by_grade,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = cfun,
    extra_args = list(...)
  )
}
