#' Occurrence Counts by Grade
#'
#' Functions for analyzing frequencies and fractions of occurrences by grade for patients
#' with occurrence data. Multiple occurrences within one individual are counted once at the
#' greatest intensity/highest grade level.
#'
#' @inheritParams argument_convention
#' @param grade_groups (named `list` of `character`)\cr containing groupings of grades.
#' @param refs (named `list` of `numeric`)\cr where each name corresponds to a reference grade level
#'   and each entry represents a count.
#' @param remove_single (`logical`)\cr `TRUE` to not include the elements of one-element grade groups
#' in the the output list; in this case only the grade groups names will be included in the output.
#'
#' @name count_occurrences_by_grade
#'
NULL

#' @describeIn count_occurrences_by_grade  Helper function for [s_count_occurrences_by_grade()] to
#'   insert grade groupings into list with individual grade frequencies. The order of the final result
#'   follows the order of `grade_groups`. The elements under any-grade group (if any), i.e.
#'   the grade group equal to `refs` will be moved to the end. Grade groups names must be unique.
#' @export
#' @examples
#'
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(1:5),
#'     "Grade 1-2" = c("1", "2"),
#'     "Grade 3-4" = c("3", "4")
#'     ),
#'   list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50),
#' )
#'
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(5:1),
#'     "Grade A" = "5",
#'     "Grade B" = c("4", "3")
#'     ),
#'   list("1" = 10, "2" = 20, "3" = 30, "4" = 40, "5" = 50),
#' )
#'
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(1:5),
#'     "Grade 1-2" = c("1", "2"),
#'     "Grade 3-4" = c("3", "4")
#'     ),
#'   list("1" = 10, "2" = 5, "3" = 0)
#' )
#'
h_append_grade_groups <- function(grade_groups, refs, remove_single = TRUE) {

  assert_that(
    is.list(grade_groups),
    is.list(refs)
  )
  refs_orig <- refs
  elements <- unique(unlist(grade_groups))

  ### compute sums in groups
  grp_sum <- lapply(grade_groups, function(i) do.call(sum, refs[i]))
  if (!all_elements_in_ref(elements, names(refs))) {
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
#' @importFrom stats aggregate
#' @export
#' @examples
#'
#' library(dplyr)
#' df <- data.frame(
#'   USUBJID = as.character(c(1:6, 1)),
#'   ARM = factor(c("A", "A", "A", "B", "B", "B", "A"), levels = c("A", "B")),
#'   AETOXGR = factor(c(1, 2, 3, 4, 1, 2, 3), levels = c(1:5)),
#'   AESEV = factor(
#'     x = c("MILD", "MODERATE", "SEVERE", "MILD",  "MILD", "MODERATE", "SEVERE"),
#'     levels = c("MILD", "MODERATE", "SEVERE")
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' df_adsl <- df %>%
#'   select(USUBJID, ARM) %>%
#'   unique
#'
#' s_count_occurrences_by_grade(
#'   df,
#'   .N_col = 10L, # nolint
#'   .var = "AETOXGR",
#'   id = "USUBJID",
#'   grade_groups = list("ANY" = levels(df$AETOXGR))
#' )
#'
s_count_occurrences_by_grade <- function(df,
                                         .var,
                                         .N_col, #nolint
                                         id = "USUBJID",
                                         grade_groups = list(),
                                         remove_single = TRUE,
                                         labelstr = "") {

  assert_that(
    is_df_with_variables(df, list(grade = .var, id = id)),
    is_valid_factor(df[[.var]])
  )

  if (nrow(df) < 1) {

    grade_levels <- levels(df[[.var]])
    l_count <- as.list(rep(0, length(grade_levels)))
    names(l_count) <- grade_levels

  } else {

    assert_that(
      is_nonnegative_count(.N_col),
      noNA(df[[id]]),
      is_valid_character(df[[id]]) || is_valid_factor(df[[id]])
    )

    id <- df[[id]]
    grade <- df[[.var]]

    if (!is.ordered(grade)) {

      grade_lbl <- obj_label(grade)
      grade <- with_label(factor(grade, levels = levels(grade), ordered = TRUE), grade_lbl)

    }

    df_max <- aggregate(grade ~ id, FUN = max, drop = FALSE)
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
#' #  We need to ungroup `count_fraction` first so that the rtables formatting
#' # function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_occurrences_by_grade, .ungroup_stats = "count_fraction")
#' afun(
#'   df,
#'   .N_col = 10L, # nolint
#'   .var = "AETOXGR",
#'   id = "USUBJID",
#'   grade_groups = list("ANY" = levels(df$AETOXGR))
#' )
#'
a_count_occurrences_by_grade <- make_afun(
  s_count_occurrences_by_grade,
  .formats = c("count_fraction" = format_count_fraction)
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
#' grade_groups <-  list(
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
#'
count_occurrences_by_grade <- function(lyt,
                                       var,
                                       var_labels = var,
                                       show_labels = "default",
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

  analyze(
    lyt = lyt,
    vars = var,
    var_labels = var_labels,
    show_labels = show_labels,
    afun = afun,
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
#'   var = "AESEV",
#'   .formats = c("count_fraction" = "xx.xx (xx.xx%)")) %>%
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
