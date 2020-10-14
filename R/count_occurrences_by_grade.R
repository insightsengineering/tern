#' Occurrence Counts by Grade
#'
#' Functions for analyzing frequencies and percentages of occurrences by grade for patients
#' with occurrence data. Multiple occurrences within one individual are counted once at the
#' greatest intensity/highest grade level.
#'
#' @inheritParams argument_convention
#' @param grade_groups (named `list` of `character`)\cr containing groupings of grades.
#' @param refs (named `list` of `numeric`)\cr where each name corresponds to a reference grade level
#'   and each entry represents a count.
#' @template formatting_arguments
#' @name count_occurrences_by_grade
#'
NULL

#' @describeIn count_occurrences_by_grade  Helper function for [s_count_occurrences_by_grade()] to
#'   insert grade groupings into list with individual grade frequencies.
#'
#' @export
#' @examples
#' h_append_grade_groups(
#'   list(
#'     "Any Grade" = as.character(1:3),
#'      "Grade 1-2" = c("1", "2")
#'     ),
#'   list("1" = 10, "2" = 7, "3" = 2)
#'  )
#'
h_append_grade_groups <- function(grade_groups, refs) {

  assert_that(
    is.list(grade_groups),
    is.list(refs)
  )
  elements <- unique(unlist(grade_groups))

  assert_that(
    all_elements_in_ref(elements, names(refs))
  )

  result <- refs

  for (i in seq_along(grade_groups)){

    l_count_select <- result[grade_groups[[i]]]
    frq <- do.call("sum", l_count_select)
    index <- min(vapply(names(l_count_select), grep, x = names(result), numeric(1)))

    result <- append(result, frq, after = index - 1)
    names(result)[index] <- names(grade_groups)[i]
  }

  result
}

#' @describeIn count_occurrences_by_grade Statistics function which given occurrence data counts the
#'  number of patients by highest grade. Returns a list of counts and percentages with one element
#'  per grade level or grade level grouping.
#' @importFrom stats aggregate
#' @export
#' @examples
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1:5, 1)),
#'   ARM = factor(c("A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
#'   AETOXGR = factor(c(1, 2, 3, 1, 2, 3), levels = c(1:5)),
#'   stringsAsFactors = FALSE
#' )
#'
#' s_count_occurrences_by_grade(df,
#'                     .N_col = 10L, # nolint
#'                     .var = "AETOXGR",
#'                     id = "USUBJID",
#'                     grade_groups = list("ANY" = levels(df$AETOXGR)))
#'
s_count_occurrences_by_grade <- function(df,
                                         .var,
                                         .N_col, #nolint
                                         id = "USUBJID",
                                         grade_groups = list(),
                                         labelstr = "") {

  assert_that(
    is_df_with_variables(df, list(grade = .var, id = id)),
    is_valid_factor(df[[.var]]),
    is.count(.N_col),
    noNA(df[[id]]),
    is_valid_character(df[[id]]) || is_valid_factor(df[[id]])
  )

  id <- df[[id]]
  grade <- df[[.var]]

  if (!is.ordered(grade)) {

    grade_lbl <- label(grade)
    grade <- with_label(factor(grade, levels = levels(grade), ordered = TRUE), grade_lbl)

  }

  df_max <- aggregate(grade ~ id, FUN = max, drop = FALSE)
  l_count <- as.list(table(df_max$grade))

  if (length(grade_groups) > 0) {
    l_count <- h_append_grade_groups(grade_groups, l_count)
  }

  l_count_percent <- lapply(l_count, function(i, denom) c(i, i / denom), denom = .N_col)

  list(
    count_percent = l_count_percent
  )

}
