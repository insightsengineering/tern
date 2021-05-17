#' Count patients with marked laboratory abnormalities
#'
#' Primary analysis variable `.var` indicates whether single, replicated
#' or last marked laboratory abnormality was observed (factor).
#' Additional analysis variables are `id` (character or factor) and `direction` indicating
#' the direction of the abnormality (factor).
#' Denominator is number of patients with at least one valid measurement during
#' treatment (post-baseline), and patients in the numerator are considered as follows:
#' * For `Single, not last` and `Last or replicated`: Numerator is number of patients
#'   with `Single, not last` and `Last or replicated` levels, respectively.
#' * For `Any`: Numerator is the number of patients with either single or
#'   replicated marked abnormalities.
#'
#' @details Note that `Single, not last` and `Last or replicated` levels are
#' mutually exclusive. If a patient has abnormalities that meet both the `Single, not last`
#' and `Last or replicated` criteria, then the patient will be counted only under the `Last or replicated` category.

#' @inheritParams argument_convention
#' @param abnormal (named `string` or `character`)\cr identifying the abnormal direction in `var`.
#'
#' @name abnormal_by_marked
#'
NULL
#' @describeIn abnormal_by_marked Statistics function which returns
#' the counts and fractions of patients with `Single, not last`, `Last or replicated` and `Any`
#' marked laboratory abnormalities for a single `abnormal` level.
#' @return [s_count_abnormal_by_marked()] the single statistic `count_fraction`
#' with `Single, not last`, `Last or replicated` and `Any` results.
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)),
#'   ARMCD = factor(c(rep("ARM A", 5),rep("ARM B", 5))),
#'   ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH HIGH","LOW LOW",
#'   "LOW", "LOW", "HIGH HIGH", "LOW", "HIGH HIGH")),
#'   ONTRTFL = c("", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
#'   PARAMCD = factor(rep("CRP", 10)),
#'   AVALCAT1 = factor(c("", "", "", "SINGLE", "REPLICATED", "", "", "LAST", "", "SINGLE")),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Select only post-baseline records.
#' df <- df %>% filter(ONTRTFL == "Y")
#'  s_count_abnormal_by_marked(df = df,
#'  .var = "AVALCAT1",
#'  abnormal = c(High = "HIGH HIGH"),
#'  variables = list(id = "USUBJID", direction = "ANRIND")
#' )
#'
#'  s_count_abnormal_by_marked(df = df,
#'  .var = "AVALCAT1",
#'  abnormal = c(Low = "LOW LOW"),
#'  variables = list(id = "USUBJID", direction = "ANRIND")
#' )
s_count_abnormal_by_marked <- function(df,
                                       .var = "AVALCAT1",
                                       abnormal = c(Low = "LOW LOW"),
                                       variables = list(id = "USUBJID", direction = "ANRIND")) {
  assert_that(
    is.string(.var),
    is.list(variables),
    all(names(variables) %in% c("id", "direction")),
    is_df_with_variables(df, c(aval = .var, variables)),
    is_character_or_factor(df[[.var]]),
    is_character_or_factor(df[[variables$id]]),
    all(abnormal %in% c("LOW LOW", "HIGH HIGH"))
  )

  # Patients in the denominator have at least one post-baseline visit.
  subjects_post_any <- unique(df[[variables$id]])
  denom <- length(subjects_post_any)
  if (denom > 0) {
    # For numerators, select records based on direction.
    df_abn <- df[df[[variables$direction]] == abnormal, ]
    subjects_last_replicated <- unique(df_abn[df_abn[[.var]] %in% c("LAST", "REPLICATED"), variables$id, drop = TRUE])
    subjects_single <- unique(df_abn[df_abn[[.var]] == "SINGLE", variables$id, drop = TRUE])
    # Subjects who have both single and last/replicated abnormalities are counted in only the last/replicated group.
    subjects_single <- setdiff(subjects_single, subjects_last_replicated)
    n_single <- length(subjects_single)
    n_last_replicated <- length(subjects_last_replicated)
    n_any <- n_single + n_last_replicated
    result <- list(count_fraction = list(
      "Single, not last" = c(n_single, n_single / denom),
      "Last or replicated" = c(n_last_replicated, n_last_replicated / denom),
      "Any Abnormality" = c(n_any, n_any / denom)
    ))
  } else if (denom == 0) {
    result <- list(count_fraction = list(
      "Single, not last" = c(0, 0),
      "Last or replicated" = c(0, 0),
      "Any Abnormality" = c(0, 0)
    ))
  }
  result
}

#' @describeIn abnormal_by_marked Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal_by_marked()] returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @examples
#' # Use the Formatted Analysis function for `analyze()`. We need to ungroup `count_fraction` first
#' # so that the rtables formatting function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_abnormal_by_marked, .ungroup_stats = "count_fraction")
#' afun(
#'   df = df,
#'   .var = "AVALCAT1",
#'   abnormal = c(High = "HIGH HIGH"),
#'   variables = list(id = "USUBJID", direction = "ANRIND")
#' )

a_count_abnormal_by_marked <- make_afun(
  s_count_abnormal_by_marked,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_marked Layout creating function which can be used for creating tables,
#' which can take statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#' basic_table() %>%
#' split_cols_by("ARMCD") %>%
#' summarize_num_patients(var = "USUBJID",
#'                        .stats = "unique_count") %>%
#' count_abnormal_by_marked(var = "AVALCAT1" ,
#'                          abnormal = c(Low = "LOW LOW", High = "HIGH HIGH"),
#'                          variables = list(id = "USUBJID", direction = "ANRIND")) %>%
#'                          build_table(df = df)
#'
count_abnormal_by_marked <- function(lyt,
                                     var,
                                     abnormal,
                                     variables,
                                     ...,
                                     table_names = abnormal,
                                     .stats = NULL,
                                     .formats = NULL,
                                     .labels = NULL,
                                     .indent_mods = NULL) {
  assert_that(
    is.string(var),
    !is.null(names(abnormal)),
    is_equal_length(abnormal, table_names)
  )
  afun <- make_afun(
    a_count_abnormal_by_marked,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )
  for (i in seq_along(abnormal)) {
    abn <- abnormal[i]
    varlist <- variables
    lyt <- analyze(
      lyt = lyt,
      vars = var,
      var_labels = names(abn),
      table_names = table_names[i],
      afun = afun,
      extra_args = c(list(abnormal = abn, variables = varlist, list(...))),
      show_labels = "visible"
    )
  }
  lyt
}
