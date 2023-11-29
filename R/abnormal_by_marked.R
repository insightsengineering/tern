#' Count Patients with Marked Laboratory Abnormalities
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Primary analysis variable `.var` indicates whether single, replicated or last marked laboratory
#' abnormality was observed (`factor`). Additional analysis variables are `id` (`character` or `factor`)
#' and `direction` (`factor`) indicating the direction of the abnormality. Denominator is number of
#' patients with at least one valid measurement during the analysis.
#'   * For `Single, not last` and `Last or replicated`: Numerator is number of patients
#'     with `Single, not last` and `Last or replicated` levels, respectively.
#'   * For `Any`: Numerator is the number of patients with either single or
#'     replicated marked abnormalities.
#'
#' @inheritParams argument_convention
#' @param category (`list`)\cr with different marked category names for single
#'   and last or replicated.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("abnormal_by_marked")`
#'   to see available statistics for this function.
#'
#' @note `Single, not last` and `Last or replicated` levels are mutually exclusive. If a patient has
#'   abnormalities that meet both the `Single, not last` and `Last or replicated` criteria, then the
#'   patient will be counted only under the `Last or replicated` category.
#'
#' @name abnormal_by_marked
#' @order 1
NULL

#' @describeIn abnormal_by_marked Statistics function for patients with marked lab abnormalities.
#'
#' @return
#' * `s_count_abnormal_by_marked()` returns statistic `count_fraction` with `Single, not last`,
#'   `Last or replicated`, and `Any` results.
#'
#' @keywords internal
s_count_abnormal_by_marked <- function(df,
                                       .var = "AVALCAT1",
                                       .spl_context,
                                       category = list(single = "SINGLE", last_replicated = c("LAST", "REPLICATED")),
                                       variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir")) {
  checkmate::assert_string(.var)
  checkmate::assert_list(variables)
  checkmate::assert_list(category)
  checkmate::assert_subset(names(category), c("single", "last_replicated"))
  checkmate::assert_subset(names(variables), c("id", "param", "direction"))
  checkmate::assert_vector(unique(df[[variables$direction]]), max.len = 1)

  assert_df_with_variables(df, c(aval = .var, variables))
  checkmate::assert_multi_class(df[[.var]], classes = c("factor", "character"))
  checkmate::assert_multi_class(df[[variables$id]], classes = c("factor", "character"))


  first_row <- .spl_context[.spl_context$split == variables[["param"]], ]
  # Patients in the denominator have at least one post-baseline visit.
  subj <- first_row$full_parent_df[[1]][[variables[["id"]]]]
  subj_cur_col <- subj[first_row$cur_col_subset[[1]]]
  # Some subjects may have a record for high and low directions but
  # should be counted only once.
  denom <- length(unique(subj_cur_col))

  if (denom != 0) {
    subjects_last_replicated <- unique(
      df[df[[.var]] %in% category[["last_replicated"]], variables$id, drop = TRUE]
    )
    subjects_single <- unique(
      df[df[[.var]] %in% category[["single"]], variables$id, drop = TRUE]
    )
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
  } else {
    result <- list(count_fraction = list(
      "Single, not last" = c(0, 0),
      "Last or replicated" = c(0, 0),
      "Any Abnormality" = c(0, 0)
    ))
  }

  result
}

#' @describeIn abnormal_by_marked Formatted analysis function which is used as `afun`
#'   in `count_abnormal_by_marked()`.
#'
#' @return
#' * `a_count_abnormal_by_marked()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_abnormal_by_marked <- make_afun(
  s_count_abnormal_by_marked,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_marked Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_abnormal_by_marked()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_abnormal_by_marked()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(rep(1, 5), rep(2, 5), rep(1, 5), rep(2, 5))),
#'   ARMCD = factor(c(rep("ARM A", 5), rep("ARM B", 5), rep("ARM A", 5), rep("ARM B", 5))),
#'   ANRIND = factor(c(
#'     "NORMAL", "HIGH", "HIGH", "HIGH HIGH", "HIGH",
#'     "HIGH", "HIGH", "HIGH HIGH", "NORMAL", "HIGH HIGH", "NORMAL", "LOW", "LOW", "LOW LOW", "LOW",
#'     "LOW", "LOW", "LOW LOW", "NORMAL", "LOW LOW"
#'   )),
#'   ONTRTFL = rep(c("", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"), 2),
#'   PARAMCD = factor(c(rep("CRP", 10), rep("ALT", 10))),
#'   AVALCAT1 = factor(rep(c("", "", "", "SINGLE", "REPLICATED", "", "", "LAST", "", "SINGLE"), 2)),
#'   stringsAsFactors = FALSE
#' )
#'
#' df <- df %>%
#'   mutate(abn_dir = factor(
#'     case_when(
#'       ANRIND == "LOW LOW" ~ "Low",
#'       ANRIND == "HIGH HIGH" ~ "High",
#'       TRUE ~ ""
#'     ),
#'     levels = c("Low", "High")
#'   ))
#'
#' # Select only post-baseline records.
#' df <- df %>% filter(ONTRTFL == "Y")
#' df_crp <- df %>%
#'   filter(PARAMCD == "CRP") %>%
#'   droplevels()
#' full_parent_df <- list(df_crp, "not_needed")
#' cur_col_subset <- list(rep(TRUE, nrow(df_crp)), "not_needed")
#' spl_context <- data.frame(
#'   split = c("PARAMCD", "GRADE_DIR"),
#'   full_parent_df = I(full_parent_df),
#'   cur_col_subset = I(cur_col_subset)
#' )
#'
#' map <- unique(
#'   df[df$abn_dir %in% c("Low", "High") & df$AVALCAT1 != "", c("PARAMCD", "abn_dir")]
#' ) %>%
#'   lapply(as.character) %>%
#'   as.data.frame() %>%
#'   arrange(PARAMCD, abn_dir)
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAMCD") %>%
#'   summarize_num_patients(
#'     var = "USUBJID",
#'     .stats = "unique_count"
#'   ) %>%
#'   split_rows_by(
#'     "abn_dir",
#'     split_fun = trim_levels_to_map(map)
#'   ) %>%
#'   count_abnormal_by_marked(
#'     var = "AVALCAT1",
#'     variables = list(
#'       id = "USUBJID",
#'       param = "PARAMCD",
#'       direction = "abn_dir"
#'     )
#'   ) %>%
#'   build_table(df = df)
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAMCD") %>%
#'   summarize_num_patients(
#'     var = "USUBJID",
#'     .stats = "unique_count"
#'   ) %>%
#'   split_rows_by(
#'     "abn_dir",
#'     split_fun = trim_levels_in_group("abn_dir")
#'   ) %>%
#'   count_abnormal_by_marked(
#'     var = "AVALCAT1",
#'     variables = list(
#'       id = "USUBJID",
#'       param = "PARAMCD",
#'       direction = "abn_dir"
#'     )
#'   ) %>%
#'   build_table(df = df)
#'
#' @export
#' @order 2
count_abnormal_by_marked <- function(lyt,
                                     var,
                                     category = list(single = "SINGLE", last_replicated = c("LAST", "REPLICATED")),
                                     variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir"),
                                     na_str = default_na_str(),
                                     nested = TRUE,
                                     ...,
                                     .stats = NULL,
                                     .formats = NULL,
                                     .labels = NULL,
                                     .indent_mods = NULL) {
  checkmate::assert_string(var)

  extra_args <- list(category = category, variables = variables, ...)

  afun <- make_afun(
    a_count_abnormal_by_marked,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )

  lyt <- analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    na_str = na_str,
    nested = nested,
    show_labels = "hidden",
    extra_args = extra_args
  )
  lyt
}
