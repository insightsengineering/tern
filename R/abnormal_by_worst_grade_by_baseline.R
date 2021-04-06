#' Patient Counts with the Most Extreme Post-Baseline Toxicity Grade by Baseline NCI-CTCAE Grade
#'
#' @description
#' Patient count and fraction for laboratory test shift table, highest NCI-CTCAE grade post-baseline by
#' baseline NCI-CTCAE grade, according to user-defined `grouping_list`, is generated. Per GDSR
#' standard:
#' * for abnormality in the high direction, `grouping_list` should be defined as: \cr
#' `list("Not High" = c(0, -1, -2, -3, -4), "1" = 1, "2" = 2, "3" = 3, "4" = 4, "Missing" = "<Missing>")`
#' * for abnormality in the low direction, `grouping_list` should be defined as: \cr
#' `list("Not Low" = c(0, 1, 2, 3, 4), "1" = -1, "2" = -2, "3" = -3, "4" = -4, "Missing" = "<Missing>")` \cr
#'
#' For a particular baseline NCI-CTCAE grade group that is in the `grouping_list`, for example, "Not High",
#' assuming abnormality is in the high direction, the total number of patients who are "Not High" at
#' baseline is counted, that is the number of patients who have a baseline NCI-CTCAE grades of `0`,
#' `-1`, `-2`, `-3` or `-4`. In addition, for these patients who are "Not High" at baseline, the
#' counts and fractions of patients with post-baseline NCI-CTCAE grades in each of the groups in
#' `group_listing` are produced, i.e., "Not High", "1", "2", "3", "4" and "Missing". Note the order
#' of `grouping_list` controls the output order. \cr
#'
#' Care must be taken when specifying the `grouping_list`. Each element in the `grouping_list` must
#' be specified using a `=` sign; on the left of the `=` sign is the name of the group; on the
#' right of `=` sign is NCI-CTCAE grade(s) to be included in that group, i.e., `"NAME" = c(1, 2, 3, 4)`. \cr
#'
#' User may define custom functions for extracting the worst lab grades. At the minimum, the output `df`
#' should contain relevant subject-level variables derived from ADSL (e.g. `USUBJID`, `ACTARM`) and analysis variables
#' sourced from ADLB:`PARAM` or `PARAMCD`, `ATOXGR` and `BTOXGR`.
#' One worst lab grade per patient per lab is expected for each of `ATOXGR` and `BTOXGR` unless summarizing the worst
#' grade by visit in which case a visit variable such as `AVISIT` would be necessary.
#'
#' @inheritParams argument_convention
#'
#' @name abnormal_by_worst_grade_by_baseline
#'
NULL

#' @describeIn abnormal_by_worst_grade_by_baseline Helper function to filter ADSL and ADLB datasets
#' to create a dataset with the worst post-baseline lab using (`worst_flag`) flag. Missing data will be created for the
#' following situations:
#'  * patients who are present in ADSL but no lab data in ADLB (both baseline and post-baseline)
#'  * patients who do not have any post-baseline lab values
#'  * patients without any post-baseline values flagged as the worst
#' @param adsl (`data frame`) ADSL dataframe
#' @param adlb (`data frame`) ADLB dataframe
#' @param worst_flag (named `vector`)
#' Worst post-baseline lab flag variable
#' @param by_visit (`logical`) defaults to `FALSE` to generate worst grade per patient.
#' If worst grade per patient per visit is specified for `worst_flag`, then
#' `by_visit` should be `TRUE` to generate worst grade patient per visit.
#' @param no_fillin_visits (named `character`)
#' Visits that are not considered for post-baseline worst toxicity grade. Defaults to `c("SCREENING", "BASELINE")`.
#' @return [h_adsl_adlb_merge_using_worst_flag()] returns a `df`
#' containing variables shared between `adlb` and `adsl` along with variables relevant for analysis:
#' `PARAM`, `PARAMCD`, `ATOXGR`, and `BTOXGR`.  Optionally `AVISIT`, `AVISITN` are included when `by_visit = TRUE` and
#' `no_fillin_visits = c("SCREENING", "BASELINE")`.
#'
#' @export
#' @importFrom dplyr select filter pull left_join
#'
#' @examples
#' library(random.cdisc.data)
#' adlb <- radlb(cached = TRUE)
#' adsl <- radsl(cached = TRUE)
#'
#' # `h_adsl_adlb_merge_using_worst_flag`
#' adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl, adlb, worst_flag = c("WGRHIFL" = "Y"))
#'
#' # `h_adsl_adlb_merge_using_worst_flag` by visit example
#' adlb_out_by_visit <- h_adsl_adlb_merge_using_worst_flag(
#'   adsl,
#'   adlb,
#'   worst_flag = c("WGRLOVFL" = "Y"),
#'   by_visit = TRUE
#' )
#'
h_adsl_adlb_merge_using_worst_flag <- function( #nolint
  adsl,
  adlb,
  worst_flag = c("WGRHIFL" = "Y"),
  by_visit = FALSE,
  no_fillin_visits = c("SCREENING", "BASELINE")
){
  col_names <- names(worst_flag)
  filter_values <- worst_flag

  temp <- Map(
    function(x, y) which(adlb[[x]] == y),
    col_names,
    filter_values
  )

  position_satisfy_filters <- Reduce(intersect, temp)

  adsl_adlb_common_columns <- intersect(colnames(adsl), colnames(adlb))
  columns_from_adlb <- c("USUBJID", setdiff(colnames(adlb), adsl_adlb_common_columns))

  adlb_f <- adlb[position_satisfy_filters, ] %>%
    filter(!.data[["AVISIT"]] %in% no_fillin_visits) %>%
    select(columns_from_adlb)

  avisits_grid <- adlb %>%
    filter(!.data[["AVISIT"]] %in% no_fillin_visits) %>%
    pull(.data[["AVISIT"]]) %>%
    unique()

  if (by_visit) {
    adsl_lb <- expand.grid(
      USUBJID = unique(adsl$USUBJID),
      AVISIT = avisits_grid,
      PARAMCD = unique(adlb$PARAMCD)
    )

    adsl_lb <- adsl_lb %>%
      left_join(unique(adlb[c("AVISIT", "AVISITN")]), by = "AVISIT") %>%
      left_join(unique(adlb[c("PARAM", "PARAMCD")]), by = "PARAMCD")

    adsl_lb <- adsl %>%
      select(adsl_adlb_common_columns) %>%
      merge(adsl_lb, by = "USUBJID")

    by_variables_from_adlb <- c("USUBJID", "AVISIT", "AVISITN", "PARAMCD", "PARAM")

    adlb_out <- merge(
      adlb_f,
      adsl_lb,
      by = by_variables_from_adlb,
      all = TRUE,
      sort = FALSE
      )

    adlb_var_labels <- c(var_labels(adlb[by_variables_from_adlb]),
                         var_labels(adlb[columns_from_adlb[! columns_from_adlb %in% by_variables_from_adlb]]),
                         var_labels(adsl[adsl_adlb_common_columns[adsl_adlb_common_columns != "USUBJID"]])
                         )

    }else{
    adsl_lb <- expand.grid(
      USUBJID = unique(adsl$USUBJID),
      PARAMCD = unique(adlb$PARAMCD)
      )

    adsl_lb <- adsl_lb %>% left_join(unique(adlb[c("PARAM", "PARAMCD")]), by = "PARAMCD")

    adsl_lb <- adsl %>%
      select(adsl_adlb_common_columns) %>%
      merge(adsl_lb, by = "USUBJID")

    by_variables_from_adlb <- c("USUBJID", "PARAMCD", "PARAM")

    adlb_out <- merge(
      adlb_f,
      adsl_lb,
      by = by_variables_from_adlb,
      all = TRUE,
      sort = FALSE
    )

    adlb_var_labels <- c(var_labels(adlb[by_variables_from_adlb]),
                         var_labels(adlb[columns_from_adlb[! columns_from_adlb %in% by_variables_from_adlb]]),
                         var_labels(adsl[adsl_adlb_common_columns[adsl_adlb_common_columns != "USUBJID"]])
    )
  }

  adlb_out$ATOXGR <- as.factor(adlb_out$ATOXGR) #nolint
  adlb_out$BTOXGR <- as.factor(adlb_out$BTOXGR) #nolint

  adlb_out <- df_explicit_na(adlb_out)
  var_labels(adlb_out) <- adlb_var_labels

  adlb_out
}

#' @describeIn abnormal_by_worst_grade_by_baseline Helper function to group patients
#' (identified using `id`) according to post-baseline toxicity in `.var`,
#' then the counts and fractions are produced according to `grouping_list`, along with total number
#' of patients.
#' @param grouping_list (named `list`) defining how inner NCI-CTCAE grades should be grouped.
#' @return [h_group_counter()] returns the total number of patients, and counts and fractions for each
#' group identified in the `grouping_list` in a named list, with names from `grouping_list`.
#'
#' @export
#'
#' @examples
#'
#' # `h_group_counter`
#' h_group_counter(
#'   df = adlb_out,
#'   id = "USUBJID",
#'   .var = "ATOXGR",
#'   grouping_list = list(
#'     `Missing`= "<Missing>",
#'     `Not High` = c(0, -1, -2, -3, -4),
#'     `1` = 1,
#'     `2` = 2,
#'     `3` = 3,
#'     `4` = 4
#'     )
#'   )
#'
h_group_counter <- function(df, id, .var, grouping_list) {

  assert_that(
    is.string(id),
    is.string(.var),
    is_df_with_variables(df, list(val = c(id, .var)))
  )

  n <- length(unique(df[[id]]))

  if (nrow(df) > 0) {
    by_grade <- lapply(grouping_list, function(i) {
      num <- length(unique(df[df[[.var]] %in% i, id, drop = TRUE]))
      c(num, num / n)
    })
  }else{
    by_grade <- lapply(grouping_list, function(i) {
      c(0, 0)
      })
  }

  list(count = list("Total" = n), count_fraction = by_grade)
}

#' @describeIn abnormal_by_worst_grade_by_baseline Statistics function, for each NCI-CTCAE grade
#' group specified in `by_grade`, this function calculates the total, counts and fraction of
#' patients' worst NCI-CTCAE grades in `.var` falling into each of the groups
#' in `group_listing`.
#'
#' @param variables (named `list` of `string`) list of additional analysis variables including:
#' * `id` (`string`): \cr subject variable name
#' * `by_var` (`string`): \cr name of the data column containing the outer level toxicity variable within which toxicity
#' variable `.var` is counted. Inputs for `by_var` and `.var` can be switched, e.g. `.var = ATOXGR` and
#' `by_var = BTOXGR`, this generates post-baseline grade by baseline grade table, or `.var = BTOXGR` and
#' `by_var = ATOXGR`, this generates baseline grade by post-baseline grade table.
#' * `by_grade` (`character`): \cr outer level toxicity group of interest for
#' [s_count_abnormal_by_worst_grade_by_baseline()]. `by_grade` must be identical to the name
#' of an element in the `grouping_list`
#' * `by_grade_list` (`list`): \cr indicating all outer groups of interest for
#' [count_abnormal_by_worst_grade_by_baseline()]. If `by_grade_list`` is not specified,
#' then all groups in `grouping_list` will be used. `by_grade_list` follows the same format as `grouping_list`.
#'
#' @return [s_count_abnormal_by_worst_grade_by_baseline()] returns the total patient count, and the
#' counts and fraction of patients' worst post-baseline NCI-CTCAE grades in each of the groups
#' in `group_listing`, for a particular baseline NCI-CTCAE grade group, `by_grade`. Elements of
#' `by_grade_list` must be in the set of names of elements in `grouping_list`, or
#'
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' # Count worst post-baseline lab for patients who were `Not Low` at baseline.
#' s_count_abnormal_by_worst_grade_by_baseline(
#'   df = adlb_out %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   grouping_list = list(
#'     "Missing" = "<Missing>",
#'     "Not Low" = c(0, 1, 2, 3, 4),
#'     "1" = -1,
#'     "2" = -2,
#'     "3" = -3,
#'     "4" = -4
#'     ),
#'   variables = list(
#'     id = "USUBJID",
#'     by_var = "BTOXGR",
#'     by_grade = "Not Low"
#'   )
#' )
#'
s_count_abnormal_by_worst_grade_by_baseline <- function(df, #nolint
                                                        .var = "ATOXGR",
                                                        grouping_list = list(
                                                          `Missing` = "<Missing>",
                                                          `Not High` = c(0, -1, -2, -3, -4),
                                                          `1` = 1,
                                                          `2` = 2,
                                                          `3` = 3,
                                                          `4` = 4
                                                          ),
                                                        variables = list(
                                                          id = "USUBJID",
                                                          by_var = "BTOXGR",
                                                          by_grade = "Missing"
                                                          )
                                                        ) {
  assert_that(
    is.string(.var),
    is_variables(variables[1:2]),
    setequal(names(variables), c("id", "by_var", "by_grade")),
    #variables$by_grade %in% names(grouping_list),
    is_df_with_variables(df, c(aval = .var, variables[1:2]))
  )


  baseline <- unlist(variables$by_grade, use.names = FALSE)

  anl <- data.frame(
    df[[variables$id]],
    baseline = df[[variables$by_var]],
    df[[.var]],
    stringsAsFactors = FALSE
    )
  anl <- setNames(anl, c(variables$id, "baseline", .var))

  anl <- anl[as.character(anl$baseline) %in% baseline, ]

  h_group_counter(anl, variables$id, .var,  grouping_list)
}


#' @describeIn abnormal_by_worst_grade_by_baseline Formatted Analysis function which can be further customized by
#' calling [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal_by_worst_grade()] returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @examples
#'
#' afun <- make_afun(a_count_abnormal_by_worst_grade_by_baseline, .ungroup_stats = "count_fraction")
#' afun(
#' df = adlb_out %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#' .var = "ATOXGR",
#' grouping_list = list(
#'   "Missing" = "<Missing>",
#'   "Not High" = c(0, -1, -2, -3, -4),
#'   "1" = 1,
#'    "2" = 2,
#'    "3" = 3,
#'    "4" = 4
#'    ),
#' variables = list(id = "USUBJID",
#'                  by_var = "BTOXGR",
#'                  by_grade = "4" )
#' )
#'
a_count_abnormal_by_worst_grade_by_baseline <- make_afun(  #nolint
  s_count_abnormal_by_worst_grade_by_baseline,
  .formats = c(count = "xx", count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_worst_grade_by_baseline Layout creating function which can be used for creating tables,
#'    which can take statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   add_colcounts() %>%
#'   split_rows_by("PARAMCD") %>%
#'   count_abnormal_by_worst_grade_by_baseline(
#'     var = "ATOXGR",
#'     grouping_list = list(
#'       "Missing" = "<Missing>",
#'       "Not High" = c(0, -1, -2, -3, -4),
#'       "1" = 1,
#'       "2" = 2,
#'       "3" = 3,
#'       "4" = 4
#'       ),
#'     variables = list(
#'       id = "USUBJID",
#'       by_var = "BTOXGR"
#'       )
#'     ) %>%
#'   build_table(df = adlb_out, alt_counts_df = adsl)
#'
count_abnormal_by_worst_grade_by_baseline <- function(lyt, #nolint
                                                      var,
                                                      grouping_list,
                                                      variables = list(
                                                        id = "USUBJID",
                                                        by_var = "BTOXGR",
                                                        by_grade_list = NULL
                                                        ),
                                                      ...,
                                                      table_names = NULL,
                                                      .stats = NULL,
                                                      .formats = NULL,
                                                      .labels = NULL,
                                                      .indent_mods = NULL) {
  assert_that(
    is.string(var),
    setequal(names(variables)[1:2], c("id", "by_var"))
  )

  afun <- make_afun(
    a_count_abnormal_by_worst_grade_by_baseline,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = c("count", "count_fraction")
  )

  by_grade_list <- if_null(variables$by_grade_list, grouping_list)

  for (i in seq(by_grade_list)) {
    varlist <- list(
      id = variables$id,
      by_var = variables$by_var,
      by_grade = by_grade_list[i]
    )
    lyt <- analyze(
      lyt = lyt,
      vars = var,
      var_labels = names(varlist$by_grade),
      table_names = names(varlist$by_grade),
      afun = afun,
      extra_args = c(list(grouping_list = grouping_list, variables = varlist), list(...)),
      show_labels = "visible"
      )
    }

  lyt
}
