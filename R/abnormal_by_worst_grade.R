#' Patient Counts with the Most Extreme Post-baseline Toxicity Grade per Direction of Abnormality
#'
#' Primary analysis variable `.var` indicates the toxicity grade (numeric), and additional
#'   analysis variables are `id` (character or factor) and `worst_grade_flag` (logical). For each
#'   direction specified in `abnormal` (e.g. high or low) we condition on the worst grade flag and count
#'   patients in the denominator as number of patients with at least one valid measurement during treatment,
#'   and patients in the numerator as follows:
#' * `1` to `5`: Numerator is number of patients with worst grades 1-5 respectively;
#' * `Any`: Numerator is number of patients with at least one abnormality, which means grade is different from 0.
#'
#' @details Note that `df` should be filtered to include only post-baseline records.
#'
#' @inheritParams argument_convention
#' @param abnormal (`character`)\cr identifying the abnormality direction.
#'
#' @name abnormal_by_worst_grade
#'
NULL

#' @describeIn abnormal_by_worst_grade Statistics function which counts patients with worst grade
#'   for a single `abnormal` level, consisting of counts and percentages of patients with worst grade
#'   `1` to `5`, and `Any` non-zero grade.
#' @return [s_count_abnormal_by_worst_grade()] the single statistic `count_fraction` with grade 1 to 5
#'   and "Any" results.
#'
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adlb <- radlb(cached = TRUE)
#' adlb_f <- adlb %>%
#'   filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
#'   mutate(
#'     ATOXGR = as.numeric(as.character(ATOXGR)),
#'     WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
#'     WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE)
#'   )
#'
#' s_count_abnormal_by_worst_grade(
#'   df = adlb_f %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = "low",
#'   variables = list(id = "USUBJID", worst_grade_flag = "WGRLOFL")
#' )
#' s_count_abnormal_by_worst_grade(
#'   df = adlb_f %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = "high",
#'   variables = list(id = "USUBJID", worst_grade_flag = "WGRHIFL")
#' )
#'
s_count_abnormal_by_worst_grade <- function(df, #nolint
                                            .var = "ATOXGR",
                                            abnormal = c("low", "high"),
                                            variables = list(id = "USUBJID", worst_grade_flag = "WGRLOFL")) {
  abnormal <- match.arg(abnormal)
  assert_that(
    is.string(.var),
    is.string(abnormal),
    is.list(variables),
    all(names(variables) %in% c("id", "worst_grade_flag")),
    is_df_with_variables(df, c(aval = .var, variables)),
    is_numeric_vector(df[[.var]]),
    is_logical_vector(df[[variables$worst_grade_flag]]),
    is_character_or_factor(df[[variables$id]])
  )

  df <- df[!is.na(df[[.var]]), ]
  anl <- data.frame(
    id = df[[variables$id]],
    grade = df[[.var]],
    flag = df[[variables$worst_grade_flag]],
    stringsAsFactors = FALSE
  )
  # Denominator is number of patients with at least one valid measurement during treatment
  n <- length(unique(anl$id))
  # Numerator is number of patients with worst high grade (grade 1 to 5) or low grade (grade -1 to -5)
  if (abnormal == "low") {
    anl_abn <- anl[anl$flag & anl$grade < 0, , drop = FALSE]
    grades <- setNames(- (1:5), as.character(1:5))
  } else if (abnormal == "high") {
    anl_abn <- anl[anl$flag & anl$grade > 0, , drop = FALSE]
    grades <- setNames(1:5, as.character(1:5))
  }

  by_grade <- lapply(grades, function(i) {
    num <- length(unique(anl_abn[anl_abn$grade == i, "id", drop = TRUE]))
    c(num, num / n)
  })
  # Numerator for "Any" grade is number of patients with at least one high/low abnormality
  any_grade_num <- length(unique(anl_abn$id))

  list(count_fraction = c(by_grade, list("Any" = c(any_grade_num, any_grade_num / n))))
}

#' @describeIn abnormal_by_worst_grade Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal_by_worst_grade()] returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @examples
#' # Use the Formatted Analysis function for `analyze()`. We need to ungroup `count_fraction` first
#' # so that the rtables formatting function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_abnormal_by_worst_grade, .ungroup_stats = "count_fraction")
#' afun(
#'   df = adlb_f %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = "high",
#'   variables = list(id = "USUBJID", worst_grade_flag = "WGRHIFL")
#' )
#'
a_count_abnormal_by_worst_grade <- make_afun(  #nolint
  s_count_abnormal_by_worst_grade,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn abnormal_by_worst_grade Layout creating function which can be used for creating tables,
#'    which can take statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#'
#' basic_table() %>%
#'   count_abnormal_by_worst_grade(
#'     var = "ATOXGR",
#'     abnormal = c(Low = "low", High = "high"),
#'     variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
#'   ) %>%
#'   build_table(df = adlb_f %>% filter(ARMCD == "ARM A" & PARAMCD == "CRP"))
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAMCD") %>%
#'   count_abnormal_by_worst_grade(
#'     var = "ATOXGR",
#'     abnormal = c(Low = "low", High = "high"),
#'     variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
#'   ) %>%
#'   build_table(df = adlb_f)
#'
count_abnormal_by_worst_grade <- function(lyt,
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
    setequal(names(abnormal), names(variables$worst_grade_flag)),
    is_equal_length(abnormal, table_names)
  )
  afun <- make_afun(
    a_count_abnormal_by_worst_grade,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )
  for (i in seq_along(abnormal)) {
    abn <- abnormal[i]
    varlist <- variables
    varlist$worst_grade_flag <- varlist$worst_grade_flag[names(abn)]
    lyt <- analyze(
      lyt = lyt,
      vars = var,
      var_labels = names(abn),
      table_names = table_names[i],
      afun = afun,
      extra_args = c(list(abnormal = abn, variables = varlist), list(...)),
      show_labels = "visible"
    )
  }
  lyt
}
