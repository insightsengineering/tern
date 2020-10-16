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
#' @template formatting_arguments
#'
#' @name abnormal_by_worst_grade
#'
NULL

#' @describeIn abnormal_by_worst_grade For a single `abnormal` level, produce a named list with
#'    counts and percentages of patients with worst grade `1` to `5`, and `Any` non-zero grade.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' adlb <- radlb(cached = TRUE)
#' adlb_f <- adlb %>%
#'   dplyr::filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
#'   dplyr::mutate(
#'     ATOXGR = as.numeric(as.character(ATOXGR)),
#'     WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
#'     WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE)
#'   )
#'
#' h_abnormal_by_worst_grade(
#'   df = adlb_f %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = "low",
#'   variables = list(id = "USUBJID", worst_grade_flag = "WGRLOFL")
#' )
#' h_abnormal_by_worst_grade(
#'   df = adlb_f %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = "high",
#'   variables = list(id = "USUBJID", worst_grade_flag = "WGRHIFL")
#' )
#'
h_abnormal_by_worst_grade <- function(df,
                                      .var = "ATOXGR",
                                      abnormal = c("low", "high"),
                                      variables = list(id = "USUBJID", worst_grade_flag = "WGRLOFL")) {
  abnormal <- match.arg(abnormal)
  assertthat::assert_that(
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
  if (abnormal == "low"){
    anl_abn <- anl[anl$flag & anl$grade < 0, , drop = FALSE]
    grades <- setNames(- (1:5), as.character(1:5))
  } else if (abnormal == "high"){
    anl_abn <- anl[anl$flag & anl$grade > 0, , drop = FALSE]
    grades <- setNames(1:5, as.character(1:5))
  }

  by_grade <- lapply(grades, function(i) {
    num <- length(unique(anl_abn[anl_abn$grade == i, "id", drop = TRUE]))
    c(num, num / n)
  })
  # Numerator for "Any" grade is number of patients with at least one high/low abnormality
  any_grade_num <- length(unique(anl_abn$id))

  c(by_grade, list("Any" = c(any_grade_num, any_grade_num / n)))
}

#' @describeIn abnormal_by_worst_grade Statistics function which counts patients with worst grade
#'    for multiple `abnormal` levels, and returns a list with labeled entries.
#' @export
#' @examples
#'
#' # Use the statistics function to count patients for multiple abnormal levels.
#' s_abnormal_by_worst_grade(
#'   df = adlb_f %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = c(HIGH = "high"),
#'   variables = list(id = "USUBJID", worst_grade_flag = c(HIGH = "WGRHIFL"))
#' )
#'
#' s_abnormal_by_worst_grade(
#'   df = adlb_f %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
#'   .var = "ATOXGR",
#'   abnormal = c(Low = "low", High = "high"),
#'   variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
#' )
#'
s_abnormal_by_worst_grade <- function(df,
                                      .var,
                                      abnormal = c("Low" = "low", "High" = "high"),
                                      variables = list(
                                        id = "USUBJID",
                                        worst_grade_flags = c("Low" = "WGRLOFL", "High" = "WGRHIFL")
                                      )) {
  assert_that(
    !is.null(names(abnormal)),
    !is.null(names(variables$worst_grade_flag)),
    identical(names(abnormal), names(variables$worst_grade_flag))
  )

  result <- Map(function(abn, flag) {
    h_abnormal_by_worst_grade(
      df = df,
      .var = .var,
      abnormal = abn,
      variables = list(
        id = variables$id,
        worst_grade_flag = flag
      )
    )
  }, abnormal, variables$worst_grade_flag)

  lbl_result <- Map(function(x, nm) {
    list(
      section_label = with_label("", nm),
      count_fraction = x
    )
  }, x = result, nm = names(result))
  flatten_list(lbl_result)
}


#' @describeIn abnormal_by_worst_grade Layout creating function which can be used for creating tables,
#'    which can take statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#'
#' basic_table() %>%
#'   abnormal_by_worst_grade(
#'     vars = "ATOXGR",
#'     abnormal = c(Low = "low", High = "high"),
#'     variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
#'   ) %>%
#'   build_table(df = adlb_f %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"))
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD") %>%
#'   split_rows_by("PARAMCD") %>%
#'   abnormal_by_worst_grade(
#'     vars = "ATOXGR",
#'     abnormal = c(Low = "low", High = "high"),
#'     variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
#'   ) %>%
#'   build_table(df = adlb_f)
#'
abnormal_by_worst_grade <- function(lyt,
                                    vars,
                                    ...) {
  a_abnormal_by_worst_grade <- format_wrap_df(
    s_abnormal_by_worst_grade,
    indent_mods = c(section_label = 0L, count_fraction = 2L),
    formats = c(section_label = "xx", count_fraction = format_count_fraction)
  )
  analyze(
    lyt,
    vars,
    afun = a_abnormal_by_worst_grade,
    extra_args = list(...)
  )
}
