#' Patient Counts with Abnormal Range Values by Baseline Status
#'
#' Primary analysis variable `.var` indicates the abnormal range result (character or factor), and additional
#'   analysis variables are `id` (character or factor) and `baseline` (character or factor). For each
#'   direction specified in `abnormal` (e.g. high or low) we condition on baseline range result and count
#'   patients in the numerator and denominator as follows:
#' * `Not <abnormal>`
#'   * `denom`: the number of patients without abnormality at baseline (excluding those with missing baseline)
#'   * `num`:  the number of patients in `denom` who also have at least one abnormality post-baseline
#' * `<Abnormal>`
#'   * `denom`: the number of patients with abnormality at baseline
#'   * `num`: the number of patients in `denom` who also have at least one abnormality post-baseline
#' * `Total`
#'   * `denom`: the number of patients with at least one valid measurement post-baseline
#'   * `num`: the number of patients in `denom` who also have at least one abnormality post-baseline
#'
#' @details Note that `df` should be filtered to include only post-baseline records.
#'
#' @inheritParams argument_convention
#' @param abnormal (`character`)\cr identifying the abnormal range level(s) in `.var`.
#'
#' @template formatting_arguments
#'
#' @name abnormal_by_baseline
NULL

#' @describeIn abnormal_by_baseline For a single `abnormal` level, produce a named list with 3 slots:
#'   `not_abnormal`, `abnormal` and `total`. Each slot contains a vector with `num` and `denom` counts
#'   of patients.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'  USUBJID = as.character(c(1:6)),
#'  ANRIND = factor(c(rep("LOW", 4), "NORMAL", "HIGH")),
#'  BNRIND = factor(c("LOW", "NORMAL", "HIGH", NA, "LOW", "NORMAL"))
#'  )
#'
#' # Just for one abnormal level.
#' count_abnormal_by_baseline(df, .var = "ANRIND", abnormal = "HIGH")
#'
count_abnormal_by_baseline <- function(df,
                                       .var,
                                       abnormal,
                                       variables = list(id = "USUBJID", baseline = "BNRIND")
) {

  assert_that(
    is.string(.var),
    is.string(abnormal),
    is.list(variables),
    all(names(variables) %in% c("id", "baseline")),
    is_df_with_variables(df, c(range = .var, variables)),
    is_character_or_factor(df[[variables$id]]),
    is_character_or_factor(df[[variables$baseline]]),
    is_character_or_factor(df[[.var]])
  )

  # keep only records with valid analysis value
  df <- df[!is.na(df[[.var]]), ]

  anl <- data.frame(
    id = df[[variables$id]],
    var = as_factor_keep_attributes(df[[.var]]),
    baseline = as_factor_keep_attributes(df[[variables$baseline]]),
    stringsAsFactors = FALSE
  )

  # Total
  #  - Patients in denominator: have at least one valid measurement post-baseline
  #  - Patients in numerator: have at least one abnormality
  total_denom <- length(unique(anl$id))
  total_num <- length(unique(anl$id[anl$var == abnormal]))

  # baseline NA records are counted only in total rows
  anl <- anl[!is.na(anl$baseline), ]

  # Abnormal
  #   - Patients in denominator: have abnormality at baseline
  #   - Patients in numerator: have abnormality at baseline AND
  #     have at least one abnormality post-baseline
  abn_denom <- length(unique(anl$id[anl$baseline == abnormal]))
  abn_num <- length(unique(anl$id[anl$baseline == abnormal & anl$var == abnormal]))

  # Not abnormal
  #   - Patients in denominator: do not have abnormality at baseline
  #   - Patients in numerator: do not have abnormality at baseline AND
  #     have at least one abnormality post-baseline
  not_abn_denom <- length(unique(anl$id[anl$baseline != abnormal]))
  not_abn_num <- length(unique(anl$id[anl$baseline != abnormal & anl$var == abnormal]))

  result <- list(
    c(num = not_abn_num, denom = not_abn_denom),
    c(num = abn_num, denom = abn_denom),
    c(num = total_num, denom = total_denom)
  )

  setNames(result, c("not_abnormal", "abnormal", "total"))
}

#' @describeIn abnormal_by_baseline Statistics function which counts patients with abnormal range values
#'   by baseline status for multiple `abnormal` levels, and returns a list with labeled entries.
#' @export
#' @examples
#'
#' # Use the statistics function to count patients for multiple abnormal levels.
#' s_count_abnormal_by_baseline(df, .var = "ANRIND", abnormal = c(low = "LOW", high = "HIGH"))
#'
s_count_abnormal_by_baseline <- function(df,
                                         .var,
                                         abnormal,
                                         ...
) {

  assert_that(!is.null(names(abnormal)))

  result <- lapply(abnormal, count_abnormal_by_baseline, df = df, .var = .var, ...)

  lbl_result <- Map(function(x, nm){

    null_name <- paste0(toupper(substr(nm, 1, 1)), tolower(substring(nm, 2)))
    not_abn_name <- paste("Not", tolower(nm), "baseline status")
    abn_name <- paste(null_name, "baseline status")
    total_name <- "Total"

    list(
      section_label = with_label("", null_name),
      not_abnormal = with_label(x[["not_abnormal"]], not_abn_name),
      abnormal = with_label(x[["abnormal"]], abn_name),
      total = with_label(x[["total"]], total_name)
    )

  }, x = result, nm = names(result))

  flatten_list(lbl_result)
}

#' @describeIn abnormal_by_baseline Layout creating function which can be used for creating tables, which can take
#'   statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#'
#' # Layout creating function.
#' basic_table() %>%
#'   analyze_abnormal_by_baseline(vars = "ANRIND", abnormal = c(high = "HIGH")) %>%
#'   build_table(df)
#'
#' # Passing of statistics function and formatting arguments.
#' df2 <- data.frame(
#'   ID = as.character(c(1, 2, 3, 4)),
#'   RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
#'   BLRANGE = factor(c("LOW", "HIGH", "HIGH", "NORMAL"))
#' )
#'
#' basic_table() %>%
#'   analyze_abnormal_by_baseline(
#'     vars = "RANGE",
#'     abnormal = c(low = "LOW"),
#'     variables = list(id = "ID", baseline = "BLRANGE"),
#'     .formats = c(not_abnormal = "xx / xx", abnormal = "xx / xx", total = "xx / xx"),
#'     .indent_mods = c(total = 2L)
#'   ) %>%
#'   build_table(df2)
#'
analyze_abnormal_by_baseline <- function(lyt,
                                         vars,
                                         ...) {
  a_count_abnormal_by_baseline <- format_wrap_df(
    s_count_abnormal_by_baseline,
    indent_mods = c(section_label = 0L, not_abnormal = 1L, abnormal = 1L, total = 1L),
    formats = c(
      section_label = "xx",
      not_abnormal = format_fraction,
      abnormal = format_fraction,
      total = format_fraction
    )
  )
  analyze(
    lyt,
    vars,
    afun = a_count_abnormal_by_baseline,
    extra_args = c(
      list(...)
    )
  )
}
