#' Patient Counts with Abnormal Range Values
#'
#' Primary analysis variable is the `range` factor, and additional analysis variables are
#' `id` (character or factor), `visit` (factor).
#' We count patients in the numerator and denominator as follows:
#' \describe{
#'   \item{`num`}{the number of patients without this abnormality at baseline and with
#'     this abnormality recorded while on treatment.}
#'   \item{`denom`}{the number of patients without this abnormality at baseline and at least one
#'     post-baseline assessment.}
#' }
#' Here the baseline visit is identified as the `visit` level(s) in `baseline`.
#' Note that the denominators include patients that might have other abnormal levels
#' at baseline, and patients with missing baseline.
#'
#' @inheritParams argument_convention
#' @param abnormal string identifying the abnormal range level(s) in `vars`
#' @param baseline string identifying the baseline level(s) in `visit`
#'
#' @template formatting_arguments
#'
#' @name abnormal
NULL

#' @describeIn abnormal Produce a vector with `num` and `denom` counts of patients for a single `abnormal`
#'   level.
#' @export
#' @examples
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 2, 2)),
#'   AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
#'   ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
#' )
#' # Just for one abnormal level.
#' count_abnormal(df, .var = "ANRIND", abnormal = "HIGH")
count_abnormal <- function(df,
                           .var,
                           abnormal,
                           variables = list(id = "USUBJID", visit = "AVISIT"),
                           baseline = "BASELINE"
) {
  assert_that(
    is_df_with_variables(df, c(range = .var, variables)),
    is.string(abnormal),
    is_character_vector(baseline),
    is_character_or_factor(df[[variables$id]]),
    is.factor(df[[variables$visit]]),
    is.factor(df[[.var]]),
    abnormal %in% levels(df[[.var]]),
    all(baseline %in% levels(df[[variables$visit]]))
  )

  # Split up data frame in baseline and post-baseline visit rows.
  df_baseline <- df[df[[variables$visit]] %in% baseline, ]
  df_post_baseline <- df[!(df[[variables$visit]] %in% baseline), ]

  # Patients in the denominator fulfill:
  # - have at least one post-baseline visit
  # - their baseline must not be abnormal.
  subjects_post_any <- df_post_baseline[[variables$id]]
  subjects_exclude <- df_baseline[df_baseline[[.var]] == abnormal, ][[variables$id]]
  subjects_denom <- setdiff(subjects_post_any, subjects_exclude)
  denom <- length(subjects_denom)

  # Patients in the numerator fulfill:
  # - have at least one post-baseline visit with the required abnormality level
  # - are part of the denominator patients.
  subjects_post_abnormal <- df_post_baseline[df_post_baseline[[.var]] == abnormal, ][[variables$id]]
  subjects_num <- intersect(subjects_post_abnormal, subjects_denom)
  num <- length(subjects_num)

  result <- c(num = num, denom = denom)
  return(result)
}

#' @describeIn abnormal Statistics function which counts patients with abnormal range values
#'   for multiple `abnormal` levels, and returns a list with one element each.
#' @export
#' @examples
#'
#' # Use the statistics function to count patients for multiple abnormal levels.
#' s_count_abnormal(df, .var = "ANRIND", abnormal = c(low = "LOW", high = "HIGH"))
s_count_abnormal <- function(df,
                             .var,
                             abnormal,
                             ...) {

  assert_that(!is.null(names(abnormal)))
  result <- lapply(abnormal, count_abnormal, df = df, .var = .var, ...)
  list(fraction = result)
}

#' @describeIn abnormal Layout creating function which can be used for creating tables, which can take
#'   statistics function arguments and additional format arguments (see below).
#' @export
#' @examples
#'
#' # Layout creating function.
#' basic_table() %>%
#'   analyze_abnormal(vars = "ANRIND", abnormal = c(high = "HIGH")) %>%
#'   build_table(df)
#'
#' # Passing of statistics function and formatting arguments.
#' df2 <- data.frame(
#'   ID = as.character(c(1, 1, 2, 2)),
#'   VISIT = factor(c("SCREENING", "WEEK 1", "SCREENING", "WEEK 1")),
#'   RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
#' )
#' basic_table() %>%
#'   analyze_abnormal(
#'     vars = "RANGE",
#'     abnormal = c(low = "LOW", high = "HIGH"),
#'     variables = list(id = "ID", visit = "VISIT"),
#'     baseline = "SCREENING",
#'     .labels = c(low = "< LLN"),
#'     .indent_mods = c(fraction = 1L)
#'   ) %>%
#'   build_table(df2)
analyze_abnormal <- function(lyt,
                             vars,
                             ...) {
  a_count_abnormal <- format_wrap_df(
    s_count_abnormal,
    indent_mods = c(fraction = 0L),
    formats = c(fraction = format_fraction)
  )
  analyze(
    lyt,
    vars,
    afun = a_count_abnormal,
    extra_args = c(
      list(...)
    )
  )
}
