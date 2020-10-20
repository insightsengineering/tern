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
#' @param abnormal (named `string` or `character`)\cr identifying the abnormal range level(s) in `var`.
#' @param baseline (`string`)\cr identifying the baseline level(s) in `visit`.
#'
#' @name abnormal
NULL

#' @describeIn abnormal Statistics function which counts patients with abnormal range values
#'   for a single `abnormal` level.
#' @return [s_count_abnormal()] returns the statistic `fraction` which is a
#'   vector with `num` and `denom` counts of patients.
#' @export
#' @examples
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 2, 2)),
#'   AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
#'   ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
#' )
#' # For abnormal level "HIGH" we get the following counts.
#' s_count_abnormal(df, .var = "ANRIND", abnormal = c(high = "HIGH"))
#'
s_count_abnormal <- function(df,
                             .var,
                             abnormal,
                             variables = list(id = "USUBJID", visit = "AVISIT"),
                             baseline = "BASELINE"
) {
  assert_that(
    is_df_with_variables(df, c(range = .var, variables)),
    is.string(abnormal),
    !is.null(names(abnormal)),
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
  list(fraction = with_label(result, names(abnormal)))
}

#' @describeIn abnormal Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal()] returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @examples
#' # Use the Formatted Analysis function for `analyze()`.
#' a_count_abnormal(df, .var = "ANRIND", abnormal = c(low = "LOW"))
#'
a_count_abnormal <- make_afun(
  s_count_abnormal,
  .formats = c(fraction = format_fraction)
)

#' @describeIn abnormal Layout creating function which can be used for creating tables, which can take
#'   statistics function arguments and additional format arguments (see below). Note that it only
#'   works with a single variable but multiple abnormal levels.
#' @return [count_abnormal()] can be used with multiple abnormal levels and modifies the layout.
#' @export
#' @examples
#' # Layout creating function.
#' basic_table() %>%
#'   count_abnormal(var = "ANRIND", abnormal = c(high = "HIGH", low = "LOW")) %>%
#'   build_table(df)
#'
#' # Passing of statistics function and formatting arguments.
#' df2 <- data.frame(
#'   ID = as.character(c(1, 1, 2, 2)),
#'   VISIT = factor(c("SCREENING", "WEEK 1", "SCREENING", "WEEK 1")),
#'   RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
#' )
#' basic_table() %>%
#'   count_abnormal(
#'     var = "RANGE",
#'     abnormal = c(low = "LOW", high = "HIGH"),
#'     variables = list(id = "ID", visit = "VISIT"),
#'     baseline = "SCREENING"
#'   ) %>%
#'   build_table(df2)
#'
count_abnormal <- function(lyt,
                           var,
                           abnormal,
                           ...,
                           .stats = NULL,
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {
  afun <- make_afun(
    a_count_abnormal,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  assert_that(is.string(var))
  for (i in seq_along(abnormal)) {
    abn <- abnormal[i]
    lyt <- analyze(
      lyt = lyt,
      vars = var,
      afun = afun,
      extra_args = c(list(abnormal = abn), list(...)),
      show_labels = "hidden"
    )
  }
  lyt
}
