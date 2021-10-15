#' Patient Counts with Abnormal Range Values
#'
#' Primary analysis variable `.var` indicates the abnormal range result (character or factor)
#' and additional analysis variables are `id` (character or factor) and `baseline` (character or factor).
#' For each direction specified in `abnormal` (e.g. high or low) count patients in the
#' numerator and denominator as follows:
#' \describe{
#'   \item{`num`}{the number of patients with this abnormality recorded while on treatment.}
#'   \item{`denom`}{the number of patients with at least one post-baseline assessment.}
#' }
#' Note, the denominator includes patients that might have other abnormal levels at baseline,
#' and patients with missing baseline. Note, optionally patients with this abnormality at
#' baseline can be excluded from numerator and denominator.
#'
#' @details Note that `df` should be filtered to include only post-baseline records.
#'
#' @inheritParams argument_convention
#' @param abnormal (named `string` or `character`)\cr identifying the abnormal range level(s) in `var`.
#'
#' @name abnormal
#' @include formats.R
#'
NULL

#' @describeIn abnormal Statistics function which counts patients with abnormal range values
#'   for a single `abnormal` level.
#' @param exclude_base_abn (`flag`)\cr whether to exclude subjects with baseline abnormality
#'   from numerator and denominator.
#' @return [s_count_abnormal()] returns the statistic `fraction` which is a
#'   vector with `num` and `denom` counts of patients.
#' @export
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 2, 2)),
#'   ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
#'   BNRIND = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
#'   ONTRTFL = c("", "Y", "", "Y"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Select only post-baseline records.
#' df <- df %>%
#'   filter(ONTRTFL == "Y")
#'
#' # For abnormal level "HIGH" we get the following counts.
#' s_count_abnormal(df, .var = "ANRIND", abnormal = c(high = "HIGH", low = "LOW"))
#'
#' # Optionally exclude patients with abnormality at baseline.
#' s_count_abnormal(df, .var = "ANRIND", abnormal = c(high = "HIGH", low = "LOW"), exclude_base_abn = TRUE)
#'
s_count_abnormal <- function(df,
                             .var,
                             abnormal,
                             variables = list(id = "USUBJID", baseline = "BNRIND"),
                             exclude_base_abn = FALSE
) {
  assert_that(
    is_df_with_variables(df, c(range = .var, variables)),
    is_character_vector(abnormal, min_length = 2, max_length = 2),
    !is.null(names(abnormal)),
    any(abnormal %in% levels(df[[.var]])),
    is_character_or_factor(df[[variables$baseline]]),
    is_character_or_factor(df[[variables$id]]),
    is.factor(df[[.var]]),
    is.flag(exclude_base_abn)
  )

  # This will define the abnormal levels theoretically possible for a specific lab parameter
  # within a split level of a layout.
  abn_levels <- intersect(abnormal, levels(df[[.var]]))
  names(abn_levels) <- names(abnormal)[abnormal %in% abn_levels]

  result <- split(numeric(0), factor(names(abn_levels), levels = names(abn_levels)))

  for (abn in names(abn_levels)) {

    abnormal <- abn_levels[abn]

    # Patients in the denominator fulfill:
    # - have at least one post-baseline visit
    # - their baseline must not be abnormal if `exclude_base_abn`.
    subjects_post_any <- df[[variables$id]]
    subjects_exclude <- if (exclude_base_abn) {
      df[df[[variables$baseline]] == abnormal, ][[variables$id]]
    } else {
      c()
    }
    subjects_denom <- setdiff(subjects_post_any, subjects_exclude)
    denom <- length(subjects_denom)

    # Patients in the numerator fulfill:
    # - have at least one post-baseline visit with the required abnormality level
    # - are part of the denominator patients.
    subjects_post_abnormal <- df[df[[.var]] == abnormal, ][[variables$id]]
    subjects_num <- intersect(subjects_post_abnormal, subjects_denom)
    num <- length(subjects_num)

    result[[abn]] <- with_label(c(num = num, denom = denom), abn)

  }
  result <- list(fraction = result)
  result
}

#' @describeIn abnormal Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal()] returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @examples
#' # Use the Formatted Analysis function for `analyze()`.
#' a_fun <- make_afun(a_count_abnormal, .ungroup_stats = "fraction")
#' a_fun(df, .var = "ANRIND", abnormal = c(low = "LOW", high = "HIGH"))
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
#'   RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
#'   BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
#'   ONTRTFL = c("", "Y", "", "Y"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Select only post-baseline records.
#' df2 <- df2 %>%
#'   filter(ONTRTFL == "Y")
#'
#' basic_table() %>%
#'   count_abnormal(
#'     var = "RANGE",
#'     abnormal = c(low = "LOW", high = "HIGH"),
#'     variables = list(id = "ID", baseline = "BL_RANGE")
#'   ) %>%
#'   build_table(df2)
#'
count_abnormal <- function(lyt,
                           var,
                           ...,
                           table_names = var,
                           .stats = NULL,
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {

  afun <- make_afun(
    a_count_abnormal,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "fraction"
  )
  assert_that(
    is.string(var)
  )

  analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    table_names = table_names,
    extra_args = list(...),
    show_labels = "hidden"
  )
}
