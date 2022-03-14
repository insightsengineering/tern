#' Patient Counts with Abnormal Range Values
#'
#' Primary analysis variable `.var` indicates the abnormal range result (character or factor)
#' and additional analysis variables are `id` (character or factor) and `baseline` (character or
#' factor). For each direction specified in `abnormal` (e.g. high or low) count patients in the
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
#' @param abnormal (`named list`)\cr identifying the abnormal range level(s) in `var`. Default to
#' `list(Low = "LOW", High = "HIGH")` but you can also group different levels into the name list,
#' for example, `abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH"))`
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
#' s_count_abnormal(df, .var = "ANRIND", abnormal = list(high = "HIGH", low = "LOW"))
#'
#' # Optionally exclude patients with abnormality at baseline.
#' s_count_abnormal(
#'   df,
#'   .var = "ANRIND",
#'   abnormal = list(high = "HIGH", low = "LOW"),
#'   exclude_base_abn = TRUE
#' )
s_count_abnormal <- function(df,
                             .var,
                             abnormal = list(Low = "LOW", High = "HIGH"),
                             variables = list(id = "USUBJID", baseline = "BNRIND"),
                             exclude_base_abn = FALSE) {
  checkmate::assert_list(abnormal, types = "character", names = "named", len = 2, any.missing = FALSE)

  assertthat::assert_that(
    is_df_with_variables(df, c(range = .var, variables)),
    any(unlist(abnormal) %in% levels(df[[.var]])),
    is_character_or_factor(df[[variables$baseline]]),
    is_character_or_factor(df[[variables$id]]),
    is.factor(df[[.var]]),
    assertthat::is.flag(exclude_base_abn)
  )

  count_abnormal_single <- function(abn_name, abn) {
    # Patients in the denominator fulfill:
    # - have at least one post-baseline visit
    # - their baseline must not be abnormal if `exclude_base_abn`.
    if (exclude_base_abn) {
      denom_select <- !(df[[variables$baseline]] %in% abn)
    } else {
      denom_select <- TRUE
    }
    denom <- length(unique(df[denom_select, variables$id, drop = TRUE]))

    # Patients in the numerator fulfill:
    # - have at least one post-baseline visit with the required abnormality level
    # - are part of the denominator patients.
    num_select <- (df[[.var]] %in% abn) & denom_select
    num <- length(unique(df[num_select, variables$id, drop = TRUE]))

    with_label(c(num = num, denom = denom), abn_name)
  }

  # This will define the abnormal levels theoretically possible for a specific lab parameter
  # within a split level of a layout.
  abnormal_lev <- lapply(abnormal, intersect, levels(df[[.var]]))
  abnormal_lev <- abnormal_lev[vapply(abnormal_lev, function(x) length(x) > 0, integer(1))]

  result <- sapply(names(abnormal_lev), function(i) count_abnormal_single(i, abnormal_lev[[i]]), simplify = FALSE)
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
#' a_fun(df, .var = "ANRIND", abnormal = list(low = "LOW", high = "HIGH"))
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
#'   count_abnormal(var = "ANRIND", abnormal = list(high = "HIGH", low = "LOW")) %>%
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
#'     abnormal = list(low = "LOW", high = "HIGH"),
#'     variables = list(id = "ID", baseline = "BL_RANGE")
#'   ) %>%
#'   build_table(df2)
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
  assertthat::assert_that(
    assertthat::is.string(var)
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
