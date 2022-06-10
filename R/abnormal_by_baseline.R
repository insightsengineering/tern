#' Patient Counts with Abnormal Range Values by Baseline Status
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details
#' Note that `df` should be filtered to include only post-baseline records.
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
#'
#' @inheritParams argument_convention
#' @param abnormal (`character`)\cr identifying the abnormal range level(s) in `.var`.
#'
#' @name abnormal_by_baseline
#' @include formats.R
NULL

#' @describeIn abnormal_by_baseline Description Function that produces the labels for [s_count_abnormal_by_baseline()].
#'
#' @examples
#' tern:::d_count_abnormal_by_baseline("LOW")
#'
#' @keywords internal
d_count_abnormal_by_baseline <- function(abnormal) {
  null_name <- paste0(toupper(substr(abnormal, 1, 1)), tolower(substring(abnormal, 2)))
  not_abn_name <- paste("Not", tolower(abnormal), "baseline status")
  abn_name <- paste(null_name, "baseline status")
  total_name <- "Total"

  list(
    not_abnormal = not_abn_name,
    abnormal = abn_name,
    total = total_name
  )
}

#' @describeIn abnormal_by_baseline For a single `abnormal` level, produce a statistic `fraction` which is
#'   a named list with 3 elements: `not_abnormal`, `abnormal` and `total`.
#'   Each element contains a vector with `num` and `denom` counts of patients.
#'   Please note that if the baseline variable or analysis variable contains `NA`, it is expected that `NA` has been
#'   conveyed to `na_level` appropriately beforehand with `df_explicit_na()` or `explicit_na()`.
#'
#' @param na_level (`string`) \cr the explicit `na_level` argument you used in the pre-processing steps (maybe with
#'   `df_explicit_na()`). The default is `"<Missing>"`.
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = as.character(c(1:6)),
#'   ANRIND = factor(c(rep("LOW", 4), "NORMAL", "HIGH")),
#'   BNRIND = factor(c("LOW", "NORMAL", "HIGH", NA, "LOW", "NORMAL"))
#' )
#' df <- df_explicit_na(df)
#'
#' # Just for one abnormal level.
#' tern:::s_count_abnormal_by_baseline(df, .var = "ANRIND", abnormal = "HIGH")
#'
#' @keywords internal
s_count_abnormal_by_baseline <- function(df,
                                         .var,
                                         abnormal,
                                         na_level = "<Missing>",
                                         variables = list(id = "USUBJID", baseline = "BNRIND")) {
  assertthat::assert_that(
    assertthat::is.string(.var),
    assertthat::is.string(abnormal),
    assertthat::is.string(na_level),
    is.list(variables),
    all(names(variables) %in% c("id", "baseline")),
    is_df_with_variables(df, c(range = .var, variables)),
    is_character_or_factor(df[[variables$id]]),
    is_character_or_factor(df[[variables$baseline]]),
    is_character_or_factor(df[[.var]])
  )

  # If input is passed as character, changed to factor
  df[[.var]] <- as_factor_keep_attributes(df[[.var]], na_level = na_level)
  df[[variables$baseline]] <- as_factor_keep_attributes(df[[variables$baseline]], na_level = na_level)
  assertthat::assert_that(
    is_factor_no_na(df[[.var]]),
    is_factor_no_na(df[[variables$baseline]])
  )
  # Keep only records with valid analysis value.
  df <- df[df[[.var]] != na_level, ]

  anl <- data.frame(
    id = df[[variables$id]],
    var = df[[.var]],
    baseline = df[[variables$baseline]],
    stringsAsFactors = FALSE
  )

  # Total:
  #  - Patients in denominator: have at least one valid measurement post-baseline.
  #  - Patients in numerator: have at least one abnormality.
  total_denom <- length(unique(anl$id))
  total_num <- length(unique(anl$id[anl$var == abnormal]))

  # Baseline NA records are counted only in total rows.
  anl <- anl[anl$baseline != na_level, ]

  # Abnormal:
  #   - Patients in denominator: have abnormality at baseline.
  #   - Patients in numerator: have abnormality at baseline AND
  #     have at least one abnormality post-baseline.
  abn_denom <- length(unique(anl$id[anl$baseline == abnormal]))
  abn_num <- length(unique(anl$id[anl$baseline == abnormal & anl$var == abnormal]))

  # Not abnormal:
  #   - Patients in denominator: do not have abnormality at baseline.
  #   - Patients in numerator: do not have abnormality at baseline AND
  #     have at least one abnormality post-baseline.
  not_abn_denom <- length(unique(anl$id[anl$baseline != abnormal]))
  not_abn_num <- length(unique(anl$id[anl$baseline != abnormal & anl$var == abnormal]))

  labels <- d_count_abnormal_by_baseline(abnormal)
  list(fraction = list(
    not_abnormal = formatters::with_label(c(num = not_abn_num, denom = not_abn_denom), labels$not_abnormal),
    abnormal = formatters::with_label(c(num = abn_num, denom = abn_denom), labels$abnormal),
    total = formatters::with_label(c(num = total_num, denom = total_denom), labels$total)
  ))
}

#' @describeIn abnormal_by_baseline Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @return [a_count_abnormal_by_baseline()] returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' # Use the Formatted Analysis function for `analyze()`. We need to ungroup `fraction` first
#' # so that the `rtables` formatting function `format_fraction()` can be applied correctly.
#' afun <- make_afun(tern:::a_count_abnormal_by_baseline, .ungroup_stats = "fraction")
#' afun(df, .var = "ANRIND", abnormal = "LOW")
#'
#' @keywords internal
a_count_abnormal_by_baseline <- make_afun(
  s_count_abnormal_by_baseline,
  .formats = c(fraction = format_fraction)
)

#' @describeIn abnormal_by_baseline Layout creating function which can be used for creating tables, which can take
#'   statistics function arguments and additional format arguments (see below).
#'
#' @inheritParams argument_convention
#'
#' @export
#'
#' @examples
#'
#' # Layout creating function.
#' basic_table() %>%
#'   count_abnormal_by_baseline(var = "ANRIND", abnormal = c(High = "HIGH")) %>%
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
#'   count_abnormal_by_baseline(
#'     var = "RANGE",
#'     abnormal = c(Low = "LOW"),
#'     variables = list(id = "ID", baseline = "BLRANGE"),
#'     .formats = c(fraction = "xx / xx"),
#'     .indent_mods = c(fraction = 2L)
#'   ) %>%
#'   build_table(df2)
count_abnormal_by_baseline <- function(lyt,
                                       var,
                                       abnormal,
                                       ...,
                                       table_names = abnormal,
                                       .stats = NULL,
                                       .formats = NULL,
                                       .labels = NULL,
                                       .indent_mods = NULL) {
  assertthat::assert_that(
    assertthat::is.string(var),
    !is.null(names(abnormal)),
    is_equal_length(abnormal, table_names)
  )
  afun <- make_afun(
    a_count_abnormal_by_baseline,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "fraction"
  )
  for (i in seq_along(abnormal)) {
    abn <- abnormal[i]
    lyt <- analyze(
      lyt = lyt,
      vars = var,
      var_labels = names(abn),
      afun = afun,
      table_names = table_names[i],
      extra_args = c(list(abnormal = abn), list(...)),
      show_labels = "visible"
    )
  }
  lyt
}
