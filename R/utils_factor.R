#' Combine Factor Levels
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Combine specified old factor Levels in a single new level.
#'
#' @param x factor
#' @param levels level names to be combined
#' @param new_level name of new level
#'
#' @return A `factor` with the new levels.
#'
#' @examples
#' x <- factor(letters[1:5], levels = letters[5:1])
#' combine_levels(x, levels = c("a", "b"))
#'
#' combine_levels(x, c("e", "b"))
#'
#' @export
combine_levels <- function(x, levels, new_level = paste(levels, collapse = "/")) {
  checkmate::assert_factor(x)
  checkmate::assert_subset(levels, levels(x))

  lvls <- levels(x)

  lvls[lvls %in% levels] <- new_level

  levels(x) <- lvls

  x
}

#' Conversion of a Vector to a Factor
#'
#' Converts `x` to a factor and keeps its attributes. Warns appropriately such that the user
#' can decide whether they prefer converting to factor manually (e.g. for full control of
#' factor levels).
#'
#' @param x (`atomic`)\cr object to convert.
#' @param x_name (`string`)\cr name of `x`.
#' @param na_level (`string`)\cr the explicit missing level which should be used when converting a character vector.
#' @param verbose defaults to `TRUE`. It prints out warnings and messages.
#'
#' @return A `factor` with same attributes (except class) as `x`. Does not modify `x` if already a `factor`.
#'
#' @keywords internal
as_factor_keep_attributes <- function(x,
                                      x_name = deparse(substitute(x)),
                                      na_level = "<Missing>",
                                      verbose = TRUE) {
  checkmate::assert_atomic(x)
  checkmate::assert_string(x_name)
  checkmate::assert_string(na_level)
  checkmate::assert_flag(verbose)
  if (is.factor(x)) {
    return(x)
  }
  x_class <- class(x)[1]
  if (verbose) {
    warning(paste(
      "automatically converting", x_class, "variable", x_name,
      "to factor, better manually convert to factor to avoid failures"
    ))
  }
  if (identical(length(x), 0L)) {
    warning(paste(
      x_name, "has length 0, this can lead to tabulation failures, better convert to factor"
    ))
  }
  if (is.character(x)) {
    x_no_na <- explicit_na(sas_na(x), label = na_level)
    if (any(na_level %in% x_no_na)) {
      do.call(
        structure,
        c(
          list(.Data = forcats::fct_relevel(x_no_na, na_level, after = Inf)),
          attributes(x)
        )
      )
    } else {
      do.call(structure, c(list(.Data = as.factor(x)), attributes(x)))
    }
  } else {
    do.call(structure, c(list(.Data = as.factor(x)), attributes(x)))
  }
}

#' Labels for Bins in Percent
#'
#' This creates labels for quantile based bins in percent. This assumes the right-closed
#' intervals as produced by [cut_quantile_bins()].
#'
#' @param probs (`proportion` vector)\cr the probabilities identifying the quantiles.
#'   This is a sorted vector of unique `proportion` values, i.e. between 0 and 1, where
#'   the boundaries 0 and 1 must not be included.
#' @param digits (`integer`)\cr number of decimal places to round the percent numbers.
#'
#' @return A `character` vector with labels in the format `[0%,20%]`, `(20%,50%]`, etc.
#'
#' @keywords internal
bins_percent_labels <- function(probs,
                                digits = 0) {
  if (isFALSE(0 %in% probs)) probs <- c(0, probs)
  if (isFALSE(1 %in% probs)) probs <- c(probs, 1)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, unique = TRUE, sorted = TRUE)
  percent <- round(probs * 100, digits = digits)
  left <- paste0(utils::head(percent, -1), "%")
  right <- paste0(utils::tail(percent, -1), "%")
  without_left_bracket <- paste0(left, ",", right, "]")
  with_left_bracket <- paste0("[", utils::head(without_left_bracket, 1))
  if (length(without_left_bracket) > 1) {
    with_left_bracket <- c(
      with_left_bracket,
      paste0("(", utils::tail(without_left_bracket, -1))
    )
  }
  with_left_bracket
}

#' Cutting Numeric Vector into Empirical Quantile Bins
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This cuts a numeric vector into sample quantile bins.
#'
#' @inheritParams bins_percent_labels
#' @param x (`numeric`)\cr the continuous variable values which should be cut into
#'   quantile bins. This may contain `NA` values, which are then
#'   not used for the quantile calculations, but included in the return vector.
#' @param labels (`character`)\cr the unique labels for the quantile bins. When there are `n`
#'   probabilities in `probs`, then this must be `n + 1` long.
#' @param type (`integer`)\cr type of quantiles to use, see [stats::quantile()] for details.
#' @param ordered (`flag`)\cr should the result be an ordered factor.
#'
#' @return A `factor` variable with appropriately-labeled bins as levels.
#'
#' @note Intervals are closed on the right side. That is, the first bin is the interval
#'   `[-Inf, q1]` where `q1` is the first quantile, the second bin is then `(q1, q2]`, etc.,
#'   and the last bin is `(qn, +Inf]` where `qn` is the last quantile.
#'
#' @examples
#' # Default is to cut into quartile bins.
#' cut_quantile_bins(cars$speed)
#'
#' # Use custom quantiles.
#' cut_quantile_bins(cars$speed, probs = c(0.1, 0.2, 0.6, 0.88))
#'
#' # Use custom labels.
#' cut_quantile_bins(cars$speed, labels = paste0("Q", 1:4))
#'
#' # NAs are preserved in result factor.
#' ozone_binned <- cut_quantile_bins(airquality$Ozone)
#' which(is.na(ozone_binned))
#' # So you might want to make these explicit.
#' explicit_na(ozone_binned)
#'
#' @export
cut_quantile_bins <- function(x,
                              probs = c(0.25, 0.5, 0.75),
                              labels = NULL,
                              type = 7,
                              ordered = TRUE) {
  checkmate::assert_flag(ordered)
  checkmate::assert_numeric(x)
  if (isFALSE(0 %in% probs)) probs <- c(0, probs)
  if (isFALSE(1 %in% probs)) probs <- c(probs, 1)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, unique = TRUE, sorted = TRUE)
  if (is.null(labels)) labels <- bins_percent_labels(probs)
  checkmate::assert_character(labels, len = length(probs) - 1, any.missing = FALSE, unique = TRUE)

  if (all(is.na(x))) {
    # Early return if there are only NAs in input.
    return(factor(x, ordered = ordered, levels = labels))
  }

  quantiles <- stats::quantile(
    x,
    probs = probs,
    type = type,
    na.rm = TRUE
  )

  checkmate::assert_numeric(quantiles, unique = TRUE)

  cut(
    x,
    breaks = quantiles,
    labels = labels,
    ordered_result = ordered,
    include.lowest = TRUE,
    right = TRUE
  )
}

#' Discard Certain Levels from a Factor
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This discards the observations as well as the levels specified from a factor.
#'
#' @param x (`factor`)\cr the original factor.
#' @param discard (`character`)\cr which levels to discard.
#'
#' @return A modified `factor` with observations as well as levels from `discard` dropped.
#'
#' @examples
#' fct_discard(factor(c("a", "b", "c")), "c")
#'
#' @export
fct_discard <- function(x, discard) {
  checkmate::assert_factor(x)
  checkmate::assert_character(discard, any.missing = FALSE)
  new_obs <- x[!(x %in% discard)]
  new_levels <- setdiff(levels(x), discard)
  factor(new_obs, levels = new_levels)
}

#' Insertion of Explicit Missings in a Factor
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This inserts explicit missings in a factor based on a condition. Additionally,
#' existing `NA` values will be explicitly converted to given `na_level`.
#'
#' @param x (`factor`)\cr the original factor.
#' @param condition (`logical`)\cr where to insert missings.
#' @param na_level (`string`)\cr which level to use for missings.
#'
#' @return A modified `factor` with inserted and existing `NA` converted to `na_level`.
#'
#' @seealso [forcats::fct_na_value_to_level()] which is used internally.
#'
#' @examples
#' fct_explicit_na_if(factor(c("a", "b", NA)), c(TRUE, FALSE, FALSE))
#'
#' @export
fct_explicit_na_if <- function(x, condition, na_level = "<Missing>") {
  checkmate::assert_factor(x, len = length(condition))
  checkmate::assert_logical(condition)
  x[condition] <- NA
  x <- forcats::fct_na_value_to_level(x, level = na_level)
  forcats::fct_drop(x, only = na_level)
}

#' Collapsing of Factor Levels and Keeping Only Those New Group Levels
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This collapses levels and only keeps those new group levels, in the order provided.
#' The returned factor has levels in the order given, with the possible missing level last (this will
#' only be included if there are missing values).
#'
#' @param .f (`factor` or `character`)\cr original vector.
#' @param ... (named `character` vectors)\cr levels in each vector provided will be collapsed into
#'   the new level given by the respective name.
#' @param .na_level (`string`)\cr which level to use for other levels, which should be missing in the
#'   new factor. Note that this level must not be contained in the new levels specified in `...`.
#'
#' @return A modified `factor` with collapsed levels. Values and levels which are not included
#'   in the given `character` vector input will be set to the missing level `.na_level`.
#'
#' @note Any existing `NA`s in the input vector will not be replaced by the missing level. If needed,
#'   [explicit_na()] can be called separately on the result.
#'
#' @seealso [forcats::fct_collapse()], [forcats::fct_relevel()] which are used internally.
#'
#' @examples
#' fct_collapse_only(factor(c("a", "b", "c", "d")), TRT = "b", CTRL = c("c", "d"))
#'
#' @export
fct_collapse_only <- function(.f, ..., .na_level = "<Missing>") {
  new_lvls <- names(list(...))
  if (checkmate::test_subset(.na_level, new_lvls)) {
    stop(paste0(".na_level currently set to '", .na_level, "' must not be contained in the new levels"))
  }
  x <- forcats::fct_collapse(.f, ..., other_level = .na_level)
  do.call(forcats::fct_relevel, args = c(list(.f = x), as.list(new_lvls)))
}

#' Ungroup Non-Numeric Statistics
#'
#' Ungroups grouped non-numeric statistics within input vectors `.formats`, `.labels`, and `.indent_mods`.
#'
#' @inheritParams argument_convention
#' @param x  (`named list` of `numeric`)\cr list of numeric statistics containing the statistics to ungroup.
#'
#' @return A `list` with modified elements `x`, `.formats`, `.labels`, and `.indent_mods`.
#'
#' @seealso [a_summary()] which uses this function internally.
#'
#' @keywords internal
ungroup_stats <- function(x,
                          .formats,
                          .labels,
                          .indent_mods) {
  checkmate::assert_list(x)
  empty_pval <- "pval" %in% names(x) && length(x[["pval"]]) == 0
  empty_pval_counts <- "pval_counts" %in% names(x) && length(x[["pval_counts"]]) == 0
  x <- unlist(x, recursive = FALSE)

  # If p-value is empty it is removed by unlist and needs to be re-added
  if (empty_pval) x[["pval"]] <- character()
  if (empty_pval_counts) x[["pval_counts"]] <- character()
  .stats <- names(x)

  # Ungroup stats
  .formats <- lapply(.stats, function(x) {
    .formats[[if (!grepl("\\.", x)) x else regmatches(x, regexpr("\\.", x), invert = TRUE)[[1]][1]]]
  })
  .indent_mods <- sapply(.stats, function(x) {
    .indent_mods[[if (!grepl("\\.", x)) x else regmatches(x, regexpr("\\.", x), invert = TRUE)[[1]][1]]]
  })
  .labels <- sapply(.stats, function(x) {
    if (!grepl("\\.", x)) .labels[[x]] else regmatches(x, regexpr("\\.", x), invert = TRUE)[[1]][2]
  })

  list(
    x = x,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
}
