#' Combine Factor Levels
#'
#' Combine specified old factor Levels in a single new level.
#'
#' @param x factor
#' @param levels level names to be combined
#' @param new_level name of new level
#'
#' @return a factor
#'
#' @export
#'
#' @examples
#' x <- factor(letters[1:5], levels = letters[5:1])
#' tern:::combine_levels(x, levels = c('a', 'b') )
#'
#' tern:::combine_levels(x, c('e', 'b'))
#'
combine_levels <- function(x, levels, new_level = paste(levels, collapse = "/")) {
  stopifnot(
    is.factor(x),
    all(levels %in% levels(x))
  )

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
#' @param na_level (`string`)\cr the explicit missing level which should be used when
#'   converting a character vector.
#'
#' @return The factor with same attributes (except class) as `x`. Does not do any modifications
#'   if `x` is already a factor.
#'
#' @export
#'
#' @examples
#' as_factor_keep_attributes(with_label(c(1, 1, 2, 3), "id"))
#' as_factor_keep_attributes(c("a", "b", ""), "id")
#'
as_factor_keep_attributes <- function(x,
                                      x_name = deparse(substitute(x)),
                                      na_level = "<Missing>") {
  assert_that(
    is.atomic(x),
    is.string(x_name),
    is.string(na_level)
  )
  if (is.factor(x)) {
    return(x)
  }
  x_class <- class(x)[1]
  warning(paste(
    "automatically converting", x_class, "variable", x_name,
    "to factor, better manually convert to factor to avoid failures"
  ))
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
#' @return Character vector with labels in the format `[0%,20%]`, `(20%,50%]`, etc.
#' @export
#'
#' @examples
#' # Just pass the internal probability bounds, then 0 and 100% will be added automatically.
#' bins_percent_labels(c(0.2, 0.5))
#'
#' # Determine how to round.
#' bins_percent_labels(0.35224, digits = 1)
#'
#' # Passing an empty vector just gives a single bin 0-100%.
#' bins_percent_labels(c())
#'
bins_percent_labels <- function(probs,
                                digits = 0) {
  assert_that(is_quantiles_vector(probs, include_boundaries = FALSE))
  probs <- c(0, probs, 1)
  percent <- round(probs * 100, digits = digits)
  left <- paste0(head(percent, -1), "%")
  right <- paste0(tail(percent, -1), "%")
  without_left_bracket <- paste0(left, ",", right, "]")
  with_left_bracket <- paste0("[", head(without_left_bracket, 1))
  if (length(without_left_bracket) > 1) {
    with_left_bracket <- c(
      with_left_bracket,
      paste0("(", tail(without_left_bracket, -1))
    )
  }
  with_left_bracket
}

#' Cutting Numeric Vector into Empirical Quantile Bins
#'
#' This cuts a numeric vector into sample quantile bins. Note that the intervals are closed on
#' the right side. That is, the first bin is the interval `[-Inf, q1]` where `q1` is
#' the first quantile, the second bin is then `(q1, q2]`, etc., and the last bin
#' is `(qn, +Inf]` where `qn` is the last quantile.
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
#' @return The factor variable with the appropriately labeled bins as levels.
#' @export
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
cut_quantile_bins <- function(x,
                              probs = c(0.25, 0.5, 0.75),
                              labels = bins_percent_labels(probs),
                              type = 7,
                              ordered = TRUE) {
  assert_that(
    is.numeric(x),
    is_quantiles_vector(probs, include_boundaries = FALSE),
    is_character_vector(labels, min_length = length(probs) + 1, max_length = length(probs) + 1),
    !any(duplicated(labels)),
    is.flag(ordered)
  )
  if (all(is.na(x))) {
    # Early return if there are only NAs in input.
    return(factor(x, ordered = ordered, levels = labels))
  }
  probs <- c(0, probs, 1)
  quantiles <- quantile(
    x,
    probs = probs,
    type = type,
    na.rm = TRUE
  )
  assert_that(
    !any(duplicated(quantiles)),
    msg = "Duplicate quantiles produced, please use a coarser `probs` vector"
  )
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
#' This discards the observations as well as the levels specified from a factor.
#'
#' @param x (`factor`)\cr the original factor.
#' @param discard (`character`)\cr which levels to discard.
#'
#' @return The modified factor with observations as well as levels from `discard` dropped.
#' @export
#'
#' @examples
#' fct_discard(factor(c("a", "b", "c")), "c")
#'
fct_discard <- function(x, discard) {
  assert_that(
    is.factor(x),
    is.character(discard),
    noNA(discard)
  )
  new_obs <- x[x != discard]
  new_levels <- setdiff(levels(x), discard)
  factor(new_obs, levels = new_levels)
}

#' Insertion of Explicit Missings in a Factor
#'
#' This inserts explicit missings in a factor based on a condition. Note that also additional
#' existing `NA` values will be explicitly converted to given `na_level`.
#'
#' @param x (`factor`)\cr the original factor.
#' @param condition (`logical`)\cr where to insert missings.
#' @param na_level (`string`)\cr which level to use for missings.
#'
#' @return The modified factor with inserted and existing `NA` converted to `na_level`.
#' @seealso [forcats::fct_explicit_na()] which is used internally.
#'
#' @importFrom forcats fct_explicit_na
#' @export
#'
#' @examples
#' fct_explicit_na_if(factor(c("a", "b", NA)), c(TRUE, FALSE, FALSE))
#'
fct_explicit_na_if <- function(x, condition, na_level = "<Missing>") {
  assert_that(
    is.factor(x),
    is.logical(condition),
    identical(length(x), length(condition))
  )
  x[condition] <- NA
  forcats::fct_explicit_na(x, na_level = na_level)
}

#' Collapsing of Factor Levels and Keeping Only Those New Group Levels
#'
#' This collapses levels and only keeps those new group levels, in the order provided.
#'
#' @param .f (`factor` or `character`)\cr original vector.
#' @param ... (named `character` vectors)\cr levels in each vector provided will be collapsed into
#'   the new level given by the respective name.
#'
#' @return The modified factor with collapsed levels. Values and levels which are not included
#'   in the given character vectors input are discarded. The returned factor has levels in the order
#'   given.
#' @seealso [forcats::fct_collapse()], [fct_discard()], [forcats::fct_relevel()] which are used
#'   internally.
#'
#' @export
#' @importFrom forcats fct_collapse fct_relevel
#'
#' @examples
#' fct_collapse_only(factor(c("a", "b", "c", "d")), TRT = "b", CTRL = c("c", "d"))
#'
fct_collapse_only <- function(.f, ...) {
  x <- forcats::fct_collapse(.f, ..., other_level = "..DISCARD..")
  x <- fct_discard(x, discard = "..DISCARD..")
  new_lvls <- names(list(...))
  do.call(forcats::fct_relevel, args = c(list(.f = x), as.list(new_lvls)))
}
