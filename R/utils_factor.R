
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
#' fct_discard(factor("a", "b", "c"), "c")
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
