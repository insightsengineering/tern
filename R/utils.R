#' Util function to create label for confidence interval
#'
#' @inheritParams argument_convention
#' @return a `string`
#' @examples
#' tern:::f_conf_level(0.95)
#' @noRd
#'
f_conf_level <- function(conf_level) {
  assert_that(is_proportion(conf_level))
  paste0(conf_level * 100, "% CI")
}

#' Utility function to return a named list of covariate names.
#'
#' @param covariates (`character`)\cr a vector that can contain single variable names (such as
#'   `"X1"`), and/or interaction terms indicated by `"X1 * X2"`.
#' @return a named `list` of character vector.
#'
#' @importFrom stats setNames
#'
get_covariates <- function(covariates) {
  assert_that(is.character(covariates))
  cov_vars <- unique(trimws(unlist(strsplit(covariates, "\\*"))))
  setNames(as.list(cov_vars), cov_vars)
}

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


#' Replicate Entries of a Vector if Required
#'
#' Note that this will fail if `x` is not having length `n` or being a scalar.
#'
#' @inheritParams argument_convention
#' @param n (`count`)\cr how many entries we need.
#'
#' @return Just input `x` if it has the required length already or is `NULL`,
#'   otherwise if it is scalar the replicated version of it with `n` entries.
#'
to_n <- function(x, n) {
  if (is.null(x)) {
    NULL
  } else if (length(x) == 1) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("dimension mismatch")
  }
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

#' Check an Arm Variable
#'
#' @inheritParams argument_convention
#' @param x (`data.frame` or `vector`)\cr input data.
#' @param col_N (`numeric`)\cr explicit column counts.
#' @param min_num_levels (`count`)\cr minimum number of levels that `col_by` needs to have.
#'
#' @return Nothing, just passes without error if everything is ok.
#'
check_col_by_factor <- function(x,
                                col_by,
                                col_N, # nolint
                                min_num_levels = 2) {
  stopifnot(
    is.factor(col_by),
    !any(is.na(col_by)) && !("" %in% levels(col_by)),
    length(col_N) == nlevels(col_by),
    nlevels(col_by) >= min_num_levels
  )
  if (is.data.frame(x)) {
    stopifnot(nrow(col_by) == nrow(x))
  } else {
    stopifnot(nrow(col_by) == length(x))
  }

  invisible(NULL)
}

#' Check Element Dimension
#'
#' Checks if the elements in `...` have the same dimension.
#'
#' @param ... data.frames or vectors
#' @param omit_null are \code{NULL} elements in \code{...} to be omitted from the check?
#'
#' @importFrom stats na.omit
#'
check_same_n <- function(..., omit_null = TRUE) {
  dots <- list(...)

  n_list <- Map(function(x, name) {
    if (is.null(x)) {
      if (omit_null) {
        NA_integer_
      } else {
        stop("arg", name, "is not supposed to be NULL")
      }
    } else if (is.data.frame(x)) {
      nrow(x)
    } else if (is.atomic(x)) {
      length(x)
    } else {
      stop("data structure for ", name, "is currently not supported")
    }
  },
  dots, names(dots))

  n <- na.omit(unlist(n_list))

  if (length(unique(n)) > 1) {
    sel <- which(n != n[1])
    stop("dimension mismatch:", paste(names(n)[sel], collapse = ", "), " do not have N=", n[1])
  }

  TRUE
}

#' Make Names Without Dots
#'
#' @param nams (`character`)\cr vector of original names.
#'
#' @return Character vector of proper names, which does not use dots in contrast to
#'   [base::make.names()].
#'
#' @export
#' @examples
#' make_names(c("foo Bar", "1 2 3 bla"))
#'
make_names <- function(nams) {
  orig <- make.names(nams)
  gsub(".", "", x = orig, fixed = TRUE)
}

#' Conversion of Months to Days
#'
#' @param x (`numeric`)\cr time in months.
#'
#' @return A `numeric` vector with the time in days.
#' @export
#' @examples
#' x <- c(13.25, 8.15, 1, 2.834)
#' month2day(x)
#'
month2day <- function(x) {
  assert_that(is_numeric_vector(x))
  x * 30.4375
}

#' Conversion of Days to Months
#'
#' @param x (`numeric`)\cr time in days.
#'
#' @return A `numeric` vector with the time in months.
#' @export
#' @examples
#' x <- c(403, 248, 30, 86)
#' day2month(x)
#'
day2month <- function(x) {
  assert_that(is_numeric_vector(x))
  x / 30.4375
}

#' Return an empty numeric if all elements are `NA`.
#'
#' @param x (`numeric`)\cr vector.
#'
#' @return An empty `numeric`.
#' @export
#' @examples
#' x <- c(NA, NA, NA)
#' empty_vector_if_na(x)
#'
empty_vector_if_na <- function(x) {
  if (all(is.na(x))) {
    numeric()
  } else {
    x
  }
}

#' Combine Two Vectors Elementwise
#'
#' @param x (`vector`)\cr first vector to combine.
#' @param y (`vector`)\cr second vector to combine.
#'
#' @return A `list` where each element combines corresponding elements of `x` and `y`.
#' @export
#' @examples
#' combine_vectors(1:3, 4:6)
#'
combine_vectors <- function(x, y) {

  assert_that(
    is.vector(x),
    is.vector(y),
    is_equal_length(x, y)
  )

  result <- lapply(as.data.frame(rbind(x, y)), `c`)
  names(result) <- NULL
  result
}

#' Extract Elements by Name
#'
#' This utility function extracts elements from a vector `x` by `names`.
#' Differences to the standard [base::`[`()] function are:
#'
#' - If `x` is `NULL`, then still always `NULL` is returned (same as in base function).
#' - If `x` is not `NULL`, then the intersection of its names is made with `names` and those
#'   elements are returned. That is, `names` which don't appear in `x` are not returned as `NA`s.
#'
#' @param x (named `vector`)\cr where to extract named elements from.
#' @param names (`character`)\cr vector of names to extract.
#'
#' @return Either `NULL` or the extracted elements from `x`.
#' @importFrom rlang is_named
#'
extract <- function(x, names) {
  if (is.null(x)) {
    return(NULL)
  }
  assert_that(
    is_named(x),
    is.character(names)
  )
  which_extract <- intersect(names(x), names)
  if (length(which_extract) > 0) {
    x[which_extract]
  } else {
    NULL
  }
}

#' Labels for Adverse Event Baskets
#'
#' @param aesi (`character`)\cr with standardized MedDRA query name (e.g. SMQzzNAM) or customized query
#'   name (e.g. CQzzNAM).
#' @param scope (`character`)\cr with scope of query (e.g. SMQzzSC).
#'
#' @return A `string` with the standard label for the AE basket.
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' adae <- radae(cached = TRUE)
#'
#' # Standardized query label includes scope.
#' aesi_label(adae$SMQ01NAM, scope = adae$SMQ01SC)
#'
#' # Customized query label.
#' aesi_label(adae$CQ01NAM)
#'
aesi_label <- function(aesi, scope = NULL) {

  assert_that(
    is.character(aesi),
    is.character(scope) || is.null(scope)
  )

  aesi_label <- obj_label(aesi)
  aesi <- sas_na(aesi)
  aesi <- unique(aesi)[!is.na(unique(aesi))]

  lbl <- if (length(aesi) == 1 & !is.null(scope)) {

    scope <- sas_na(scope)
    scope <- unique(scope)[!is.na(unique(scope))]
    assert_that(
      is.string(scope)
    )
    paste0(aesi, " (", scope, ")")

  } else if (length(aesi) == 1 & is.null(scope)) {
    aesi
  } else {
    aesi_label
  }

  lbl
}

#' Indicate Arm Variable in Formula
#'
#' We use `arm` to indicate the study arm variable in `tern` formulas.
#'
#' @param x arm information
#'
#' @export
#'
arm <- function(x) {
  structure(x, varname = deparse(substitute(x)))
}
