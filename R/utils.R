#' Re-implemented \code{\link[base:range]{range.default}} default S3 method for numerical objects only.
#' It returns \code{c(NA, NA)} instead of \code{c(-Inf, Inf)} for zero-length data
#' without any warnings.
#'
#' @param x numeric S3 class, a sequence of numbers for which the range is computed.
#' @param na.rm (`logical`)\cr indicating if NA should be omitted.
#' @param finite (`logical`)\cr indicating if non-finite elements should be removed.
#'
#' @return 2-elements vector of class numeric.
#'
#' @export
#'
#' @examples
#' range_noinf(1:5)
#' range_noinf(c(1:5, NA, NA), na.rm = TRUE)
#' range_noinf(numeric(), na.rm = TRUE)
#' range_noinf(c(1:5, NA, NA, Inf), na.rm = TRUE, finite = TRUE)
#' range_noinf(Inf)
#' range_noinf(Inf, na.rm = TRUE, finite = TRUE)
#' range_noinf(c(Inf, NA), na.rm = FALSE, finite = TRUE)
#' range_noinf(c(1, Inf, NA), na.rm = FALSE, finite = TRUE)
#'
range_noinf <- function(x, na.rm = FALSE, finite = FALSE) { # nolint

  assert_that(is.numeric(x), msg = "Argument x in range_noinf function must be of class numeric.")

  if (finite) {
    x <- x[is.finite(x)] # removes NAs too
  } else if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    rval <- c(NA, NA)
    mode(rval) <- typeof(x)
  }
  else {
    rval <- c(min(x, na.rm = FALSE), max(x, na.rm = FALSE))
  }

  return(rval)

}

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
#' library(scda)
#' adae <- synthetic_cdisc_data("latest")$adae
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

#' Smooth Function with Optional Grouping
#'
#' This produces \code{loess} smoothed estimates of `y` with Student confidence intervals.
#'
#' @param df (`data.frame`)\cr.
#' @param x (`character`)\cr value with x column name.
#' @param y (`character`)\cr value with y column name.
#' @param groups (`character`)\cr vector with optional grouping variables names.
#' @param level (`numeric`) level of confidence interval to use (0.95 by default).
#' @return A `data.frame` with original `x`, smoothed `y`, `ylow`, `yhigh` and
#' optional `groups` variables formatted to factor type.
#'
get_smooths <- function(df, x, y, groups = NULL, level = 0.95) {
  assert_that(is.data.frame(df))
  df_cols <- colnames(df)
  assert_that(is.string(x) && (x %in% df_cols) && is.numeric(df[[x]]))
  assert_that(is.string(y) && (y %in% df_cols) && is.numeric(df[[y]]))
  assert_that(is.null(groups) || (is.character(groups) && all(groups %in% df_cols)))

  smooths <- function(x, y) {
    predict(stats::loess(y ~ x), se = TRUE)
  }

  if (!is.null(groups)) {
    cc <- complete.cases(df[c(x, y, groups)])
    df_c <- df[cc, c(x, y, groups)]
    df_c_ordered <- df_c[do.call("order", as.list(df_c[, groups, drop = FALSE])), ]
    df_c_g <- data.frame(Map(as.factor, df_c_ordered[groups]))

    df_smooth_raw <-
      by(df_c_ordered, df_c_g, function(d) {
        plx <- smooths(d[[x]], d[[y]])
        data.frame(
          x = d[[x]],
          y = plx$fit,
          ylow = plx$fit - qt(level, plx$df) * plx$se,
          yhigh = plx$fit + qt(level, plx$df) * plx$se
        )
      })

    df_smooth <- do.call(rbind, df_smooth_raw)
    df_smooth[groups] <- df_c_g

    df_smooth
  } else {
    cc <- complete.cases(df[c(x, y)])
    df_c <- df[cc, ]
    plx <- smooths(df_c[[x]], df_c[[y]])

    df_smooth <- data.frame(
      x = df_c[[x]],
      y = plx$fit,
      ylow = plx$fit - qt(level, plx$df) * plx$se,
      yhigh = plx$fit + qt(level, plx$df) * plx$se
    )

    df_smooth
  }
}

#' Number of Available (Non-Missing Entries) in a Vector
#'
#' Small utility function for better readability.
#'
#' @param x (`vector`)\cr where to count the non-missing values.
#'
#' @return Number of non-missing values.
#' @export
#'
#' @examples
#' n_available(c(1, NA, 2))
#'
n_available <- function(x) {
  sum(!is.na(x))
}

# used for tests at the moments
reapply_varlabels <- function(x, varlables, ...) { # nolintr # nousage
  do.call(var_relabel, c(list(x = x), as.list(varlables), list(...)))
}
