#' Re-implemented [range()] Default S3 method for numerical objects
#'
#' This function returns `c(NA, NA)` instead of `c(-Inf, Inf)` for zero-length data
#' without any warnings.
#'
#' @param x (`numeric`)\cr a sequence of numbers for which the range is computed.
#' @param na.rm (`logical`)\cr indicating if `NA` should be omitted.
#' @param finite (`logical`)\cr indicating if non-finite elements should be removed.
#'
#' @return A 2-element vector of class `numeric`.
#'
#' @keywords internal
range_noinf <- function(x, na.rm = FALSE, finite = FALSE) { # nolint

  checkmate::assert_numeric(x)

  if (finite) {
    x <- x[is.finite(x)] # removes NAs too
  } else if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    rval <- c(NA, NA)
    mode(rval) <- typeof(x)
  } else {
    rval <- c(min(x, na.rm = FALSE), max(x, na.rm = FALSE))
  }

  return(rval)
}

#' Utility function to create label for confidence interval
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams argument_convention
#'
#' @return A `string`.
#'
#' @export
f_conf_level <- function(conf_level) {
  assert_proportion_value(conf_level)
  paste0(conf_level * 100, "% CI")
}

#' Utility function to create label for p-value
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param test_mean (`number`)\cr mean value to test under the null hypothesis.
#'
#' @return A `string`.
#'
#' @export
f_pval <- function(test_mean) {
  checkmate::assert_numeric(test_mean, len = 1)
  paste0("p-value (H0: mean = ", test_mean, ")")
}

#' Utility function to return a named list of covariate names.
#'
#' @param covariates (`character`)\cr a vector that can contain single variable names (such as
#'   `"X1"`), and/or interaction terms indicated by `"X1 * X2"`.
#'
#' @return A named `list` of `character` vector.
#'
#' @keywords internal
get_covariates <- function(covariates) {
  checkmate::assert_character(covariates)
  cov_vars <- unique(trimws(unlist(strsplit(covariates, "\\*"))))
  stats::setNames(as.list(cov_vars), cov_vars)
}

#' Replicate Entries of a Vector if Required
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Replicate entries of a vector if required.
#'
#' @inheritParams argument_convention
#' @param n (`count`)\cr how many entries we need.
#'
#' @return `x` if it has the required length already or is `NULL`,
#'   otherwise if it is scalar the replicated version of it with `n` entries.
#'
#' @note This function will fail if `x` is not of length `n` and/or is not a scalar.
#'
#' @export
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

#' Check Element Dimension
#'
#' Checks if the elements in `...` have the same dimension.
#'
#' @param ... (`data.frame`s or `vector`s)\cr any data frames/vectors.
#' @param omit_null (`logical`)\cr whether `NULL` elements in `...` should be omitted from the check.
#'
#' @return A `logical` value.
#'
#' @keywords internal
check_same_n <- function(..., omit_null = TRUE) {
  dots <- list(...)

  n_list <- Map(
    function(x, name) {
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
    dots, names(dots)
  )

  n <- stats::na.omit(unlist(n_list))

  if (length(unique(n)) > 1) {
    sel <- which(n != n[1])
    stop("Dimension mismatch:", paste(names(n)[sel], collapse = ", "), " do not have N=", n[1])
  }

  TRUE
}

#' Utility function to check if a float is equal to another float
#'
#' @description Uses `.Machine$double.eps` as the tolerance for the comparison.
#'
#' @param x (`float`)\cr A single number.
#' @param y (`float`)\cr A single number.
#'
#' @return `TRUE`, if identical. `FALSE`, otherwise
#'
#' @keywords internal
.is_equal_float <- function(x, y) {
  checkmate::assert_number(x)
  checkmate::assert_number(y)

  # Define a tolerance
  tolerance <- .Machine$double.eps

  # Check if x is close enough to y
  abs(x - y) < tolerance
}

#' Make Names Without Dots
#'
#' @param nams (`character`)\cr vector of original names.
#'
#' @return A `character` `vector` of proper names, which does not use dots in contrast to [make.names()].
#'
#' @keywords internal
make_names <- function(nams) {
  orig <- make.names(nams)
  gsub(".", "", x = orig, fixed = TRUE)
}

#' Conversion of Months to Days
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Conversion of Months to Days. This is an approximative calculation because it
#' considers each month as having an average of 30.4375 days.
#'
#' @param x (`numeric`)\cr time in months.
#'
#' @return A `numeric` vector with the time in days.
#'
#' @examples
#' x <- c(13.25, 8.15, 1, 2.834)
#' month2day(x)
#'
#' @export
month2day <- function(x) {
  checkmate::assert_numeric(x)
  x * 30.4375
}

#' Conversion of Days to Months
#'
#' @param x (`numeric`)\cr time in days.
#'
#' @return A `numeric` vector with the time in months.
#'
#' @examples
#' x <- c(403, 248, 30, 86)
#' day2month(x)
#'
#' @export
day2month <- function(x) {
  checkmate::assert_numeric(x)
  x / 30.4375
}

#' Return an empty numeric if all elements are `NA`.
#'
#' @param x (`numeric`)\cr vector.
#'
#' @return An empty `numeric` if all elements of `x` are `NA`, otherwise `x`.
#'
#' @examples
#' x <- c(NA, NA, NA)
#' # Internal function - empty_vector_if_na
#' @keywords internal
empty_vector_if_na <- function(x) {
  if (all(is.na(x))) {
    numeric()
  } else {
    x
  }
}

#' Combine Two Vectors Element Wise
#'
#' @param x (`vector`)\cr first vector to combine.
#' @param y (`vector`)\cr second vector to combine.
#'
#' @return A `list` where each element combines corresponding elements of `x` and `y`.
#'
#' @examples
#' combine_vectors(1:3, 4:6)
#'
#' @export
combine_vectors <- function(x, y) {
  checkmate::assert_vector(x)
  checkmate::assert_vector(y, len = length(x))

  result <- lapply(as.data.frame(rbind(x, y)), `c`)
  names(result) <- NULL
  result
}

#' Extract Elements by Name
#'
#' This utility function extracts elements from a vector `x` by `names`.
#' Differences to the standard `[` function are:
#'
#' - If `x` is `NULL`, then still always `NULL` is returned (same as in base function).
#' - If `x` is not `NULL`, then the intersection of its names is made with `names` and those
#'   elements are returned. That is, `names` which don't appear in `x` are not returned as `NA`s.
#'
#' @param x (named `vector`)\cr where to extract named elements from.
#' @param names (`character`)\cr vector of names to extract.
#'
#' @return `NULL` if `x` is `NULL`, otherwise the extracted elements from `x`.
#'
#' @keywords internal
extract_by_name <- function(x, names) {
  if (is.null(x)) {
    return(NULL)
  }
  checkmate::assert_named(x)
  checkmate::assert_character(names)
  which_extract <- intersect(names(x), names)
  if (length(which_extract) > 0) {
    x[which_extract]
  } else {
    NULL
  }
}

#' Labels for Adverse Event Baskets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param aesi (`character`)\cr with standardized `MedDRA` query name (e.g. `SMQzzNAM`) or customized query
#'   name (e.g. `CQzzNAM`).
#' @param scope (`character`)\cr with scope of query (e.g. `SMQzzSC`).
#'
#' @return A `string` with the standard label for the `AE` basket.
#'
#' @examples
#' adae <- tern_ex_adae
#'
#' # Standardized query label includes scope.
#' aesi_label(adae$SMQ01NAM, scope = adae$SMQ01SC)
#'
#' # Customized query label.
#' aesi_label(adae$CQ01NAM)
#'
#' @export
aesi_label <- function(aesi, scope = NULL) {
  checkmate::assert_character(aesi)
  checkmate::assert_character(scope, null.ok = TRUE)
  aesi_label <- obj_label(aesi)
  aesi <- sas_na(aesi)
  aesi <- unique(aesi)[!is.na(unique(aesi))]

  lbl <- if (length(aesi) == 1 && !is.null(scope)) {
    scope <- sas_na(scope)
    scope <- unique(scope)[!is.na(unique(scope))]
    checkmate::assert_string(scope)
    paste0(aesi, " (", scope, ")")
  } else if (length(aesi) == 1 && is.null(scope)) {
    aesi
  } else {
    aesi_label
  }

  lbl
}

#' Indicate Study Arm Variable in Formula
#'
#' We use `study_arm` to indicate the study arm variable in `tern` formulas.
#'
#' @param x arm information
#'
#' @return `x`
#'
#' @keywords internal
study_arm <- function(x) {
  structure(x, varname = deparse(substitute(x)))
}

#' Smooth Function with Optional Grouping
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This produces `loess` smoothed estimates of `y` with Student confidence intervals.
#'
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param x (`character`)\cr value with x column name.
#' @param y (`character`)\cr value with y column name.
#' @param groups (`character`)\cr vector with optional grouping variables names.
#' @param level (`numeric`)\cr level of confidence interval to use (0.95 by default).
#'
#' @return A `data.frame` with original `x`, smoothed `y`, `ylow`, and `yhigh`, and
#'   optional `groups` variables formatted as `factor` type.
#'
#' @export
get_smooths <- function(df, x, y, groups = NULL, level = 0.95) {
  checkmate::assert_data_frame(df)
  df_cols <- colnames(df)
  checkmate::assert_string(x)
  checkmate::assert_subset(x, df_cols)
  checkmate::assert_numeric(df[[x]])
  checkmate::assert_string(y)
  checkmate::assert_subset(y, df_cols)
  checkmate::assert_numeric(df[[y]])

  if (!is.null(groups)) {
    checkmate::assert_character(groups)
    checkmate::assert_subset(groups, df_cols)
  }

  smooths <- function(x, y) {
    stats::predict(stats::loess(y ~ x), se = TRUE)
  }

  if (!is.null(groups)) {
    cc <- stats::complete.cases(df[c(x, y, groups)])
    df_c <- df[cc, c(x, y, groups)]
    df_c_ordered <- df_c[do.call("order", as.list(df_c[, groups, drop = FALSE])), , drop = FALSE]
    df_c_g <- data.frame(Map(as.factor, df_c_ordered[groups]))

    df_smooth_raw <-
      by(df_c_ordered, df_c_g, function(d) {
        plx <- smooths(d[[x]], d[[y]])
        data.frame(
          x = d[[x]],
          y = plx$fit,
          ylow = plx$fit - stats::qt(level, plx$df) * plx$se.fit,
          yhigh = plx$fit + stats::qt(level, plx$df) * plx$se.fit
        )
      })

    df_smooth <- do.call(rbind, df_smooth_raw)
    df_smooth[groups] <- df_c_g

    df_smooth
  } else {
    cc <- stats::complete.cases(df[c(x, y)])
    df_c <- df[cc, ]
    plx <- smooths(df_c[[x]], df_c[[y]])

    df_smooth <- data.frame(
      x = df_c[[x]],
      y = plx$fit,
      ylow = plx$fit - stats::qt(level, plx$df) * plx$se.fit,
      yhigh = plx$fit + stats::qt(level, plx$df) * plx$se.fit
    )

    df_smooth
  }
}

#' Number of Available (Non-Missing Entries) in a Vector
#'
#' Small utility function for better readability.
#'
#' @param x (`any`)\cr vector in which to count non-missing values.
#'
#' @return Number of non-missing values.
#'
#' @keywords internal
n_available <- function(x) {
  sum(!is.na(x))
}

#' Reapply Variable Labels
#'
#' This is a helper function that is used in tests.
#'
#' @param x (`vector`)\cr vector of elements that needs new labels.
#' @param varlabels (`character`)\cr vector of labels for `x`.
#' @param ... further parameters to be added to the list.
#'
#' @return `x` with variable labels reapplied.
#'
#' @export
reapply_varlabels <- function(x, varlabels, ...) {
  named_labels <- c(as.list(varlabels), list(...))
  formatters::var_labels(x)[names(named_labels)] <- as.character(named_labels)
  x
}

# Wrapper function of survival::clogit so that when model fitting failed, a more useful message would show
clogit_with_tryCatch <- function(formula, data, ...) { # nolint
  tryCatch(
    survival::clogit(formula = formula, data = data, ...),
    error = function(e) stop("model not built successfully with survival::clogit")
  )
}
