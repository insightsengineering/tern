#' Formatting Functions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' See below for the list of formatting functions created in `tern` to work with `rtables`.
#'
#' Other available formats can be listed via [`formatters::list_valid_format_labels()`]. Additional
#' custom formats can be created via the [`formatters::sprintf_format()`] function.
#'
#' @family formatting functions
#' @name formatting_functions
NULL

#' Formatting Fraction and Percentage
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats a fraction together with ratio in percent.
#'
#' @param x (`integer`)\cr with elements `num` and `denom`.
#' @param ... required for `rtables` interface.
#'
#' @return A string in the format `num / denom (ratio %)`. If `num` is 0, the format is `num / denom`.
#'
#' @examples
#' format_fraction(x = c(num = 2L, denom = 3L))
#' format_fraction(x = c(num = 0L, denom = 3L))
#'
#' @family formatting functions
#' @export
format_fraction <- function(x, ...) {
  attr(x, "label") <- NULL

  checkmate::assert_vector(x)
  checkmate::assert_count(x["num"])
  checkmate::assert_count(x["denom"])

  result <- if (x["num"] == 0) {
    paste0(x["num"], "/", x["denom"])
  } else {
    paste0(
      x["num"], "/", x["denom"],
      " (", round(x["num"] / x["denom"] * 100, 1), "%)"
    )
  }

  return(result)
}

#' Formatting Fraction and Percentage with Fixed Single Decimal Place
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats a fraction together with ratio in percent with fixed single decimal place.
#' Includes trailing zero in case of whole number percentages to always keep one decimal place.
#'
#' @param x (`integer`)\cr with elements `num` and `denom`.
#' @param ... required for `rtables` interface.
#'
#' @return A string in the format `num / denom (ratio %)`. If `num` is 0, the format is `num / denom`.
#'
#' @examples
#' format_fraction_fixed_dp(x = c(num = 1L, denom = 2L))
#' format_fraction_fixed_dp(x = c(num = 1L, denom = 4L))
#' format_fraction_fixed_dp(x = c(num = 0L, denom = 3L))
#'
#' @family formatting functions
#' @export
format_fraction_fixed_dp <- function(x, ...) {
  attr(x, "label") <- NULL
  checkmate::assert_vector(x)
  checkmate::assert_count(x["num"])
  checkmate::assert_count(x["denom"])

  result <- if (x["num"] == 0) {
    paste0(x["num"], "/", x["denom"])
  } else {
    paste0(
      x["num"], "/", x["denom"],
      " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
    )
  }
  return(result)
}

#' Formatting Count and Fraction
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats a count together with fraction with special consideration when count is `0`.
#'
#' @param x (`integer`)\cr vector of length 2, count and fraction.
#' @param ... required for `rtables` interface.
#'
#' @return A string in the format `count (fraction %)`. If `count` is 0, the format is `0`.
#'
#' @examples
#' format_count_fraction(x = c(2, 0.6667))
#' format_count_fraction(x = c(0, 0))
#'
#' @family formatting functions
#' @export
format_count_fraction <- function(x, ...) {
  attr(x, "label") <- NULL

  if (any(is.na(x))) {
    return("NA")
  }

  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  assert_proportion_value(x[2], include_boundaries = TRUE)

  result <- if (x[1] == 0) {
    "0"
  } else {
    paste0(x[1], " (", round(x[2] * 100, 1), "%)")
  }

  return(result)
}

#' Formatting Count and Percentage with Fixed Single Decimal Place
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Formats a count together with fraction with special consideration when count is `0`.
#'
#' @param x (`integer`)\cr vector of length 2, count and fraction.
#' @param ... required for `rtables` interface.
#'
#' @return A string in the format `count (fraction %)`. If `count` is 0, the format is `0`.
#'
#' @examples
#' format_count_fraction_fixed_dp(x = c(2, 0.6667))
#' format_count_fraction_fixed_dp(x = c(2, 0.5))
#' format_count_fraction_fixed_dp(x = c(0, 0))
#'
#' @family formatting functions
#' @export
format_count_fraction_fixed_dp <- function(x, ...) {
  attr(x, "label") <- NULL

  if (any(is.na(x))) {
    return("NA")
  }

  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  assert_proportion_value(x[2], include_boundaries = TRUE)

  result <- if (x[1] == 0) {
    "0"
  } else if (x[2] == 1) {
    sprintf("%d (100%%)", x[1])
  } else {
    sprintf("%d (%.1f%%)", x[1], x[2] * 100)
  }

  return(result)
}

#' Formatting Count and Fraction with Special Case for Count < 10
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats a count together with fraction with special consideration when count is less than 10.
#'
#' @inheritParams format_count_fraction
#'
#' @return A string in the format `count (fraction %)`. If `count` is less than 10, only `count` is printed.
#'
#' @examples
#' format_count_fraction_lt10(x = c(275, 0.9673))
#' format_count_fraction_lt10(x = c(2, 0.6667))
#' format_count_fraction_lt10(x = c(9, 1))
#'
#' @family formatting functions
#' @export
format_count_fraction_lt10 <- function(x, ...) {
  attr(x, "label") <- NULL

  if (any(is.na(x))) {
    return("NA")
  }

  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  assert_proportion_value(x[2], include_boundaries = TRUE)

  result <- if (x[1] < 10) {
    paste0(x[1])
  } else {
    paste0(x[1], " (", round(x[2] * 100, 1), "%)")
  }

  return(result)
}

#' Formatting: XX as Formatting Function
#'
#' Translate a string where x and dots are interpreted as number place
#' holders, and others as formatting elements.
#'
#' @param str (`string`)\cr template.
#'
#' @return An `rtables` formatting function.
#'
#' @examples
#' test <- list(c(1.658, 0.5761), c(1e1, 785.6))
#'
#' z <- format_xx("xx (xx.x)")
#' sapply(test, z)
#'
#' z <- format_xx("xx.x - xx.x")
#' sapply(test, z)
#'
#' z <- format_xx("xx.x, incl. xx.x% NE")
#' sapply(test, z)
#'
#' @family formatting functions
#' @export
format_xx <- function(str) {
  # Find position in the string.
  positions <- gregexpr(pattern = "x+\\.x+|x+", text = str, perl = TRUE)
  x_positions <- regmatches(x = str, m = positions)[[1]]

  # Roundings depends on the number of x behind [.].
  roundings <- lapply(
    X = x_positions,
    function(x) {
      y <- strsplit(split = "\\.", x = x)[[1]]
      rounding <- function(x) {
        round(x, digits = ifelse(length(y) > 1, nchar(y[2]), 0))
      }
      return(rounding)
    }
  )

  rtable_format <- function(x, output) {
    values <- Map(y = x, fun = roundings, function(y, fun) fun(y))
    regmatches(x = str, m = positions)[[1]] <- values
    return(str)
  }

  return(rtable_format)
}

#' Formatting Numeric Values By Significant Figures
#'
#' Format numeric values to print with a specified number of significant figures.
#'
#' @param sigfig (`integer`)\cr number of significant figures to display.
#' @param format (`character`)\cr the format label (string) to apply when printing the value. Decimal
#'   places in string are ignored in favor of formatting by significant figures. Formats options are:
#'   `"xx"`, `"xx / xx"`, `"(xx, xx)"`, `"xx - xx"`, and `"xx (xx)"`.
#' @param num_fmt (`character`)\cr numeric format modifiers to apply to the value. Defaults to `"fg"` for
#'   standard significant figures formatting - fixed (non-scientific notation) format (`"f"`)
#'   and `sigfig` equal to number of significant figures instead of decimal places (`"g"`). See the
#'   [formatC()] `format` argument for more options.
#'
#' @return An `rtables` formatting function.
#'
#' @examples
#' fmt_3sf <- format_sigfig(3)
#' fmt_3sf(1.658)
#' fmt_3sf(1e1)
#'
#' fmt_5sf <- format_sigfig(5)
#' fmt_5sf(0.57)
#' fmt_5sf(0.000025645)
#'
#' @family formatting functions
#' @export
format_sigfig <- function(sigfig, format = "xx", num_fmt = "fg") {
  checkmate::assert_integerish(sigfig)
  format <- gsub("xx\\.|xx\\.x+", "xx", format)
  checkmate::assert_choice(format, c("xx", "xx / xx", "(xx, xx)", "xx - xx", "xx (xx)"))
  function(x, ...) {
    if (!is.numeric(x)) stop("`format_sigfig` cannot be used for non-numeric values. Please choose another format.")
    num <- formatC(signif(x, digits = sigfig), digits = sigfig, format = num_fmt, flag = "#")
    num <- gsub("\\.$", "", num) # remove trailing "."

    format_value(num, format)
  }
}

#' Formatting Fraction with Lower Threshold
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats a fraction when the second element of the input `x` is the fraction. It applies
#' a lower threshold, below which it is just stated that the fraction is smaller than that.
#'
#' @param threshold (`proportion`)\cr lower threshold.
#'
#' @return An `rtables` formatting function that takes numeric input `x` where the second
#'   element is the fraction that is formatted. If the fraction is above or equal to the threshold,
#'   then it is displayed in percentage. If it is positive but below the threshold, it returns,
#'   e.g. "<1" if the threshold is `0.01`. If it is zero, then just "0" is returned.
#'
#' @examples
#' format_fun <- format_fraction_threshold(0.05)
#' format_fun(x = c(20, 0.1))
#' format_fun(x = c(2, 0.01))
#' format_fun(x = c(0, 0))
#'
#' @family formatting functions
#' @export
format_fraction_threshold <- function(threshold) {
  assert_proportion_value(threshold)
  string_below_threshold <- paste0("<", round(threshold * 100))
  function(x, ...) {
    assert_proportion_value(x[2], include_boundaries = TRUE)
    ifelse(
      x[2] > 0.01,
      round(x[2] * 100),
      ifelse(
        x[2] == 0,
        "0",
        string_below_threshold
      )
    )
  }
}

#' Formatting Extreme Values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' `rtables` formatting functions that handle extreme values.
#'
#' @param digits (`integer`)\cr number of decimal places to display.
#'
#' @details For each input, apply a format to the specified number of `digits`. If the value is
#'    below a threshold, it returns "<0.01" e.g. if the number of `digits` is 2. If the value is
#'    above a threshold, it returns ">999.99" e.g. if the number of `digits` is 2.
#'    If it is zero, then returns "0.00".
#'
#' @family formatting functions
#' @name extreme_format
NULL

#' @describeIn extreme_format Internal helper function to calculate the threshold and create formatted strings
#'  used in Formatting Functions. Returns a list with elements `threshold` and `format_string`.
#'
#' @return
#' * `h_get_format_threshold()` returns a `list` of 2 elements: `threshold`, with `low` and `high` thresholds,
#'   and `format_string`, with thresholds formatted as strings.
#'
#' @examples
#' h_get_format_threshold(2L)
#'
#' @export
h_get_format_threshold <- function(digits = 2L) {
  checkmate::assert_integerish(digits)

  low_threshold <- 1 / (10 ^ digits) # styler: off
  high_threshold <- 1000 - (1 / (10 ^ digits)) # styler: off

  string_below_threshold <- paste0("<", low_threshold)
  string_above_threshold <- paste0(">", high_threshold)

  list(
    "threshold" = c(low = low_threshold, high = high_threshold),
    "format_string" = c(low = string_below_threshold, high = string_above_threshold)
  )
}

#' @describeIn extreme_format Internal helper function to apply a threshold format to a value.
#'   Creates a formatted string to be used in Formatting Functions.
#'
#' @param x (`number`)\cr value to format.
#'
#' @return
#' * `h_format_threshold()` returns the given value, or if the value is not within the digit threshold the relation
#'   of the given value to the digit threshold, as a formatted string.
#'
#' @examples
#' h_format_threshold(0.001)
#' h_format_threshold(1000)
#'
#' @export
h_format_threshold <- function(x, digits = 2L) {
  if (is.na(x)) {
    return(x)
  }

  checkmate::assert_numeric(x, lower = 0)

  l_fmt <- h_get_format_threshold(digits)

  result <- if (x < l_fmt$threshold["low"] && 0 < x) {
    l_fmt$format_string["low"]
  } else if (x > l_fmt$threshold["high"]) {
    l_fmt$format_string["high"]
  } else {
    sprintf(fmt = paste0("%.", digits, "f"), x)
  }

  unname(result)
}

#' Formatting a Single Extreme Value
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Create Formatting Function for a single extreme value.
#'
#' @inheritParams extreme_format
#'
#' @return An `rtables` formatting function that uses threshold `digits` to return a formatted extreme value.
#'
#' @examples
#' format_fun <- format_extreme_values(2L)
#' format_fun(x = 0.127)
#' format_fun(x = Inf)
#' format_fun(x = 0)
#' format_fun(x = 0.009)
#'
#' @family formatting functions
#' @export
format_extreme_values <- function(digits = 2L) {
  function(x, ...) {
    checkmate::assert_scalar(x, na.ok = TRUE)

    h_format_threshold(x = x, digits = digits)
  }
}

#' Formatting Extreme Values Part of a Confidence Interval
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formatting Function for extreme values part of a confidence interval. Values
#' are formatted as e.g. "(xx.xx, xx.xx)" if the number of `digits` is 2.
#'
#' @inheritParams extreme_format
#'
#' @return An `rtables` formatting function that uses threshold `digits` to return a formatted extreme
#'   values confidence interval.
#'
#' @examples
#' format_fun <- format_extreme_values_ci(2L)
#' format_fun(x = c(0.127, Inf))
#' format_fun(x = c(0, 0.009))
#'
#' @family formatting functions
#' @export
format_extreme_values_ci <- function(digits = 2L) {
  function(x, ...) {
    checkmate::assert_vector(x, len = 2)
    l_result <- h_format_threshold(x = x[1], digits = digits)
    h_result <- h_format_threshold(x = x[2], digits = digits)

    paste0("(", l_result, ", ", h_result, ")")
  }
}

#' Automatic formats from data significant digits
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formatting function for the majority of default methods used in [analyze_vars()].
#' For non-derived values, the significant digits of data is used (e.g. range), while derived
#' values have one more digits (measure of location and dispersion like mean, standard deviation).
#' This function can be called internally with "auto" like, for example,
#' `.formats = c("mean" = "auto")`. See details to see how this works with the inner function.
#'
#' @param dt_var (`numeric`) \cr all the data the statistics was created upon. Used only to find
#'   significant digits. In [analyze_vars] this comes from `.df_row` (see
#'   [rtables::additional_fun_params]), and it is the row data after the above row splits. No
#'   column split is considered.
#' @param x_stat (`string`) \cr string indicating the current statistical method used.
#'
#' @return A string that `rtables` prints in a table cell.
#'
#' @details
#' The internal function is needed to work with `rtables` default structure for
#' format functions, i.e. `function(x, ...)`, where is x are results from statistical evaluation.
#' It can be more than one element (e.g. for `.stats = "mean_sd"`).
#'
#' @examples
#' x_todo <- c(0.001, 0.2, 0.0011000, 3, 4)
#' res <- c(mean(x_todo[1:3]), sd(x_todo[1:3]))
#'
#' # x is the result coming into the formatting function -> res!!
#' format_auto(dt_var = x_todo, x_stat = "mean_sd")(x = res)
#' format_auto(x_todo, "range")(x = range(x_todo))
#' no_sc_x <- c(0.0000001, 1)
#' format_auto(no_sc_x, "range")(x = no_sc_x)
#'
#' @family formatting functions
#' @export
format_auto <- function(dt_var, x_stat) {
  function(x = "", ...) {
    checkmate::assert_numeric(x, min.len = 1)
    checkmate::assert_numeric(dt_var, min.len = 1)
    # Defaults - they may be a param in the future
    der_stats <- c(
      "mean", "sd", "se", "median", "geom_mean", "quantiles", "iqr",
      "mean_sd", "mean_se", "mean_se", "mean_ci", "mean_sei", "mean_sdi",
      "median_ci"
    )
    nonder_stats <- c("n", "range", "min", "max")

    # Safenet for miss-modifications
    stopifnot(length(intersect(der_stats, nonder_stats)) == 0) # nolint
    checkmate::assert_choice(x_stat, c(der_stats, nonder_stats))

    # Finds the max number of digits in data
    detect_dig <- vapply(dt_var, count_decimalplaces, FUN.VALUE = numeric(1)) %>%
      max()

    if (x_stat %in% der_stats) {
      detect_dig <- detect_dig + 1
    }

    # Render input
    str_vals <- formatC(x, digits = detect_dig, format = "f")
    def_fmt <- get_formats_from_stats(x_stat)[[x_stat]]
    str_fmt <- str_extract(def_fmt, invert = FALSE)[[1]]
    if (length(str_fmt) != length(str_vals)) {
      stop(
        "Number of inserted values as result (", length(str_vals),
        ") is not the same as there should be in the default tern formats for ",
        x_stat, " (-> ", def_fmt, " needs ", length(str_fmt), " values). ",
        "See tern_default_formats to check all of them."
      )
    }

    # Squashing them together
    inv_str_fmt <- str_extract(def_fmt, invert = TRUE)[[1]]
    stopifnot(length(inv_str_fmt) == length(str_vals) + 1) # nolint

    out <- vector("character", length = length(inv_str_fmt) + length(str_vals))
    is_even <- seq_along(out) %% 2 == 0
    out[is_even] <- str_vals
    out[!is_even] <- inv_str_fmt

    return(paste0(out, collapse = ""))
  }
}

# Utility function that could be useful in general
str_extract <- function(string, pattern = "xx|xx\\.|xx\\.x+", invert = FALSE) {
  regmatches(string, gregexpr(pattern, string), invert = invert)
}

# Helper function
count_decimalplaces <- function(dec) {
  if (abs(dec - round(dec)) > .Machine$double.eps^0.5) { # For precision
    nchar(strsplit(format(dec, scientific = FALSE, trim = FALSE), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Apply Auto Formatting
#'
#' Checks if any of the listed formats in `.formats` are `"auto"`, and replaces `"auto"` with
#' the correct implementation of `format_auto` for the given statistics, data, and variable.
#'
#' @inheritParams argument_convention
#' @param x_stats (named `list`)\cr a named list of statistics where each element corresponds
#'   to an element in `.formats`, with matching names.
#'
#' @keywords internal
apply_auto_formatting <- function(.formats, x_stats, .df_row, .var) {
  is_auto_fmt <- vapply(.formats, function(ii) is.character(ii) && ii == "auto", logical(1))
  if (any(is_auto_fmt)) {
    auto_stats <- x_stats[is_auto_fmt]
    var_df <- .df_row[[.var]] # xxx this can be extended for the WHOLE data or single facets
    .formats[is_auto_fmt] <- lapply(names(auto_stats), format_auto, dt_var = var_df)
  }
  .formats
}
