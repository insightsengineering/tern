#' Missing data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Substitute missing data with a string or factor level.
#'
#' @param x (`factor` or `character`)\cr values for which any missing values should be substituted.
#' @param label (`string`)\cr string that missing data should be replaced with.
#' @param drop_na (`flag`)\cr if `TRUE` and `x` is a factor, any levels
#'   that are only `label` will be dropped.
#'
#' @return `x` with any `NA` values substituted by `label`.
#'
#' @examples
#' explicit_na(c(NA, "a", "b"))
#' is.na(explicit_na(c(NA, "a", "b")))
#'
#' explicit_na(factor(c(NA, "a", "b")))
#' is.na(explicit_na(factor(c(NA, "a", "b"))))
#'
#' explicit_na(sas_na(c("a", "")))
#'
#' explicit_na(factor(levels = c(NA, "a")))
#' explicit_na(factor(levels = c(NA, "a")), drop_na = TRUE) # previous default
#'
#' @export
explicit_na <- function(x, label = default_na_str(), drop_na = default_drop_na()) {
  checkmate::assert_string(label, na.ok = TRUE)
  checkmate::assert_flag(drop_na)

  if (is.factor(x)) {
    x <- forcats::fct_na_value_to_level(x, label)
    if (drop_na) {
      x <- forcats::fct_drop(x, only = label)
    }
  } else if (is.character(x)) {
    x[is.na(x)] <- label
  } else {
    stop("only factors and character vectors allowed")
  }

  x
}
#' @describeIn explicit_na should `NA` values without a dedicated level be dropped?
#'
#' @return
#' * `tern_default_drop_na`: (`flag`)\cr default value for `drop_na` argument in `explicit_na()`.
#'
#' @export
default_drop_na <- function() {
  getOption("tern_default_drop_na", default = TRUE)
}

#' @describeIn explicit_na Setter for default `NA` value replacement string. Sets the
#'   option `"tern_default_drop_na"` within the R environment.
#'
#' @return
#' * `tern_default_drop_na` has no return value.
#'
#' @export
set_default_drop_na <- function(drop_na) {
  checkmate::assert_flag(drop_na, null.ok = TRUE)
  options("tern_default_drop_na" = drop_na)
}

#' Convert strings to `NA`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' SAS imports missing data as empty strings or strings with whitespaces only. This helper function can be used to
#' convert these values to `NA`s.
#'
#' @inheritParams explicit_na
#' @param empty (`flag`)\cr if `TRUE`, empty strings get replaced by `NA`.
#' @param whitespaces (`flag`)\cr if `TRUE`, strings made from only whitespaces get replaced with `NA`.
#'
#' @return `x` with `""` and/or whitespace-only values substituted by `NA`, depending on the values of
#'   `empty` and `whitespaces`.
#'
#' @examples
#' sas_na(c("1", "", " ", "   ", "b"))
#' sas_na(factor(c("", " ", "b")))
#'
#' is.na(sas_na(c("1", "", " ", "   ", "b")))
#'
#' @export
sas_na <- function(x, empty = TRUE, whitespaces = TRUE) {
  checkmate::assert_flag(empty)
  checkmate::assert_flag(whitespaces)

  if (is.factor(x)) {
    empty_levels <- levels(x) == ""
    if (empty && any(empty_levels)) levels(x)[empty_levels] <- NA

    ws_levels <- grepl("^\\s+$", levels(x))
    if (whitespaces && any(ws_levels)) levels(x)[ws_levels] <- NA

    x
  } else if (is.character(x)) {
    if (empty) x[x == ""] <- NA_character_

    if (whitespaces) x[grepl("^\\s+$", x)] <- NA_character_

    x
  } else {
    stop("only factors and character vectors allowed")
  }
}
