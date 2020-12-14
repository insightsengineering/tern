#' Missing Data
#'
#' Substitute missing data with a string or factor level.
#'
#' @param x factor or character vector
#' @param label character string that missing data should be replaced with
#'
#' @importFrom forcats fct_explicit_na
#'
#' @export
#'
#' @examples
#'
#' explicit_na(c(NA, "a", "b"))
#' is.na(explicit_na(c(NA, "a", "b")))
#'
#' explicit_na(factor(c(NA, "a", "b")))
#' is.na(explicit_na(factor(c(NA, "a", "b"))))
#'
#' explicit_na(sas_na(c("a", "")))
#'
explicit_na <- function(x, label = "<Missing>") {
  stopifnot(is_character_single(label))

  if (is.factor(x)) {
    forcats::fct_explicit_na(x, label)
  } else if (is.character(x)) {
    x[is.na(x)] <- label
    x
  } else {
    stop("only factors and character vectors allowed")
  }
}


#' Convert Strings to `NA`
#'
#' SAS imports missing data as empty strings or strings with whitespaces only. This helper function can be used to
#' convert these values to \code{NA}s.
#'
#' @inheritParams explicit_na
#' @param empty boolean if true empty strings get replaced by \code{NA}
#' @param whitespaces boolean, if true then strings made from whitespaces only get replaced with \code{NA}
#'
#' @export
#'
#' @examples
#'
#' sas_na(c("1", "", " ", "   ", "b"))
#' sas_na(factor(c("", " ", "b")))
#'
#' is.na(sas_na(c("1", "", " ", "   ", "b")))
sas_na <- function(x, empty = TRUE, whitespaces = TRUE) {

  stopifnot(is_logical_single(empty), is_logical_single(whitespaces))

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
