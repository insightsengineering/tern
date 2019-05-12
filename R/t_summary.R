#' Summarize an Object for Different Groups
#'
#' @inheritParams t_summary.data.frame
#' @param x an object to dispatch on
#' @param ... arguments passed on to methods
#'
#' @export
#'
#' @seealso \code{\link{t_summary.data.frame}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' t_summary(iris$Sepal.Length, iris$Species)
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL$AGE, ADSL$ARMCD)
#' t_summary(ADSL[, c("AGE", "SEX", "RACE")], ADSL$ARMCD)
#' with(ADSL, t_summary(AGE > 65, ARMCD))
t_summary <- function(x,
                      col_by,
                      col_N = table(col_by), # nolint
                      ...) {
  UseMethod("t_summary", x)
}

#' Return an \code{rtable} with a no Method Message
#'
#' If there is no explicit method for an object an \code{rtable} with one row with an
#' appropriate message is returned.
#'
#' @inheritParams t_summary.data.frame
#' @param ... not used arguments
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' t_summary(structure(1:5, class = "aaa"), factor(LETTERS[c(1,2,1,1,2)]))
t_summary.default <- function(x, # nolint
                              col_by,
                              col_N = table(col_by), # nolint
                              ...) {

  tbl <- rtable(
    header = levels(col_by),
    rrowl(paste("no t_summary method for class:", class(x)), lapply(levels(col_by), function(x)rcell("-")))
  )
  header_add_N(tbl, col_N)

}

#' Variables Summary Table
#'
#' Similar as the demographic table in STREAM
#'
#' @inheritParams argument_convention
#' @param x data frame with variables to be summarized as described in the
#'   details section. If the variable has a \code{label} attribute then it will
#'   be used for the row name.
#' @param ... arguments passed on to methods
#'
#' @details
#' Every variable in \code{x} will be mapped to a summary table using
#' \code{\link{t_summary}} and then be stacked.
#'
#' @export
#'
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' # with iris data
#' t_summary(iris$Sepal.Length, iris$Species)
#'
#' library(random.cdisc.data)
#' library(dplyr)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL[, c("SEX", "AGE")], col_by  = no_by("All"), col_N = nrow(ADSL))
#'
#' # control the label
#' ADSL <- var_relabel(ADSL, AGE = "Baseline Age of patient")
#'
#' # control categorical order
#' ADSL$SEX <- relevel(ADSL$SEX, "M", "F")
#'
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients")
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients", useNA = 'always')
#'
#' ADSL$SEX[1:10] <- NA
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients", useNA = 'ifany')
#' t_summary(ADSL[, c("SEX", "AGE")], col_by = ADSL$ARM, total = "All Patients", denominator = "N",
#'           useNA = 'ifany')
#'
#' ADSL_AGE65 <- ADSL %>%
#'   dplyr::filter(AGE > 65)
#' t_summary(ADSL_AGE65[, c("AGE", "SEX")], ADSL_AGE65$ARM, total = "All Patients",
#'           col_N = table(ADSL$ARM), denominator = "N", drop_levels = TRUE)
t_summary.data.frame <- function(x, # nolint
                                 col_by,
                                 col_N = table(col_by), # nolint
                                 total = NULL,
                                 ...) {

  # check argument validity and consitency
  check_col_by(col_by, col_N, min_num_levels = 1)

  # If total column is requested stack the data and change col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) {
    tmp <- add_total(x = x, col_by = col_by, total_level = total, col_N = col_N)
    x <- tmp$x
    col_by <- tmp$col_by
    col_N <- tmp$col_N # nolint
  }

   rtables_vars <- Map(function(var, varlabel) {
     tbl <- t_summary(x = var, col_by = col_by, col_N = col_N, ...)
     insert_rrow(indent_table(tbl, 1), rrow(varlabel))
   }, x, var_labels(x, fill = TRUE))

   rbindl_rtables(rtables_vars, gap = 1)
}

#' Summarize Numeric Variables
#'
#' Tabulate the number of non-missing observations, mean, sd, median, and range
#' for different groups.
#'
#' @inheritParams t_summary.data.frame
#' @param x numeric variable
#'
#' @template return_rtable
#'
#' @export
#'
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' # with iris data
#' t_summary(iris$Sepal.Length, iris$Species)
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL$AGE, ADSL$ARM)
#' t_summary(ADSL$AGE, ADSL$ARM, total = "All")
#'
#' ADSL$AGE[1:10] <- NA
#' t_summary(ADSL$AGE, no_by("All"), col_N = nrow(ADSL) )
t_summary.numeric <- function(x, # nolint
                              col_by,
                              col_N = table(col_by), # nolint
                              total = NULL,
                              ...) {

  # check argument validity and consitency
  check_col_by(col_by, col_N, min_num_levels = 1)

  if (!is.no_by(col_by)) {
    if (length(x) != length(col_by)) {
      stop("dimension missmatch x and col_by")
    }
  }

    # If total column is requested stack the data and change by, col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) {

    tmp <- add_total(x = x, col_by = col_by, total_level = total, col_N = col_N)
    x <- tmp$x
    col_by <- tmp$col_by
    col_N <- tmp$col_N # nolint
  }

  tbl <- rbind(
    rtabulate(x, col_by, count_n, row.name = "n"),
    rtabulate(x, col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
    rtabulate(x, col_by, median, row.name = "Median", format = "xx.xx", na.rm = TRUE),
    rtabulate(x, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", na.rm = TRUE)
  )

  header_add_N(tbl, col_N)
}

#' Summarize Categorical Data
#'
#' Tabulate the number of non-missing observations, number of observations per level and
#' percentage.
#'
#' @inheritParams t_summary.data.frame
#' @param x factor variable
#' @param useNA choose whether missing data (NAs) should be displayed as a level.
#' @param denominator either n or N for calculating the level associated
#'   percentage. With option N, the reference population from \code{col_N} is used as the denominator.
#'   With option n, the number of non-missing records from \code{x} is used as the denominator.
#' @param drop_levels boolean whether to drop zero count levels
#'
#' @template return_rtable
#'
#' @export
#' @template author_waddella
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.numeric}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#'
#' @examples
#' # with iris data
#' t_summary(iris$Species, iris$Species)
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' t_summary(ADSL$SEX, ADSL$ARM, total = "All")
#' t_summary(ADSL$SEX, ADSL$ARM, useNA = "always")
#'
#' ADSL$SEX[1:10] <- NA
#' t_summary(ADSL$SEX, ADSL$ARM, denominator = "N", useNA = "ifany", total = "All")
#' t_summary(ADSL$SEX, ADSL$ARM, denominator = "n", useNA = "no", total = "All")
t_summary.factor <- function(x, # nolint
                             col_by,
                             col_N = table(col_by), # nolint
                             total = NULL,
                             useNA = c("no", "ifany", "always"), # nolint
                             denominator = c("n", "N"),
                             drop_levels = FALSE, ...) {

  # check argument validity and consitency
  check_col_by(col_by, col_N, min_num_levels = 1)

  useNA <- match.arg(useNA) # nolint
  denominator <- match.arg(denominator)

  if (drop_levels) {
    x <- droplevels(x)
  }

  if (!is.no_by(col_by)) {
    if (length(x) != length(col_by)) {
      stop("dimension missmatch x and col_by")
    }
  }

  # If total column is requested stack the data and change col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) {

    tmp <- add_total(x = x, col_by = col_by, total_level = total, col_N = col_N)
    x <- tmp$x
    col_by <- tmp$col_by
    col_N <- tmp$col_N # nolint
  }

  if (denominator == "n" && !is.no_by(col_by)) {
    denom <- table(col_by[!is.na(x)])
  } else if (denominator == "n" && is.no_by(col_by)) {
    denom <- table(rep(col_by, sum(!is.na(x))))
  } else {
    denom <- col_N
  }

  tbl <- rbind(
    rtabulate(as.numeric(x), col_by, count_n, row.name = "n"),
    rtabulate(
      x = x,
      col_by = col_by,
      FUN = function(x_cell, denom) {
        if (length(x_cell) > 0) {
          length(x_cell) * c(1, 1 / denom)
        } else {
          rcell("-", format = "xx")
        }
      },
      format = "xx (xx.xx%)",
      col_wise_args = list(denom = denom)
    )
  )

  if (useNA == "always" || (useNA == "ifany" && any(is.na(x)))) {
    if (is.no_by(col_by)) {
      na_row <- sum(is.na(x))
    } else {
      na_row <- tapply(x, col_by, function(x) sum(is.na(x)))
    }
    tbl <- insert_rrow(tbl, rrowl("<NA>", na_row, format = "xx"), nrow(tbl) + 1)
  }

  header_add_N(tbl, col_N)
}

#' Summarize Character Data
#'
#' Currently treated as factors
#'
#' @inheritParams t_summary.data.frame
#' @param x a character vector
#' @param ... arguments passed on to \code{\link{t_summary.factor}}
#'
#' @template author_waddella
#'
#' @export
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary_by}}
t_summary.character <- function(x, # nolint
                                col_by,
                                col_N = table(col_by), # nolint
                                total = NULL,
                                ...) {
  t_summary(as.factor(x), col_by, col_N, total, ...)
}

#' Summarize Date Data
#'
#' Tabulate the range of dates.
#'
#' @inheritParams t_summary.data.frame
#' @param x a Date object
#'
#' @template author_waddella
#'
#' @export
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.numeric}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#'
#' @examples
#' today <- Sys.Date()
#' tenweeks <- seq(today, length.out=10, by="1 week")
#' t_summary(tenweeks, no_by("all"), length(tenweeks))
#'
#' t_summary(tenweeks, factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)]))
#'
#' t_summary(tenweeks, factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)]), total = "All")
t_summary.Date <- function(x, # nolint
                           col_by,
                           col_N = table(col_by), # nolint
                           total = NULL,  ...) {

  # check argument validity and consitency
  check_col_by(col_by, col_N, min_num_levels = 1)

  # If total column is requested stack the data and change by, col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) {
    tmp <- add_total(x = x, col_by = col_by, total_level = total, col_N = col_N)
    x <- tmp$x
    col_by <- tmp$col_by
    col_N <- tmp$col_N # nolint
  }

  tbl <- rtables:::rtabulate_default(x, col_by, FUN = function(x) {
    paste(range(na.omit(x)), collapse = " to ")
  }, row.name = "range of dates", ...)

  header_add_N(tbl, col_N)

}

#' Summarize Boolean Data
#'
#' Boolean data will be converted to factor
#'
#' @inheritParams t_summary.data.frame
#' @param x a logical vector
#' @param row_name_true character string with row.name for TRUE summary
#' @param row_name_false character string with row.name for FALSE summary
#' @param ... arguments passed on to \code{\link{t_summary.factor}}
#
#' @template author_waddella
#'
#' @export
#'
#' @seealso \code{\link{t_summary}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.data.frame}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}, \code{\link{t_summary_by}}
#'
#' @examples
#' t_summary(
#'  x = c(TRUE,FALSE,NA,TRUE,FALSE,FALSE,FALSE,TRUE),
#'  col_by = factor(LETTERS[c(1,1,1,2,2,3,3,2)])
#' )
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 100, seed = 1)
#'
#' ADSL$AGE[1:10] <- NA
#'
#' with(ADSL, t_summary(AGE > 65, ARM, useNA = "ifany"))
#' with(ADSL, t_summary(AGE > 65, ARM, denominator = "N", total = "All", useNA = "ifany",
#'                      row.name.TRUE = "Baseline Age > 65", row.name.FALSE = "Baseline Age <= 65"))
t_summary.logical <- function(x, # nolint
                              col_by,
                              col_N = table(col_by), # nolint
                              total = NULL,
                              row_name_true = "TRUE",
                              row_name_false = "FALSE",
                              ...) {

  xf <- factor(x, levels = c(TRUE, FALSE), labels = c(row_name_true, row_name_false))
  t_summary(xf, col_by, col_N, total, ...)
}
