

#' Summarize an Object for Different Groups
#' 
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param x an object to dispatch on
#' @param ... arguments passed on to methods
#' 
#' @details 
#' Note that N refers to the number of observations and n refers to the numer of
#' non-missing observations
#' 
#' @export
#' 
#' @examples 
#' t_summary(iris[-5], iris$Species)
#' 
#' t_summary(iris$Sepal.Length, iris$Species)
#' 
#' with(iris, t_summary(Sepal.Length > mean(Sepal.Length), iris$Species))
t_summary <- function(x, col_by, ...) {
  UseMethod("t_summary", x)
}

#' Return an rtable with a no Method Message
#' 
#' If there is no explicit method for an object an rtable with one row with an
#' appropriate message is returned.
#' 
#' @inheritParams t_summary
#' @param ... not used arguments
#' 
#' @export
#' 
#' @template author_waddella
#' 
#' @examples 
#' 
#' t_summary(structure(1:5, class = "aaa"), factor(LETTERS[c(1,2,1,1,2)]))
t_summary.default <- function(x, col_by, ...) {
  
  rtable(
    header = rtables:::rtabulate_header(col_by, length(x)),
    rrowl(paste("no t_summary method for class:", class(x)), lapply(levels(col_by), function(x)rcell("-")))
  )
}

#' Variables Summary Table
#' 
#' Similiar as the demographic table in STREAM
#' 
#' @inheritParams t_summary.default
#' @param x data frame with variables to be summarized as described in the
#'   details section. If the variable has a \code{label} attribute then it will
#'   be used for the row name.
#' @param total if not \code{NULL} then it must be a string and an addition
#'   column will be added with the overall summaries
#'  
#' @details
#' Every variable in \code{x} will be mapped to a summary table using
#' \code{\link{t_summary}} and then be stacked.
#' 
#' @export
#' 
#' @template author_waddella
#' @seealso \code{\link{t_summary}}, \code{\link{t_summary.factor}}, \code{\link{t_summary.numeric}}
#' 
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' 
#' # control the label
#' ASL <- var_relabel(ASL, BAGE = "Baseline Age of patient")
#' 
#' # control categorical order
#' ASL$SEX <- relevel(ASL$SEX, "M", "F")
#' 
#' t_summary(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients")
#' 
#' t_summary(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients",
#'                       useNA = 'always')
#' 
#' ASL$SEX[1:10] <- NA
#' 
#' t_summary(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients",
#'                       useNA_factors = 'ifany')
#' 
#' # with iris data
#' 
#' t_summary(iris[, -5], col_by  = iris$Species)
#' 
#' t_summary(iris, col_by  = no_by("All Species"))
#' 
#' 
#' x <- factor(c("A", NA, "B", "B", "A"))
#' cb <- factor(c("I", "I", "I", "II", "II"))
#' 
#' t_summary(x, col_by = cb)
#' 
#' t_summary(data.frame(x), col_by = cb, useNA = "ifany")
#' 
t_summary.data.frame <- function(x, col_by, total = NULL, ...) {
  
  # If total column is requested stack the data and change col_by accordingly 
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")
    
    x <- duplicate_with_var(x)
    col_by <- factor(c(as.character(col_by), rep(total, length(col_by))), levels = c(levels(col_by), total))
  }
  
  rtables_vars <- Map(function(var, varlabel) {
    tbl <- t_summary(x=var, col_by=col_by, ...) 
    insert_rrow(indent_table(tbl, 1), rrow(varlabel))
  }, x, var_labels(x, fill = TRUE))
  
  stack_rtables_l(rtables_vars)
}




#' Summarize Numeric Variables
#' 
#' Tabulate the number on non-missing observations, mean, sd, median, and range
#' for different groups.
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param x numeric variable
#' 
#' @template return_rtable
#' 
#' @export
#' @template author_waddella
#' 
#' @examples 
#' 
#' t_summary(iris$Sepal.Length, iris$Species)
#' 
t_summary.numeric <- function(x, col_by, ...) {
  rbind(
    rtabulate(x, col_by, count_n, row.name = "n"),
    rtabulate(x, col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
    rtabulate(x, col_by, median, row.name = "Median", na.rm = TRUE),
    rtabulate(x, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", na.rm = TRUE)
  )
}

#' Summarize Categorical Data
#' 
#' Tabulate the number on non-missing observations, number of per level and
#' percentage
#' 
#' @inheritParams t_summary.default
#' @param x numeric variable
#' @param useNA choose whether missing data (NAs) should be displayed as a level
#' @param denominator either n or N for calculating the level associated
#'   percentage.
#' 
#' @template return_rtable
#' 
#' @export
#' @template author_waddella
#' 
#' @examples 
#' 
#' t_summary(iris$Species, iris$Species)
#' 
#' 
#' x <-  factor(c("a", "b", "a", "a",  NA,  NA,  NA, "b", "b"))
#' cb <- factor(c("E", "E", "F", "F", "E", "E", "F", "F", "F"))
#' 
#' t_summary(x, cb)
#' 
#' t_summary(x, cb, useNA = "always")
#' 
#' t_summary(x, cb, denominator = "N")
#' 
#' t_summary(x, cb, useNA = "always",  denominator = "N")
#' 
#' t_summary(x, cb, useNA = "always",  denominator = "n")
#' 
t_summary.factor <- function(x, col_by, useNA = c("no", "ifany", "always"),
                             denominator = c("n", "N"), ...) {
  
  useNA <- match.arg(useNA)
  denominator <- match.arg(denominator)
  
  d <- if (denominator == "n") {
    function(x) sum(!is.na(x))
  } else {
    length
  }
  
  tbl <- rbind(
    rtabulate(as.numeric(x), col_by, count_n, row.name = "n"),
    rtabulate(
      x = x,
      col_by = col_by,
      FUN = function(x_cell, x_row, x_col) {
        if (length(x_col) > 0) length(x_cell) * c(1, 1/d(x_col)) else rcell("-", format = "xx")
      },
      row_col_data_args = TRUE,
      format = "xx (xx.xx%)"
    )
  )
  
  if (useNA == "always" || (useNA == "ifany" && any(is.na(x)))) {
    tbl <- insert_rrow(tbl, rrowl("<NA>", tapply(x, col_by, function(x)sum(is.na(x))), format = "xx"), nrow(tbl)+1)
  } 

  tbl
}

#' Summarize Character Data
#' 
#' Currently treated as for factors
#' 
#' @inheritParams t_summary.factor
#' @param x a character vector
#' @param ... arguments passed on to \code{\link{t_summary.factor}}
#' 
#' @template author_waddella
#' 
#' @export
t_summary.character <- function(x, col_by, ...) {
  t_summary(as.factor(x), col_by, ...)
}

#' Summarize Date Data
#' 
#' Currently treated as for factors
#' 
#' @inheritParams t_summary
#' @param x a Date object
#' @param ... arguments passed on to \code{\link{t_summary.factor}}
#' 
#' @template author_waddella
#' 
#' @export
#' 
#' @examples 
#' (today <- Sys.Date())
#' (tenweeks <- seq(today, length.out=10, by="1 week")) 
#' t_summary(tenweeks, no_by("all"))
#' 
#' t_summary(tenweeks, factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)]))
t_summary.Date <- function(x, col_by, ...) {
  rtables:::rtabulate_default(x, col_by, FUN = function(x) {
    paste(range(na.omit(x)), collapse = " to ")
  }, row.name = "range of dates", ...)
}

#' Summarize Boolean Data
#' 
#' 
#' @inheritParams t_summary.factor
#' @param x a factor
#' @param ... arguments passed on to \code{\link{t_summary.factor}}
#' 
#' @template author_waddella
#' 
#' @export
#' 
#' @examples 
#' t_summary(c(T,F,NA,T,F,F,F,T), factor(LETTERS[c(1,1,1,2,2,3,3,2)]))
#' 
#' t_summary(c(T,F,NA,T,F,F,F,T), factor(LETTERS[c(1,1,1,2,2,3,3,2)]), denominator = "N")
#' 
t_summary.logical <- function(x, col_by, row.name.TRUE = "TRUE", row.name.FALSE = "FALSE", denominator = c("n", "N"), ...) {

  denominator <- match.arg(denominator)
  
  rbind(
    rtabulate(x, col_by, count_n, row.name = "n", format="xx"),
    rtabulate(x, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name =  row.name.TRUE, na.rm = denominator == "n"),
    rtabulate(!x, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = row.name.FALSE, na.rm = denominator == "n")
  )
}
