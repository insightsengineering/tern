

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
t_summary <- function(x, col_by, col_N, ...) {
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
#' 
t_summary.default <- function(x, col_by, col_N = table(col_by), ...) {
  
  tbl <- rtable(
    rrowl(paste("no t_summary method for class:", class(x)), lapply(levels(col_by), function(x)rcell("-")))
  )
  header_add_N(tbl, col_N)
  
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
#' t_summary(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, col_N = table(c(ASL$ARM, ASL$ARM)), 
#'                        total = "All Patients", useNA = 'always')
#' 
#' ASL$SEX[1:10] <- NA
#' 
#' t_summary(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients",
#'                       useNA = 'ifany')
#' t_summary(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients",
#'                       denominator = "N", useNA = 'ifany')
#'                       
#' # with iris data
#' 
#' t_summary(iris[, -5], col_by  = iris$Species)
#' 
#' t_summary(iris, col_by  = no_by("All Species"), col_N = nrow(iris))
#' 
#' 
#' x <- factor(c("A", NA, "B", "B", "A"))
#' cb <- factor(c("I", "I", "I", "II", "II"))
#' 
#' t_summary(x, col_by = cb)
#' 
#' t_summary(data.frame(x), col_by = cb, useNA = "ifany")
#' 
t_summary.data.frame <- function(x, col_by, col_N=table(col_by), total = NULL, ...) {
  
  # If total column is requested stack the data and change col_by and col_N accordingly 
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")
    
    col_N <- c(col_N, sum(col_N)) 

    x <- duplicate_with_var(x)
    col_by <- factor(c(as.character(col_by), rep(total, length(col_by))), levels = c(levels(col_by), total))
  }

   rtables_vars <- Map(function(var, varlabel) {
     tbl <- t_summary(x=var, col_by=col_by, col_N = col_N, ...)
     insert_rrow(indent_table(tbl, 1), rrow(varlabel))
   }, x, var_labels(x, fill = TRUE))
  
   stack_rtables_l(rtables_vars)
}




#' Summarize Numeric Variables
#' 
#' Tabulate the number of non-missing observations, mean, sd, median, and range
#' for different groups.
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param x numeric variable
#' @param col_N  The column total for each group that is displayed in the table header with (N=xx).
#' @param total character string that will be used as a label for a column with pooled total population. If the levels of \code{col_by} are the only columns of interest then total should be \code{NULL}.
#' @template return_rtable
#' 
#' @export
#' @template author_waddella
#' 
#' @examples 
#' 
#' t_summary(iris$Sepal.Length, iris$Species)
#' 
#' t_summary(iris$Sepal.Length, iris$Species, col_N = table(iris$Species))
#' 
#' t_summary(iris$Sepal.Length, iris$Species, total = "All Species")
#' 
#' t_summary(iris$Sepal.Length, no_by("All Species"), col_N = length(iris$Sepal.Length) )
#'
t_summary.numeric <- function(x, col_by, col_N = table(col_by), total = NULL, ...) {
  
  if ( length(x)==length(col_by)){
    df <- data.frame(x = x, col_by = col_by)    
  }
  
  # If total column is requested stack the data and change by, col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")
    
    df <- duplicate_with_var(df, col_by = total)
    df$col_by <- factor(df$col_by, levels = c(levels(col_by), total))
    x <- df$x
    col_N <- c(col_N, sum(col_N)) 
    col_by <- df$col_by
  }
  
  tbl <- rbind(
    rtabulate(x, col_by, count_n, row.name = "n"),
    rtabulate(x, col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
    rtabulate(x, col_by, median, row.name = "Median", format="xx.xx", na.rm = TRUE),
    rtabulate(x, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", na.rm = TRUE)
  )
  
  header_add_N(tbl, col_N)
}

#' Summarize Categorical Data
#' 
#' Tabulate the number of non-missing observations, number of observations per level and
#' percentage.
#' 
#' @inheritParams t_summary.default
#' @param x numeric variable
#' @param col_N  The column total for each group that is displayed in the table header with (N=xx).
#' @param total character string that will be used as a label for a column with pooled total population. If the levels of \code{col_by} are the only columns of interest then total should be \code{NULL}.
#' @param useNA choose whether missing data (NAs) should be displayed as a level
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
#' @examples 
#' 
#' t_summary(iris$Species, iris$Species)
#' 
#' t_summary(iris$Species, iris$Species, total = "All Species")
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
t_summary.factor <- function(x, col_by, col_N = table(col_by), total = NULL, useNA = c("no", "ifany", "always"),
                             denominator = c("n", "N"), drop_levels = FALSE, ...) {
  
  useNA <- match.arg(useNA)
  denominator <- match.arg(denominator)
  
  if (drop_levels) x <- droplevels(x)
  
  if ( length(x)==length(col_by)){
    df <- data.frame(x = x, col_by = col_by)    
  }
  
  # If total column is requested stack the data and change by, col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")
    
    df <- duplicate_with_var(df, col_by = total)
    df$col_by <- factor(df$col_by, levels = c(levels(col_by), total))
    x <- df$x
    col_N <- c(col_N, sum(col_N)) 
    col_by <- df$col_by
  }
  
  if (denominator == "n" & !is.no_by(col_by) ) {
    denom <- table(col_by[!is.na(x)])
  } else if (denominator == "n" & is.no_by(col_by) ){
    denom <- table(rep(col_by,  sum(!is.na(x)) ))
  } else {
    denom <- col_N
  }
  
  tbl <- rbind(
    rtabulate(as.numeric(x), col_by, count_n, row.name = "n"),
    rtabulate(
      x = x,
      col_by = col_by,
      FUN = function(x_cell, denom) {
        if (length(x_cell) > 0) length(x_cell) * c(1, 1/denom) else rcell("-", format = "xx")
      },
      format = "xx (xx.xx%)",
      col_wise_args = list(denom = denom)
    )
  )
  
  if (useNA == "always" || (useNA == "ifany" && any(is.na(x)))) {
    
    if(is.no_by(col_by)){
      NA_row <- sum(is.na(x))
    } else {
      NA_row <- tapply(x, col_by, function(x)sum(is.na(x)))
    }
    
    tbl <- insert_rrow(tbl, rrowl("<NA>", NA_row, format = "xx"), nrow(tbl)+1)
  }
  
  header_add_N(tbl, col_N)

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
t_summary.character <- function(x, col_by, col_N = table(col_by), total = NULL, ...) {
  t_summary(as.factor(x), col_by, col_N, total, ...)
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
#' t_summary(tenweeks, no_by("all"), length(tenweeks))
#' 
#' t_summary(tenweeks, factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)]))
#' 
#' t_summary(tenweeks, factor(LETTERS[c(1,1,1,2,2,3,3,3,4,4)]), total = "All")
#' 
t_summary.Date <- function(x, col_by, col_N = table(col_by), total = NULL,  ...) {
  
  if ( length(x)==length(col_by)){
    df <- data.frame(x = x, col_by = col_by)    
  }
  
  # If total column is requested stack the data and change by, col_by and col_N accordingly
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")
    
    df <- duplicate_with_var(df, col_by = total)
    df$col_by <- factor(df$col_by, levels = c(levels(col_by), total))
    x <- df$x
    col_N <- c(col_N, sum(col_N)) 
    col_by <- df$col_by
  }
  
  tbl <- rtables:::rtabulate_default(x, col_by, FUN = function(x) {
    paste(range(na.omit(x)), collapse = " to ")
  }, row.name = "range of dates", ...)
  
  header_add_N(tbl, col_N)
  
}

#' Summarize Boolean Data
#' 
#' 
#' @inheritParams t_summary.factor
#' @param x a logical vector
#' @param row.name.TRUE character string with row.name for TRUE summary
#' @param row.name.FALSE character string with row.name for FALSE summary
#' @param ... arguments passed on to \code{\link{t_summary.factor}}
#' 
#' @template author_waddella
#' 
#' @export
#' 
#' @examples 
#' t_summary(
#'   x = c(TRUE,FALSE,NA,TRUE,FALSE,FALSE,FALSE,TRUE),
#'   col_by = factor(LETTERS[c(1,1,1,2,2,3,3,2)])
#' )
#' 
#' t_summary(
#'   x = c(TRUE,FALSE,NA,TRUE,FALSE,FALSE,FALSE,TRUE),
#'   col_by = factor(LETTERS[c(1,1,1,2,2,3,3,2)]),
#'   denominator = "N"
#' )
#' 
#' t_summary(
#'   x = c(TRUE,FALSE,NA,TRUE,FALSE,FALSE,FALSE,TRUE),
#'   col_by = factor(LETTERS[c(1,1,1,2,2,3,3,2)]),
#'   denominator = "N",
#'   total = "All"
#' )
#' 
t_summary.logical <- function(x, col_by, col_N = table(col_by), total = NULL, row.name.TRUE = "TRUE", row.name.FALSE = "FALSE", ...) {
  
  xf <- factor(x, levels=c(TRUE, FALSE), labels=c(row.name.TRUE, row.name.FALSE))
  t_summary(xf, col_by, col_N, total, ...)
}
