

#' Variables Summary Table
#' 
#' Similiar as the demographic table in STREAM
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param data data frame with variables to be summarized as described in the
#'   details section. If the variable has a \code{label} attribute then it will
#'   be used for the row name.
#' @param total if not \code{NULL} then it must be a string and an addition
#'   column will be added with the overall summaries
#' @param drop_levels boolean, drop zero-count levels for factors
#' @param useNA_factors forwarded to \code{useNA} of
#'   \code{\link[rtables]{rtabulate.factor}}
#'  
#' @details
#' Every variable in \code{data} will be mapped to a summary table of that
#' variable and the tables for all variables will be stacked.
#' 
#' Currently we distinguish the summary by the variable type
#' 
#' \describe{
#'   \item{numneric}{\emph{n}, \emph{Mean (SD)}, \emph{Median}, \emph{Min-Max}
#'   will be derived for each factor in \code{col_by}}
#'   \item{non-numeric}{will be converted to a factor (if it isn't already) and
#'   the number subjects in a cell and the percentage within the \code{col_by}
#'   level will be calculated}
#' }
#' 
#' 
#' @export
#' 
#' @template author_waddella
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
#' t_summarize_variables(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients")
#' 
#' t_summarize_variables(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients",
#'                       useNA_factors = 'always')
#' 
#' ASL$SEX[1:10] <- NA
#' 
#' t_summarize_variables(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients",
#'                       useNA_factors = 'ifany')
#' 
#' # with iris data
#' 
#' t_summarize_variables(iris[, -5], col_by  = iris$Species)
#' 
#' t_summarize_variables(iris, col_by  = no_by("All Species"))
#' 
#' 
#' x <- factor(c("A", NA, "B", "B", "A"))
#' cb <- factor(c("I", "I", "I", "II", "II"))
#' 
#' t_summarize_variables(data.frame(x), col_by = cb)
#' 
#' t_summarize_variables(data.frame(x), col_by = cb, useNA_factors = "ifany")
#' 
t_summarize_variables <- function(data, col_by, total = NULL, drop_levels = TRUE,
                                  useNA_factors = c("no", "ifany", "always")) {

  useNA_factors <- match.arg(useNA_factors)
  
  # Check Arguments
  if (!is.data.frame(data)) stop("data is expected to be a data frame")  
  
  if (!is(col_by, "no_by")) {
    check_same_N(data = data, col_by = col_by, omit.NULL = FALSE)
    check_col_by(col_by, 1)
  } 

  # If total column is requested stack the data and change col_by accordingly 
  if (!is.null(total) && !is.no_by(col_by)) { ## add total column
    
    if (length(total) != 1) stop("total must be either NULL or a single string")
    if (total %in% col_by) stop("total cannot be an level in col_by")

    n <- nrow(data)
    lbls <- var_labels(data)
    data <- rbind(data, data)
    col_by <- factor(c(as.character(col_by), rep(total, n)), levels = c(levels(col_by), total))
    var_labels(data) <- lbls
  }
  
  rtables_vars <- Map(function(var, varlabel) {
    tbl_summary <- if (is.numeric(var)) {
      t_summarize_numeric(var, col_by)
    } else {
      # treat as factor
      var_fct <- if (drop_levels) droplevels(as.factor(var)) else as.factor(var)
      t_summarize_factor(var_fct, col_by, useNA = useNA_factors)
    }
    
    rbind(
      rtable(header = names(tbl_summary), rrow(row.name = varlabel)),
      indent_table(tbl_summary, 1)
    )
  }, data, var_labels(data, fill = TRUE))
  
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
#' t_summarize_numeric(iris$Sepal.Length, iris$Species)
#' 
t_summarize_numeric <- function(x, col_by) {
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
#' @inheritParams rtables::rtabulate.factor
#' @param x numeric variable
#' @param perc_denominator either n or N for calculating the level associated
#'   percentage.
#' 
#' @template return_rtable
#' 
#' @export
#' @template author_waddella
#' 
#' @examples 
#' 
#' t_summarize_factor(iris$Species, iris$Species)
#' 
#' 
#' x <-  factor(c("a", "b", "a", "a",  NA,  NA,  NA, "b", "b"))
#' cb <- factor(c("E", "E", "F", "F", "E", "E", "F", "F"))
#' 
#' t_summarize_factor(x, cb)
#' 
t_summarize_factor <- function(x, col_by, useNA = c("no", "ifany", "always"),
                               perc_denominator = c("n", "N")) {
  
  useNA <- match.arg(useNA)
  
  rbind(
    rtabulate(as.numeric(x), col_by, count_n, row.name = "n", na.rm = useNA == "no"),
    rtabulate(
      x = x,
      col_by = col_by,
      FUN = function(x_cell, x_row, x_col) {
        if (length(x_col) > 0) length(x_cell) * c(1, 1/sum(!is.na(x_col))) else rcell("-", format = "xx")
      },
      row_col_data_args = TRUE,
      useNA = useNA,
      format = "xx (xx.xx%)"
    )
  )
}


