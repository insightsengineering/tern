

#' Table of Variable Summaries
#' 
#' Similiar as the demographic table in STREAM
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param data data frame with variables to be summarized as described in the
#'   details section. If the variable has a \code{label} attribute then it will
#'   be used for the row name.
#' @param total if not \code{NULL} then it must be a string and an addition
#'   column will be added with the overall summaries
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
#' @author Adrian Waddell \email{adrian.waddell@roche.com} and Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' 
#' # control the label
#' attr(ASL$BAGE, "label") <- "Baseline Age of patient"
#' 
#' # control categorical order
#' ASL$SEX <- relevel(ASL$SEX, "M", "F")
#' 
#' # control arm order
#' ASL$ARM <- relevel(ASL$ARM, "ARM B", "ARM A")
#' 
#' t_summarize_variables(ASL[, c("SEX", "BAGE")], col_by = ASL$ARM, total = "All Patients")
#' 
#' t_summarize_variables(iris[, -5], col_by  = iris$Species)
#' 
#' t_summarize_variables(iris, col_by  = no_by("All Species"))
#' 
t_summarize_variables <- function(data, col_by, total = NULL) {

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
    data <- rbind(data, data)
    col_by <- factor(c(as.character(col_by), rep(total, n)), levels = c(levels(col_by), total))
  }
  
  rtables_vars <- Map(function(var, varname) {
    tbl_summary <- if (is.numeric(var)) {
      rbind(
        rtabulate(var, col_by, length, row.name = "n", indent = 1),
        rtabulate(var, col_by, function(x) c(mean(x), sd(x)), format = "xx.xx (xx.xx)", row.name = "Mean (SD)", indent = 1),
        rtabulate(var, col_by, median, row.name = "Median", indent = 1),
        rtabulate(var, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", indent = 1)
      )
    } else {
      # treat as factor
      rtabulate(
        x = as.factor(var),
        col_by = col_by,
        FUN = function(x_cell, x_row, x_col) {
          if (length(x_col) > 0) length(x_cell) * c(1, 1/length(x_col)) else rcell("-", format = "xx")
        },
        row_col_data_args = TRUE,
        format = "xx (xx.xx%)",
        indent = 1
      )
    }
    
    # Add label row
    label <- attr(var, "label")
    if (is.null(label)) label <- varname
    
    rbind(
      rtable(header = names(tbl_summary), rrow(label)),
      tbl_summary
    )
  }, data, names(data))
  
  # now add empty rows
  Reduce(function(x,y) {rbind(x, rtable(header = names(x), rrow()), y)}, rtables_vars)
}
