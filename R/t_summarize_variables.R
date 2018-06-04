

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
      rbind(
        rtabulate(var, col_by, count_n, row.name = "n", indent = 1),
        rtabulate(var, col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)", indent = 1),
        rtabulate(var, col_by, median, row.name = "Median", indent = 1, na.rm = TRUE),
        rtabulate(var, col_by, range, format = "xx.xx - xx.xx", row.name = "Min - Max", indent = 1, na.rm = TRUE)
      )
    } else {
      # treat as factor
      var_fct <- if (drop_levels) droplevels(as.factor(var)) else as.factor(var)
      rbind(
        rtabulate(as.numeric(var_fct), col_by, count_n, row.name = "n", indent = 1, na.rm = useNA_factors == "no"),
        rtabulate(
          x = var_fct,
          col_by = col_by,
          FUN = function(x_cell, x_row, x_col) {
            if (length(x_col) > 0) length(x_cell) * c(1, 1/sum(!is.na(x_col))) else rcell("-", format = "xx")
          },
          row_col_data_args = TRUE,
          useNA = useNA_factors,
          format = "xx (xx.xx%)",
          indent = 1
        )
      )
    }
    
    rbind(
      rtable(header = names(tbl_summary), rrow(row.name = varlabel)),
      tbl_summary
    )
  }, data, var_labels(data, fill = TRUE))
  
  tbl <- stack_rtables_l(rtables_vars)
  
  header(tbl) <- if (is(col_by, "no_by")) {
    rheader(
      rrow("", col_by),
      rrowl("", wrap_with(nrow(data), "(N=", ")"))
    )    
  } else {
    rheader(
      rrowl("", as.list(levels(col_by))),
      rrowl("", tapply(col_by, col_by, length), format = "(N=xx)")
    )
  }

  tbl
}
