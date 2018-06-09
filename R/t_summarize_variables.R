
#' Variables Summary Table
#' 
#' Similiar as the demographic table in STREAM. This funtion is deprecated,
#' please use \code{\link{t_summary}} instead.
#' 
#' @inheritParams t_summary.factor
#' @inheritParams t_summary.data.frame
#' @param data data frame with variables to be summarized as described in the
#'   details section. If the variable has a \code{label} attribute then it will
#'   be used for the row name.
#' @param useNA_factors forwarded to \code{useNA} of
#'   \code{\link{t_summary.factor}}
#'  
#' @details
#' Every variable in \code{data} will be mapped to a summary table of that
#' variable and the tables for all variables will be stacked.
#' 
#' Note that this function is deprecated, please use \code{\link{t_summary}}
#' instead.
#' 
#' @export
#' 
#' @template author_waddella
#' 
#' @seealso \code{\link{t_summary}}, \code{\link{t_summary.data.frame}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}
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
#' 
t_summarize_variables <- function(data, col_by, total = NULL, drop_levels = TRUE,
                                  useNA_factors = c("no", "ifany", "always")) {
  
  warning("t_summarize_variables is deprecated, please use t_summary instead.")
  useNA_factors <- match.arg(useNA_factors)
  
  # Check Arguments
  if (!is.data.frame(data)) stop("data is expected to be a data frame")  
  
  if (!is(col_by, "no_by")) {
    check_same_N(data = data, col_by = col_by, omit.NULL = FALSE)
    check_col_by(col_by, 1)
  } 
  
  t_summary(x = data, col_by = col_by, total = NULL, useNa = useNA_factors, drop_levels=drop_levels)
}