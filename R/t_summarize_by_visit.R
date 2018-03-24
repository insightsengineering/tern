#' Summary Table by Visit
#' 
#' This function summarizes test results or change from baseline statistics by 
#' visit. Corresponds to STREAM table templates EGT01, VST01 and LBT01.
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param data data frame with numerical variables to be summarized. If the 
#'   variable has a \code{label} attribute then it will be used as the 
#'   sub-header column name.
#' @param visit factor with visit names ordered by desired display order in the 
#'   stacked table.
#' @template param_col_by
#'  
#' @template return_rtable
#' 
#' @template author_liaoc10
#'   
#' @details Every variable in \code{data} will be mapped to a sub-column under 
#'   \code{col_by} column. Multiple variables will be shown side-by-side with
#'   the variable label (if present) or variable name as the sub-column header.
#'   
#'   Currently \emph{n}, \emph{Mean (SD)}, \emph{Median}, \emph{IQR} and 
#'   \emph{Min-Max} will be derived for each variable from \code{data} per each 
#'   factor in \code{col_by}.
#'  
#' @importFrom utils stack
#' @export   
#'   
#' @examples 
#' 
#' # EXAMPLE 1
#' ANL <- expand.grid(
#'   USUBJID = paste0("p-",1:100),
#'   VISIT = paste0("visit ", 1:10),
#'   ARM = c("ARM A", "ARM B")   
#' )
#' ANL$AVAL <- rnorm(nrow(ANL))
#' ANL$CHG <- rnorm(nrow(ANL), 2, 2)
#' ANL$CHG[ANL$VISIT == "visit 1"] <- NA
#' ANL$PCHG <- ANL$CHG/ANL$AVAL*100
#' 
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' 
#' t_summarize_by_visit(data = ANL[c("AVAL")], visit = ANL$VISIT, col_by = ANL$ARM)
#' t_summarize_by_visit(data = ANL[c("PCHG")], visit = ANL$VISIT, col_by = ANL$ARM)
#' 
#' # Add label to variable instead showing variable name
#' ANL <- var_relabel(ANL, AVAL = "Value at\nVisit",
#'                         CHG = "Change from\nBaseline",
#'                         PCHG = "Percent Change\nfrom Baseline")
#' t_summarize_by_visit(data = ANL[c("AVAL", "CHG")], visit = ANL$VISIT, col_by = ANL$ARM)
#' 
#' 
#' # EXAMPLE 2
#' library(random.cdisc.data)
#' library(dplyr)
#' 
#' AQS <- radam("AQS") %>%
#'    filter(PARAMCD == "BFIALL") %>% 
#'    mutate(
#'      ARM = factor(sample(c("ARM A", "ARM B", "ARM C"), nrow(.), replace = TRUE)),
#'      AVISIT = factor(AVISIT, levels = unique(AVISIT))
#'    )
#'
#' tbl <- t_summarize_by_visit(data = AQS[c("AVAL", "CHG")], visit = AQS$AVISIT, col_by = AQS$ARM)
#' tbl
#' \dontrun{
#' Viewer(tbl)
#' }
#' 
t_summarize_by_visit <- function(data, visit, col_by) {
  
  # Check Arguments
  check_same_N(data = data, col_by = col_by, omit.NULL = TRUE)
  if (!is.data.frame(data)) stop("data is expected to be a data frame")
  check_is_factor(visit, allow_NA = FALSE)
  check_col_by(col_by, 1)
  vapply(data, check_is_numeric, logical(1))
  
  # Extracting variable metadata
  bigN <- tapply(col_by, col_by, length)
  topcol_label <- levels(col_by)
  topcol_n <- length(topcol_label)
  subcol_name <- names(data)
  subcol_n <- length(data)
  subcol_label <- var_labels(data, fill = TRUE)
  
  # Ensure no label attribute attached for stack function to work
  data <- var_labels_remove(data)
  
  # creating new data input, stack the variables in long-format
  stack_data <- stack(data)
  data <- stack_data$value
  
  # creating new visit input, stacked by as many times as number of variables specified in data
  visit <- rep(visit,subcol_n)
  
  # creating new col_by input, stacked by as many times as number of variables specified in data, releveled by arm and subcolumns
  topcol <- rep(col_by,subcol_n)
  new_colby <- paste(topcol, stack_data$ind, sep="-")
  col_by_levels <- paste(rep(topcol_label, each=subcol_n), rep(subcol_name, topcol_n), sep="-")
  col_by <- factor(new_colby, levels = col_by_levels)
    

  #Split into lists for each column
  df <- data.frame(data, col_by)
  df_byv <- split(df, visit)
  
  # Creating summary tables
  rtables_byv <- Map(function(dfi, visit_name) {
    tbl_byv <- rbind(
      rtable(header = levels(col_by), rrow(visit_name)),
      rtabulate(dfi$data, dfi$col_by, n_not_na3, row.name = "n", indent = 1),
      rtabulate(dfi$data, dfi$col_by, mean_sd3, format = "xx.xx (xx.xx)", row.name = "Mean (SD)", indent = 1),
      rtabulate(dfi$data, dfi$col_by, median_t3, row.name = "Median", indent = 1, format = "xx.xx"),
      rtabulate(dfi$data, dfi$col_by, iqr_num3, row.name = "IQR", indent = 1, format = "xx.xx - xx.xx"),
      rtabulate(dfi$data, dfi$col_by, range_t3, format = "xx.xx - xx.xx", row.name = "Min - Max", indent = 1)
    )
    tbl_byv
  }, df_byv, names(df_byv))
  
  tbl <- stack_rtables_l(rtables_byv)
  
  
  #Add top header, if more than one subcolumn add sub headers
  if (subcol_n == 1) {
    header(tbl) <- rheader(
      rrowl("", lapply(topcol_label, function(x) rcell(x, colspan = subcol_n))),
      rrowl("", lapply(wrap_with(bigN, "(N=", ")"), function(x) rcell(x, colspan = subcol_n)))
    )
  } else {
    header(tbl) <- rheader(
      rrowl("", lapply(topcol_label, function(x) rcell(x, colspan = subcol_n))),
      rrowl("", lapply(wrap_with(bigN, "(N=", ")"), function(x) rcell(x, colspan = subcol_n))),
      rrowl("", rep(subcol_label, topcol_n))
    )
  }
  
  tbl
  
}

