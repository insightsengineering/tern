#' Summary Table by Visit
#' 
#' This function summarizes test results or change from baseline statistics by 
#' visit. Corresponds to STREAM table templates EGT01, VST01 and LBT01.
#' 
#' @inheritParams rtables::rtabulate.numeric
#' @param data data frame with numerical variables to be summarized. If the 
#'   variable has a \code{label} attribute then it will be used as the column 
#'   name.
#' @param visit factor with visit names ordered by desired display order in the
#'   stacked table.
#' @template param_col_by
#'   
#'   
#' @export
#' 
#' @template return_rtable
#' @template author_liaoc10
#'   
#' @details
#' 
#' 
#' 
#' 
#' @examples 
#' 
#' ANL <- expand.grid(
#'   USUBJID = paste0("p-",1:100),
#'   VISIT = paste0("visit ", 1:10),
#'   ARM = c("ARM A", "ARM B")   
#' )
#' ANL$AVAL <- rnorm(nrow(ANL))
#' ANL$CHG <- rnorm(nrow(ANL), 2, 2)
#' ANL$CHG[ANL$VISIT == "visit 1"] <- NA
#' 
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' 
#' ANL <- var_relabel(ANL, AVAL = "Value at\nVisit", CHG = "Change from\nBaseline")
#' 
#' 
#' 
#' t_summarize_by_visit(data = ANL$AVAL, visit = ANL$VISIT, col_by = ANL$ARM)
#' 
#' t_summarize_by_visit(data = ANL$CHG, visit = ANL$VISIT, col_by = ANL$ARM)
#' 
#' arm <- as.character(ANL$ARM)
#' ARM <- factor(c(arm, paste0(arm,"-chng")), levels = c("ARM A", "ARM A-chng", "ARM B", "ARM B-chng"))
#' 
#' t_summarize_by_visit(data = c(ANL$AVAL, ANL$CHG), visit = rep(ANL$VISIT, 2), ARM)
#' 
t_summarize_by_visit <- function(data, visit, col_by) {
  
  # Check Arguments
#  check_same_N(data = data, col_by = col_by, omit.NULL = FALSE)
#  if (!is.data.frame(data)) stop("data is expected to be a data frame")  
#  if (!is.factor(visit)) stop("visit is expected to be a factor")
#  check_col_by(col_by, 1)
  
  df <- data.frame(data, col_by)
  df_byv <- split(df, visit)
  
  #dfi <- df_byv[[1]]
  #visit_name <- names(df_byv)[1]
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
  
  header(tbl) <- rheader(
    rrowl("", as.list(levels(col_by))),
    rrowl("", wrap_with(tapply(col_by, col_by, length), "(N=", ")"))
  )
  
  tbl
  
}

