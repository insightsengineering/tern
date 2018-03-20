#' Summarize Information by Visit
#' 
#' @export
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
#' 
#' 
#' t_summarize_by_visit(x = ANL$AVAL, visit = ANL$VISIT, col_by = ANL$ARM)
#' 
#' t_summarize_by_visit(x = ANL$CHG, visit = ANL$VISIT, col_by = ANL$ARM)
#' 
#' arm <- as.character(ANL$ARM)
#' ARM <- factor(c(arm, paste0(arm,"-chng")), levels = c("ARM A", "ARM A-chng", "ARM B", "ARM B-chng"))
#' 
#' t_summarize_by_visit(x = c(ANL$AVAL, ANL$CHG), visit = rep(ANL$VISIT, 2), ARM)
#' 
t_summarize_by_visit <- function(x, visit, col_by) {
  
  if (!is.factor(visit)) stop("...")
  if (!is.factor(col_by)) stop("...")
  
  
  is_na <- is.na(x)
  x[is_na] <- -999
  
  df <- data.frame(x, col_by, is_na)
  df_s <- split(df, visit)
  

  iqr_num <- function(x) {
    quantile(x, probs = c(.25, .75))
  }


  # dfi <- df_s[[1]]
  # visit_name <- names(df_s)[1]
  tbls <- Map(function(dfi, visit_name) {
    tbl_i <- rbind(
      rtable(header = levels(col_by), rrow(visit_name)),
      rtabulate(dfi$x, dfi$col_by, n_not_na, row.name = "n", indent = 1),
      rtabulate(dfi$x, dfi$col_by, mean_sd, format = "xx.xx (xx.xx)", row.name = "Mean (SD)", indent = 1),
      rtabulate(dfi$x, dfi$col_by, median_t, row.name = "Median", indent = 1, format = "xx.xx"),
      rtabulate(dfi$x, dfi$col_by, iqr_num, row.name = "IQR", indent = 1, format = "xx.xx - xx.xx"),
      rtabulate(dfi$x, dfi$col_by, range_t, format = "xx.xx - xx.xx", row.name = "Min - Max", indent = 1)
    )
    dfi_s <- split(dfi, dfi$col_by)
    
    for (col_i in 1:length(dfi_s)) {
      cell_data <- dfi_s[[col_i]]
      if (all(cell_data$is_na)) {
        for (ir in 2:6) {
          tbl_i[[ir]][[col_i]] <- rcell(" ") 
        }
      }
    }
    
    tbl_i
  }, df_s, names(df_s))
  
  do.call(stack_rtables,tbls)
  
}

