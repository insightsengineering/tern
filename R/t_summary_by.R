#' Summarize an Object for Different Groups with by Variable
#' 
#' 
#' description
#' 
#' @inheritParams t_summary
#' @param x vector
#' @param by  vector
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' t_summary_by(
#'   x = sample(c(TRUE, FALSE), 100, TRUE),
#'   by = factor(sample(paste("day", 1:5), 100, TRUE)),
#'   col_by = factor(sample(paste("ARM", LETTERS[1:3]), 100, TRUE))
#'   col_n = table(rep(paste("ARM", LETTERS[1:3]), 10))
#' )
#' 
#' # need more examples for tables that can be found in STREAM
#' 

t_summary_by <- function(x, by, col_by, col_N, ...) {
  
  # TODO: check the arguments
  
  df <- data.frame(x = x, col_by = col_by)
  
  df_s <- split(df, by)
  
  tbl_head <- rheader(rrowl("", levels(col_by)))
  
  # TODO: get the label from by for overall heading

  tbls <- Map(function(df_i, by_i) {
    tbl <- t_summary(df_i$x, df_i$col_by, ...)
    header(tbl) <- tbl_head
    
    # add the row name for by and indent
    tbl <- fast_rbind(
      rtable(tbl_head,
             rrow(by_i)), 
      indent_table(tbl, 1)
    )
    
  }, df_s, names(df_s))
  
  # use N= from col_N
  tbls <- do.call(stack_rtables, tbls)
  
  tbl_header <- rheader(
    rrowl("", levels(col_by)),
    rrowl("", col_N, format = "(N=xx)" )
  )
  header(tbls) <- tbl_header
  
  tbls
  
}