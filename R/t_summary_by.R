#' Summarize an Object for Different Groups with by Variable
#' 
#' 
#' description
#' 
#' @inheritParams t_summary
#' @param x vector
#' @param by  a \code{factor} of length \code{nrow(x)} with no missing values. The levels of \code{by} define the summary sub-groups in the table.
#' @param col_by a \code{factor} of length \code{nrow(x)} with no missing values. The levels of \code{col_by} define the columns in the table.
#' @param col_N a \code{table} object with the reference population used for the header of the table. See examples below.
#' @param total character string that will be used as a label for a column with pooled total population. If the levels of \code{col_by} are the only columns of interest then total should be \code{NULL}.
#' @param ... arguments passed on to methods
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' t_summary_by(
#'   x = sample(c(TRUE, FALSE), 100, TRUE),
#'   by = factor(sample(paste("day", 1:5), 100, TRUE)),
#'   col_by = factor(sample(paste("ARM", LETTERS[1:3]), 100, TRUE)),
#'   col_N = table(rep(paste("ARM", LETTERS[1:3]), 10)),
#'   total = "All Patients"
#' )
#' 
#' t_summary_by(
#'   x = sample(1:20, 100, TRUE),
#'   by = factor(sample(paste("day", 1:5), 100, TRUE)),
#'   col_by = factor(sample(paste("ARM", LETTERS[1:3]), 100, TRUE)),
#'   col_N = table(rep(paste("ARM", LETTERS[1:3]), 10)),
#'   total = "All Patients"
#' )
#' 
#' ANL <- data.frame(
#'  aval = sample(c(TRUE, FALSE), 100, TRUE),
#'  avisit = factor(sample(c(
#'    "Screening", "Day 5", "Day 10"
#'  ), 100, TRUE), levels = c("Screening", "Day 5", "Day 10")),
#'  col_by = factor(sample(paste("ARM", LETTERS[1:3]), 100, TRUE))
#' )
#' ANL <- var_relabel(ANL, aval = "Analysis Value", avisit = "Analysis Visit" )
#' 
#' t_summary_by(
#'   x = ANL$aval,
#'   by = ANL$avisit,
#'   col_by = ANL$col_by,
#'   col_N = table(rep(paste("ARM", LETTERS[1:3]), 10))
#' )
#' 
#' # TODO: add more examples for tables that can be found in STREAM
#' 

t_summary_by <- function(x, by, col_by, col_N, total = NULL, ...) {
  
  # check the arguments
  if (!is.factor(by)) stop("by is required to be factor")
  if (any(is.na(by))) stop("no NA allowed in by")
  
  by_lbl <- label(by)
  x_lbl <- label(x)
  if(is.null(by_lbl)) by_lbl <- ""
  if(is.null(x_lbl)) x_lbl <- ""
  
  
  if ( length(x)==length(by) & length(x)==length(col_by)){
    df <- data.frame(x = x, by = by, col_by = col_by)    
  } else{
    stop(paste0("Lengths of vectors differ. The length of x is ", length(x), ", length of by is ", length(by), ", length of col_by is ", length(col_by), "."))
  }

  # If total column is requested stack the data and change by, col_by and col_N accordingly
   if (!is.null(total) && !is.no_by(col_by)) { ## add total column

     if (length(total) != 1) stop("total must be either NULL or a single string")
     if (total %in% col_by) stop("total cannot be an level in col_by")

     df <- duplicate_with_var(df, col_by = total)
     df$col_by <- factor(df$col_by, levels = c(levels(col_by), total))
     col_by <- df$col_by
     by <- df$by
     col_N <- c(col_N, sum(col_N)) 
   }
  
  df_s <- split(df, by)

  tbl_head <- rheader(rrowl("", levels(col_by)))
  
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

  # append labels from X and BY to overall heading
  tbl_header <- rheader(
    rrowl(by_lbl, levels(col_by)),
    rrowl(x_lbl, col_N, format = "(N=xx)", indent=1 )
  )
  header(tbls) <- tbl_header

  tbls
  
}