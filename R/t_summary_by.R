#' Summarize an Object for Different Groups with by Variable
#'
#' @inheritParams t_summary
#' @param x vector
#' @param by  a \code{factor} of length \code{nrow(x)} with no missing values. The levels of \code{by} define the summary sub-groups in the table.
#' @param col_by a \code{factor} of length \code{nrow(x)} with no missing values. The levels of \code{col_by} define the columns in the table.
#' @param col_N a \code{table} object with the reference population used for the header of the table. See examples below.
#' @param total character string that will be used as a label for a column with pooled total population. If the levels of \code{col_by} are the only columns of interest then total should be \code{NULL}.
#' @param ... arguments passed on to methods
#'
#' @details
#' For every level of the variable \code{by} a summary table using \code{\link{t_summary}} will be created.
#' The individual tables are then stacked together.
#'
#' @export
#'
#' @template author_stoilovs
#'
#' @seealso \code{\link{t_summary}}, \code{\link{t_summary.data.frame}},
#'   \code{\link{t_summary.numeric}}, \code{\link{t_summary.factor}},
#'   \code{\link{t_summary.logical}}, \code{\link{t_summary.Date}},
#'   \code{\link{t_summary.character}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(N = 30, seed = 1)
#'
#' t_summary_by(
#'  x = ADSL$SEX,
#'  by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD,
#'  col_N = table(ADSL$ARMCD),
#'  total = "All Patients",
#'  drop_levels = TRUE
#' )
#'
#' ADSL$SEX[1:5] <- NA
#'
#' t_summary_by(
#'  x = ADSL$SEX,
#'  by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD,
#'  col_N = table(ADSL$ARMCD),
#'  total = "All Patients",
#'  drop_levels = TRUE,
#'  useNA = "ifany"
#' )
#'
#' ADSL <- ADSL %>% select(STUDYID, USUBJID, ARMCD)
#'
#' ADQS <- radqs(ADSL, seed = 2)
#' ADQS_f <- ADQS %>%
#'   dplyr::filter(PARAMCD=="BFIALL")
#'
#' t_summary_by(
#'  x = ADQS_f$AVAL,
#'  by = ADQS_f$AVISIT,
#'  col_by = ADQS_f$ARMCD,
#'  col_N = table(ADSL$ARMCD),
#'  total = "All Patients"
#' )
#'
#' ADQS_f$AVALCAT1 <- factor(ifelse(ADQS_f$AVAL >= 0, "Positive", "Negative"),
#'   levels = c("Positive", "Negative"))
#' ADQS_f <- var_relabel(ADQS_f, AVALCAT1 = "Result" )
#'
#' t_summary_by(
#'  x = ADQS_f$AVALCAT1,
#'  by = ADQS_f$AVISIT,
#'  col_by = ADQS_f$ARMCD,
#'  col_N = table(ADSL$ARMCD),
#'  total = "All Patients"
#' )
t_summary_by <- function(x,
                         by,
                         col_by,
                         col_N, # nolint
                         total = NULL,
                         ...) {
  stopifnot(
    is.factor(by),
    !any(is.na(by))
  )
  check_col_by(col_by, col_N, min_num_levels = 1)

  by_lbl <- label(by)
  x_lbl <- label(x)
  if (is.null(by_lbl)) {
    by_lbl <- paste(deparse(substitute(by)), sep = "\n")
  }
  if (is.null(x_lbl)) {
    x_lbl <- paste(deparse(substitute(x)), sep = "\n")
  }

  if (length(x) == length(by) && length(x) == length(col_by)) {
    df <- data.frame(x = x, by = by, col_by = col_by, stringsAsFactors = FALSE)
  } else {
    stop(paste0("Lengths of vectors differ. The length of x is ", length(x),
                ", length of by is ", length(by), ", length of col_by is ", length(col_by), "."))
  }

  # If total column is requested stack the data and change by, col_by and col_N accordingly
   if (!is.null(total) && !is.no_by(col_by)) {

     if (length(total) != 1) {
       stop("total must be either NULL or a single string")
     }
     if (total %in% col_by) {
       stop("total cannot be an level in col_by")
     }

     # duplicate x, col_by and col_N
     tmp1 <- add_total(x = x, col_by = col_by, total_level = total, col_N = col_N)
     # duplicate by variable
     tmp2 <- add_total(x = by, col_by = col_by, total_level = total, col_N = col_N)

     x <- tmp1$x
     col_by <- tmp1$col_by
     col_N <- tmp1$col_N # nolint
     by <- tmp2$x

     df <- data.frame(x = x, by = by, col_by = col_by, stringsAsFactors = FALSE)
   }

  df_s <- split(df, by, drop = FALSE)

  tbl_head <- rheader(rrowl("", levels(col_by)))

  tbls <- Map(function(df_i, by_i) {
    tbl <- t_summary(df_i$x, df_i$col_by, col_N = col_N, ...)
    header(tbl) <- tbl_head

    # add the row name for by and indent
    tbl <- rbind(
      rtable(tbl_head, rrow(by_i)),
      indent_table(tbl, 1)
    )

  }, df_s, names(df_s))

  # use N= from col_N
  tbls <- rbindl_rtables(tbls, gap = 1)

  # append labels from X and BY to overall heading
  tbl_header <- rheader(
    rrowl(by_lbl, levels(col_by)),
    rrowl(x_lbl, col_N, format = "(N=xx)", indent = 1)
  )
  header(tbls) <- tbl_header

  tbls
}
