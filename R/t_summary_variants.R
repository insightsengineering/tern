#' Summarize an Object for Different Groups with by Variable
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary
#' @param x vector
#' @param row_by a \code{factor} of length \code{nrow(x)} with no missing values. The levels of \code{by} define
#'  the summary sub-groups in the table.
#' @param col_by a \code{factor} of length \code{nrow(x)} with no missing values. The levels of \code{col_by}
#'  define the columns in the table.
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
#' ADSL <- radsl(cached = TRUE)
#'
#' t_summary_by(
#'  x = ADSL$SEX,
#'  row_by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD %>% by_add_total("All Patients"),
#'  drop_levels = TRUE
#' )
#'
#'
#' ADSL$SEX[1:5] <- NA
#'
#' t_summary_by(
#'  x = ADSL$SEX,
#'  row_by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD %>% by_add_total("All Patients"),
#'  drop_levels = TRUE,
#'  useNA = "ifany"
#' )
#'
#' ADSL <- ADSL %>% select(STUDYID, USUBJID, ARMCD)
#'
#' ADQS <- radqs(cached = TRUE)
#' ADQS_f <- ADQS %>%
#'   dplyr::filter(PARAMCD=="BFIALL")
#'
#' t_summary_by(
#'  x = ADQS_f$AVAL,
#'  row_by = ADQS_f$AVISIT,
#'  col_by = ADQS_f$ARMCD %>% by_add_total("All Patients"),
#'  col_N = col_N_add_total(table(ADSL$ARMCD)),
#' )
#'
#' ADQS_f$AVALCAT1 <- factor(ifelse(ADQS_f$AVAL >= 0, "Positive", "Negative"),
#'   levels = c("Positive", "Negative"))
#' ADQS_f <- var_relabel(ADQS_f, AVALCAT1 = "Result" )
#'
#' t_summary_by(
#'  x = ADQS_f$AVALCAT1,
#'  row_by = ADQS_f$AVISIT,
#'  col_by = ADQS_f$ARMCD %>% by_add_total("All Patients"),
#'  col_N = col_N_add_total(table(ADSL$ARMCD)),
#' )
#'
#'
#' # Table Tree
#' ADSL <- radsl(cached = TRUE)
#'
#' tbls <- t_summary_by(
#'  x = ADSL$SEX,
#'  row_by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD %>% by_add_total("All Patients"),
#'  table_tree = TRUE
#' )
#' summary(tbls)
#' to_rtable(tbls)
#'
t_summary_by <- function(x,
                         row_by,
                         col_by,
                         col_N = NULL, # nolint
                         total = NULL,
                         ...,
                         table_tree = FALSE) {
  col_by <- col_by_to_matrix(col_by, x)
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N)
    total <- NULL
  }
  # can rely on col_N computed in t_summary.list because it has different col_by, but it sums it
  row_by <- col_by_to_matrix(row_by, x)
  t_summary.list(
    x_list = lapply(row_by, function(rows) subset(x, rows)),
    col_by_list = lapply(row_by, function(rows) subset(col_by, rows)),
    col_N = col_N,
    total = total,
    ...,
    table_tree = table_tree
  )
}

t_summary.list <- function(x_list, # x_list
                           col_by_list,
                           col_N = NULL, # nolint
                           ...,
                           table_tree = FALSE) {
  if (is.null(col_N)) {
    col_N <- rowSums(data.frame(lapply(col_by_list, function(col_by) get_N(col_by))))
  }
  col_by_list <- lapply(col_by_list, function(col_by) col_by_to_matrix(col_by, x_list))

  # one child per column of x
  children <- Map(function(x, col_by, node_name) {
    check_col_by(x, col_by, col_N, min_num_levels = 1)
    node(
      name = node_name,
      content = t_summary(x = x, col_by = col_by, col_N = col_N, ...),
      children = list()
    )
  }, x_list, col_by_list, names(x_list))
  tree <- invisible_node(children = children)

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}



#' Summarize the TRUE Counts
#'
#' Note that for this function the default \code{denominator = "N"} and not \code{n} as for the other \code{t_summary}
#' functions.
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary.factor
#' @param row_name name of row
#'
#' @export
#'
#' @examples
#'
#' with(iris, t_summary_true(Sepal.Length > 5.5, col_by = Species, total = "All Flowers"))
t_summary_true <- function(x, col_by, col_N = get_N(col_by), total = NULL, row_name = deparse(substitute(x)),
                           denominator = c("N", "n")) {
  denominator <- match.arg(denominator)

  stopifnot(is.logical(x))

  t_summary.logical(x, col_by, col_N, total, row_name_true = row_name, denominator = denominator)[2, ]

}
