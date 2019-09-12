#' Summarize an Object for Different Groups with by Variable
#'
#' This is a wrapper around the basic \code{t_summary} functions with the difference that it has a row_by
#' argument. The row_by argument can either be a "by" object or it can be a "r_by" object in which case
#' each list item is interpreted recursively as is typical for the \code{terms} item
#' in \code{t_events_term_grade_id}.
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
#'  col_by = ADSL$ARMCD,
#'  total = "All Patients",
#'  drop_levels = TRUE
#' )
#'
#'
#' ADSL$SEX[1:5] <- NA
#'
#' t_summary_by(
#'  x = ADSL$SEX,
#'  row_by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD,
#'  total = "All Patients",
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
#'  col_by = ADQS_f$ARMCD,
#'  total = "All Patients",
#'  col_N = table(ADSL$ARMCD),
#' )
#'
#' ADQS_f$AVALCAT1 <- factor(ifelse(ADQS_f$AVAL >= 0, "Positive", "Negative"),
#'   levels = c("Positive", "Negative"))
#' ADQS_f <- var_relabel(ADQS_f, AVALCAT1 = "Result" )
#'
#' t_summary_by(
#'  x = ADQS_f$AVALCAT1,
#'  row_by = ADQS_f$AVISIT,
#'  col_by = ADQS_f$ARMCD,
#'  total = "All Patients",
#'  col_N = table(ADSL$ARMCD),
#' )
#'
#' ADSL <- radsl(cached = TRUE) #todo: replace by seed version
#' ADLB <- radlb(cached = TRUE) #todo: replace by seed version
#'
#' # Recursive case
#' t_summary_by(
#'   x = ADLB$AVAL,
#'   row_by = r_by(ADLB[, c("PARAM", "AVISIT")]),
#'   col_by = ADLB$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients"
#' )
#'
#'
#' # Table Tree
#' ADSL <- radsl(cached = TRUE)
#'
#' tbls <- t_summary_by(
#'  x = ADSL$SEX,
#'  row_by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD,
#'  total = "All Patients",
#'  table_tree = TRUE
#' )
#' summary(tbls)
#' to_rtable(tbls)
#'
#' # Other examples
#' ADQS <- radqs(ADSL, seed = 2) %>%
#'   mutate(PARAM = as.factor(PARAM)) %>%
#'   var_relabel(
#'     PARAM = "Questionnaire Parameter",
#'     AVISIT = "Visit"
#'   )
#'
#' # we suppress warnings because some are NaN (change CHG at screening visit)
#' suppressWarnings(t_summary_by(
#'   x = ADQS[,c("AVAL", "CHG")],
#'   row_by = r_by(ADQS[, c("PARAM", "AVISIT")]),
#'   col_by = ADQS$ARMCD,
#'   col_N = table(ADSL$ARMCD),
#'   total = "All Patients"
#' ))
#'
#' ADQS_f <- ADQS %>%
#'   dplyr::filter(PARAMCD == "BFIALL") %>%
#'   dplyr::mutate(
#'     AVALCAT1 = factor(
#'       ifelse(AVAL >= 50, "Positive", "Negative"),
#'       levels = c("Positive", "Negative")
#'     )
#'   ) %>%
#'   var_relabel(
#'     AVALCAT1 = "Result",
#'     AVISIT = "Visit"
#'   )
#'
#' t_summary_by(
#'   x = ADQS_f %>% select(c("AVALCAT1", "AVAL")),
#'   row_by = ADQS_f$AVISIT,
#'   col_by = ADQS_f$ARMCD,
#'   col_N = table(ADSL$ARMCD),
#'   total = "All Patients"
#' )
t_summary_by <- function(x,
                         row_by,
                         col_by,
                         col_N = NULL, # nolint
                         total = NULL,
                         ...,
                         table_tree = FALSE) {
  if (is.atomic(row_by)) {
    row_by <- list(simple_by(row_by))
  }
  stopifnot(is.list(row_by))

  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by)
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  tree_data <- rsplit_to_tree(
    list(x = x, col_by = col_by),
    by_lst = row_by,
    drop_empty_levels = TRUE
  )
  tree <- rapply_tree(
    tree_data,
    function(name, content, path, is_leaf, ...) {
      # only compute for leaf nodes
      content <- if (is_leaf) {
        t_summary(
          x = content$x,
          col_by = content$col_by,
          col_N = col_N,
          total = total,
          ...,
          table_tree = TRUE
        )
      } else {
        NULL
      }
      list(
        name = name,
        content = content
      )
    }
  )
  tree@name <- invisible_node_name(tree@name)

  # todo: paste(deparse(substitute(row_by)) can be very long output and is confusing
  # row_by is of class r_by
  # row_by_lbl <- paste(lapply(row_by, label), collapse = "/") %||%
  #   paste(deparse(substitute(row_by)), sep = "\n")
  # x_lbl <- label(x) %||% paste(deparse(substitute(x)), sep = "\n") # secondary index
  # rownames_header <- c(rrow(NULL, row_by_lbl), rrow(NULL, x_lbl, indent = 1))
  # tree@format_data[["left_header"]] <- rownames_header

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

#' Helper function for t_summary_by
#'
#' You can use \code{...} to add arguments like \code{total}
#'
#' If col_N is NULL, it gets it by adding up the numbers from all items in the col_by_list
#'
#' @param x_list list of x
#' @param col_by_list list of col_by, one for each item of \code{x_list}
#' @inheritParams t_summary.data.frame
t_summary.list <- function(x_list, #nolintr
                           col_by_list,
                           col_N = NULL, # nolint
                           total = NULL,
                           ...,
                           table_tree = FALSE) {
  stopifnot(
    is.list(x_list),
    is.list(col_by_list),
    length(x_list) == length(col_by_list)
  )
  col_N <- col_N %||% rowSums(data.frame(lapply(col_by_list, function(col_by) get_N(col_by)))) #nolintr
  col_by_list <- Map(function(col_by, x) col_by_to_matrix(col_by, x), col_by_list, x_list)
  if (!is.null(total)) {
    col_by_list <- lapply(col_by_list, function(col_by) by_add_total(col_by, label = total))
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

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
t_summary_true <- function(x,
                           col_by,
                           col_N = NULL, #nolintr
                           total = NULL,
                           row_name = deparse(substitute(x)),
                           denominator = c("N", "n", "omit")) {
  denominator <- match.arg(denominator)

  stopifnot(is.logical(x))

  t_summary.logical(x, col_by, col_N, total, row_name_true = row_name, denominator = denominator)[2, ]

}
