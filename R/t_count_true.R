#' Tabulate \code{TRUE} counts
#'
#' Note that for this function the default \code{denominator = "N"} and
#' not \code{n} as for the other \code{t_summary} functions.
#'
#' @inheritParams t_count_true.logical
#' @param x an object to dispatch on
#' @param ... arguments passed on to methods
#'
#' @export
#'
#' @seealso \code{\link{t_count_true.logical}},
#'    \code{\link{t_count_true.data.frame}},
#'    \code{\link{t_summary.logical}}, \code{\link{t_summary.factor}}
#'
#' @examples
#'
#' t_count_true(c(TRUE, TRUE, FALSE, FALSE), col_by = factor(c("A", "B", "B", "B")),
#'   row_name = "TRUE")
#'
#' library(dplyr)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' t_count_true(x = !is.na(ADSL$DCSREAS), col_by = ADSL$ARM, col_N = table(ADSL$ARM),
#'   row_name = "Patients who discontinued study" )
#'
#' # example with missing values
#' AGE_NA <- ADSL$AGE
#' AGE_NA[1:50] <- NA
#' t_count_true(AGE_NA > 35 , col_by = ADSL$ARM, col_N = table(ADSL$ARM),
#'   row_name = "AGE > 35")
#' t_count_true(AGE_NA > 35 , col_by = ADSL$ARM, col_N = table(ADSL$ARM),
#'   row_name = "AGE > 35", denominator = "n")
#'
#' ADAE <- radae(cached = TRUE)
#' ANL <- ADAE %>%
#'   dplyr::transmute(
#'     SER = AESER == 'Y' ,
#'     GRADE35 =  AETOXGR %in% c('3', '4', '5')
#'   ) %>%
#'   var_relabel(
#'     SER = "Serious",
#'     GRADE35 = "Grade 3-5"
#'   )
#' tbl <- t_count_true(ANL, col_by = ADAE$ARMCD, col_N = table(ADSL$ARMCD),
#'   denominator = "omit")
#' tbl
#'
#' # Add descriptive row:
#' insert_rrow(indent(tbl, 1), rrow("Total number of adverse events which are"))
t_count_true <- function(x,
                         col_by,
                         col_N = NULL, # nolint
                         total = NULL,
                         denominator = c("N", "n", "omit"),
                         ...) {
  UseMethod("t_count_true", x)
}


#' Return an \code{rtable} with a no Method Message
#'
#' If there is no explicit method for an object an \code{rtable} with one row with an
#' appropriate message is returned.
#'
#' @inheritParams t_count_true.logical
#' @param ... not used arguments
#'
#' @return \code{rtable}
#'
#' @export
#'
#' @examples
#' t_count_true(1:5, factor(LETTERS[c(1,2,1,1,2)]))
t_count_true.default <- function(x, # nolint
                                 col_by,
                                 col_N = NULL, # nolint
                                 total = NULL,
                                 denominator = c("N", "n", "omit"),
                                 ...) {

  tbl <- t_summary.default(x = x, col_by = col_by, col_N = col_N, total = total)
  row.names(tbl) <- gsub("t_summary", "t_count_true", row.names(tbl))
  tbl
}


#' Tabulate \code{TRUE} counts in a vector
#'
#' This function is a wrapper for \code{\link{t_summary.logical}} returning only
#' the counts for \code{TRUE} records. Note that for this function the default
#' \code{denominator = "N"} and not \code{n} as for the other \code{t_summary}
#' functions.
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary.factor
#' @param x a logical vector
#' @param row_name name of row
#' @param ... not used arguments
#'
#' @export
#'
#' @seealso \code{\link{t_count_true}},
#' \code{\link{t_count_true.data.frame}},
#' \code{\link{t_summary.logical}}, \code{\link{t_summary.factor}}
#'
#' @examples
#' with(iris, t_count_true(Sepal.Length > 5.5, col_by = Species, total = "All Flowers"))
#'
#' # compare with t_summary.logical
#' with(iris, t_summary(Sepal.Length > 5.5, col_by = Species, total = "All Flowers"))
#'
#' # show only counts without percent
#' with(iris, t_count_true(Sepal.Length > 5.5, col_by = Species, denominator = "omit"))
t_count_true.logical <- function(x,
                                 col_by,
                                 col_N = NULL, # nolint
                                 total = NULL,
                                 denominator = c("N", "n", "omit"),
                                 row_name = deparse(substitute(x)),
                                 ...) {

  stopifnot(is_character_single(row_name))
  denominator <- match.arg(denominator)

  t_summary.logical(x, col_by, col_N, total, denominator = denominator,
                    row_name_true = row_name)[2, ]
}

#' Tabulate \code{TRUE} counts in a list
#'
#' Helper function for \code{t_count_true.data.frame},
#' If \code{col_N} is \code{NULL}, it's derived by adding up the numbers from all items in the \code{col_by_list}
#'
#' @inheritParams t_count_true.data.frame
#' @param x_list list of x
#' @param col_by_list list of \code{col_by}, one for each item of \code{x_list}
#' @param ... not used arguments
#'
t_count_true.list <- function(x_list, #nolintr
                              col_by_list,
                              col_N = NULL, # nolint
                              total = NULL,
                              denominator = c("N", "n", "omit"),
                              table_tree = FALSE,
                              ...) {
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
      name = invisible_node_name(node_name),
      content = t_count_true(
        x = x,
        col_by = col_by,
        col_N = col_N,
        total = NULL,
        row_name = node_name,
        denominator = denominator),
      children = list(),
      format_data = list(content_indent = 0)
    )
  }, x_list, col_by_list, names(x_list))

  tree <- invisible_node(children = children, format_data = list(children_gap = 0))

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}



#' Tabulate \code{TRUE} counts in a data.frame
#'
#' Note that for this function the default \code{denominator = "N"}
#' and not \code{n} as for the other \code{t_summary} functions.
#'
#' @inheritParams t_count_true.logical
#' @inheritParams argument_convention
#' @param x data frame with only logical variables to be summarized.
#' If a variable has a \code{label} attribute then it will be used for the row name.
#' @inheritParams t_count_true.data.frame
#'
#' @export
#'
#' @seealso \code{\link{t_count_true}},
#' \code{\link{t_count_true.logical}},
#' \code{\link{t_summary.logical}}, \code{\link{t_summary.factor}},
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ANL <- ADSL %>%
#'   dplyr::transmute(
#'     SAFPOP = SAFFL == 'Y' ,
#'     DISCSTUD =  !is.na(DCSREAS)
#'   )
#'
#' t_count_true(ANL, col_by = ADSL$ARM)
#'
#' # Add variable labels
#' ANL <- ANL %>%
#'   var_relabel(
#'     SAFPOP = "Safety Population",
#'     DISCSTUD = "Discontinued study"
#'   )
#'
#' t_count_true(ANL, col_by = ADSL$ARM)
#' t_count_true(ANL, col_by = ADSL$ARM, denominator = "omit")
t_count_true.data.frame <- function(x, # nolint
                                    col_by,
                                    col_N = NULL, # nolint
                                    total = NULL,
                                    denominator = c("N", "n", "omit"),
                                    table_tree = FALSE,
                                    ...) {

  denominator <- match.arg(denominator)

  # NAs are accepted in X
  stopifnot(all(vapply(x, is.logical, FUN.VALUE = logical(1))))
  stopifnot(is.null(total) || is_character_single(total))
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  # each column of the data frame x is an element of the list
  # replicate col_by ncol times, rep does not work
  colnames(x) <- var_labels(x, fill = TRUE)
  t_count_true.list(
    x_list = x,
    col_by_list = replicate(length(x), col_by, simplify = FALSE),
    col_N = col_N,
    total = total,
    denominator = denominator,
    table_tree = table_tree
  )

}
