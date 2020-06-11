#' Summarize an Object for Different Groups with by Variable
#'
#' This is a wrapper around the basic \code{t_summary} functions with the difference that it has a row_by
#' argument.
#'
#' @inheritParams argument_convention
#' @inheritParams t_summary
#' @param x vector or \code{data.frame}
#' @param row_by (\code{factor} or \code{data.frame})\cr
#'    Defines how data from \code{x} is split into sub-tables. Dimensions must match
#'    dimensions of \code{x} and no missing values are allowed. Multi-level nesting is possible when
#'    \code{row_by} is a \code{data.frame}. Columns should be ordered with the first column specifying
#'    the first variable to split by and the last column the specifying the last variable to split by.
#' @param ... arguments passed on to methods
#'
#' @details
#' For every unique combination of levels of \code{row_by} a summary table using
#' \code{\link{t_summary}} will be created. The individual tables are then stacked together.
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
#'  denominator = "N",
#'  drop_levels = TRUE
#' )
#'
#' ADSL$SEX[1:5] <- NA
#'
#' t_summary_by(
#'  x = ADSL$SEX,
#'  row_by = ADSL$COUNTRY,
#'  col_by = ADSL$ARMCD,
#'  total = "All Patients",
#'  denominator = "N",
#'  drop_levels = TRUE,
#'  useNA = "ifany"
#' )
#'
#' ADQS <- radqs(cached = TRUE)
#' ADQS_f <- ADQS %>%
#'   dplyr::filter(PARAMCD=="BFIALL")
#'
#' t_summary_by(
#'  x = ADQS_f$AVAL,
#'  row_by = ADQS_f$AVISIT,
#'  col_by = by_all("All"),
#'  col_N = nrow(ADSL),
#' )
#'
#' ADQS_f$AVALCAT1 <- factor(ifelse(ADQS_f$AVAL >= 50, "Positive", "Negative"),
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
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#'
#' # Recursive case
#' t_summary_by(
#'   x = ADLB$AVAL,
#'   row_by = ADLB[, c("PARAM", "AVISIT")],
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
#' ADQS <- radqs(cached = TRUE) %>%
#'   mutate(PARAM = as.factor(PARAM)) %>%
#'   var_relabel(
#'     PARAM = "Questionnaire Parameter",
#'     AVISIT = "Visit"
#'   )
#'
#' # we suppress warnings because some are NaN (change CHG at screening visit)
#' suppressWarnings(t_summary_by(
#'   x = ADQS[,c("AVAL", "CHG")],
#'   row_by = ADQS[, c("PARAM", "AVISIT")],
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
#'
#' # imitate behavior of t_summarize_by_visit by adding function compare_in_header
#' ADSL <- radsl(cached = TRUE)
#' ADVS <- radvs(cached = TRUE) %>%
#'  dplyr::filter(PARAMCD=="DIABP")
#'
#' t_summary_by(
#'   x = compare_in_header(ADVS[c("AVAL", "CHG")]),
#'   row_by = ADVS$AVISIT,
#'   col_by = ADVS$ARM,
#'   col_N = table(ADSL$ARM),
#'   f_numeric = patient_numeric_fcns()
#' )
t_summary_by <- function(x,
                         row_by,
                         col_by,
                         col_N = NULL, # nolint
                         total = NULL,
                         ...,
                         table_tree = FALSE) {

  if (!is_nested_by(row_by)) {
    check_row_by(row_by, x)
  }

  if (!is.list(row_by)) {
    row_by <- list(row_by)
  }
  stopifnot(is.list(row_by))

  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) # nolint

  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }

  # should only do this at the very end; this way, hierarchical column will also be added for total_N column
  res <- apply_compare_in_header(x, col_by, col_N = col_N)

  tree_data <- rsplit_to_tree(
    list(x = res$x, col_by = res$col_by),
    by_lst = row_by,
    drop_empty_levels = TRUE
  )
  tree <- rapply_tree(
    tree_data,
    function(name, content, path, is_leaf) {
      # only compute for leaf nodes
      content <- if (is_leaf) {
        tbl <- t_summary(
          x = content$x,
          col_by = content$col_by,
          col_N = res$col_N,
          total = total,
          ..., # outer ...
          table_tree = FALSE
        )

        # post-processing to move (N=xx) row
        if ("compare_in_header" %in% names(attributes(x)) && nrow(header(tbl)) == 3){

          tbl <- move_header_N(tbl, col_N)

        }

      tbl

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
#' If \code{col_N} is \code{NULL}, it gets it by adding up the numbers from all items in the \code{col_by_list}
#'
#' @param x_list list of x
#' @param col_by_list list of \code{col_by}, one for each item of \code{x_list}
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

#  Helper functions -----

all_equal <- function(x) {
  length(unique(x)) == 1
}

#' Compare the list items / data.frame columns in the header
#'
#' Each list item will be a separate column in the header. The header will become hierarchical.
#' A data.frame is a special case of a list, where each column is a list item. All specified columns are compared.
#' All items must be of identical class
#'
#' @param x (\code{list}) list that contains items to compare in header
#'
#' @return object with attribute \code{compare_in_header}
#' @export
#'
#' @examples
#' compare_in_header(data.frame(`AVAL` = as.numeric(1:3),
#'   `CHG` = as.numeric(4:6), `CHG2` = as.numeric(7:9)))
compare_in_header <- function(x) {
  # can be list or data.frame
  stopifnot(is.list(x))
  # all columns must be of same type or else, t_summary row names will not agree
  # class[[1]] is used for S3 method dispatch
  # only numeric class type makes sense for now (different factors with different levels are difficult to compare)
  stopifnot(all(
    vapply(x, function(xx) class(xx)[[1]], character(1)) %in% c("integer", "numeric")
  ))
  stopifnot(all_equal(vapply(x, nb_entries, numeric(1))))
  structure(x, compare_in_header = TRUE)
}

#' Returns the number of entries in x
#'
#' For a data.frame, it is \code{nrow}. Defaults to \code{length}.
#'
#' @param x object
#'
#' @export
#'
#' @examples
#' nb_entries(1:3)
#' nb_entries(as.numeric(1:3))
#' nb_entries(data.frame(x = 1:3, y = 4:6))
nb_entries <- function(x) {
  UseMethod("nb_entries", x)
}

# need to export, bug otherwise
#' @export
nb_entries.default <- function(x) {
  length(x)
}

# need to export, bug otherwise
#' @export
nb_entries.data.frame <- function(x) { # nolint
  nrow(x)
}

#' Stacks \code{x} and \code{col_by} when \code{compare_in_header} attribute is present
#'
#' Will create a hierarchical header for each column of x and the columns will be compared
#'
#' @param x data.frame with numeric columns that are stacked
#' @param col_by col_by data.frame
#' @param col_N numeric vector of length \code{ncol(col_by)} or \code{NULL} -> computed from \code{col_by}
#'
#' @return list(
#'   x = stacked x,
#'   col_by = hierarchical col_by with additional hierarchical level for each column of x,
#'     TRUE at entries where corresponding column appears in x
#' )
#' @examples
#' x <- compare_in_header(data.frame(`AVAL` = as.numeric(1:3),
#'   `CHG` = as.numeric(4:6), `CHG2` = as.numeric(7:9)))
#' col_by <- data.frame(`ARM A` = c(TRUE, TRUE, FALSE), `ARM B` = c(TRUE, FALSE, FALSE))
#' res <- tern:::apply_compare_in_header(x, col_by)
#' x <- res$x
#' col_by <- res$col_by
#' col_N <- res$col_N
#' lapply(col_by, function(by) x[by])
#'
#' library(random.cdisc.data)
#' ADVS <- cadvs
#' x <- compare_in_header(ADVS[c("AVAL", "CHG")])
#' col_by <- ADVS$ARM
#' res <- tern:::apply_compare_in_header(x, col_by, col_N = NULL)
#' x <- res$x
#' col_by <- res$col_by
#' col_N <- res$col_N
apply_compare_in_header <- function(x,
                                    col_by,
                                    col_N = NULL) { # nolint
  if (!"compare_in_header" %in% names(attributes(x))) {
    return(list(x = x, col_by = col_by, col_N = col_N))
  }
  attr(x, "compare_in_header") <- NULL

  # only can replicate these row-wise
  stopifnot(is.data.frame(x))
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  # all columns of x will be stacked, therefore we use a hierarchical header
  # to select only one of the columns in the stacked x
  by_x_column <- data.frame(lapply(seq_along(x), function(idx) {
    col_by <- rep(FALSE, nrow(x) * ncol(x))
    col_by[(1 + (idx - 1) * nrow(x)):(idx * nrow(x))] <- TRUE
    col_by
  }))
  # setting colnames instead of names -> whitespace won't be replaced by '.'
  colnames(by_x_column) <- var_labels(x, fill = TRUE)

  return(list(
    x = unlist(c(x), use.names = FALSE), # stacking
    col_by = by_hierarchical(
      do.call(rbind, replicate(ncol(x), col_by, simplify = FALSE)), # stack col_bys
      by_x_column
    ),
    col_N = rep(col_N, each = ncol(x))
  ))
}

# return modified tbl with header N in second row
# only applies to compare_in_header use-cases
move_header_N <- function(tbl, col_N){ # nolint

  l_colspan <- lapply(header(tbl)[[1]], function(cell){
    attributes(cell)$colspan
    })

  l_rcell_n <- Map(function(x, cs){
    rcell(x, colspan = cs, format = "(N=xx)")
  }, col_N, l_colspan) # nolint

  new_header <- rheader(
    header(tbl)[[1]],
    rrowl(NULL, l_rcell_n),
    header(tbl)[[2]]
  )

  header(tbl) <- new_header
  tbl
}
