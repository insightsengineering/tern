#' Count unique elements per cell in a vector
#'
#' @inheritParams argument_convention
#' @param x a vector
#' @param denominator either N or omit. With option N, the reference population
#' from \code{col_N} is used as the denominator. If \code{omit} is chosen,
#' the percentage is omitted and only the counts are returned.
#' @param row_name a string with the row name to display in the summary table
#'   that is returned. Default is "number of unique elements."
#'
#' @return an \code{rtable}
#'
#' @export
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_count_unique}},
#'  \code{\link{t_events_per_term_id}}, \code{\link{t_el_events_per_term_id}},
#'  \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#'
#' t_el_count_unique(
#'  x = paste("id", c(1, 4, 2, 3, 3), sep = "-"),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#'
#' t_el_count_unique(
#'  x = paste("id", c(1, 4, 2, 3, 3), sep = "-"),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10),
#'  denominator = "omit"
#' )
#'
#' # how missing values are handled
#' t_el_count_unique(
#'  x = c(1, 4, 2, 3, 3, NA),
#'  col_by = factor(c("A", "A", "B", "C", "C", "C")),
#'  col_N = c(2, 4, 10),
#'  na_rm = TRUE
#' )
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' t_el_count_unique(ADAE$USUBJID, col_by = ADAE$ARM, col_N = table(ADSL$ARM),
#'   row_name = "Total number of patients with at least one AE")
#'
t_el_count_unique <- function(x,
                              col_by,
                              col_N = NULL, # nolint
                              total = NULL,
                              na_rm = TRUE,
                              denominator = c("N", "omit"),
                              row_name = "number of unique elements") {
  denominator <- match.arg(denominator)

  stopifnot(
    is.null(total) || is_character_single(total),
    is.atomic(x),
    is_logical_single(na_rm),
    is_character_single(row_name)
  )
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  omit_perc <- denominator == "omit"

  counts <- vapply(col_by, function(rows) {
    xi <- x[rows]
    if (na_rm) {
      xi <- na.omit(xi)
    }
    length(unique(xi))
  }, numeric(1))

  tbl_row <- if (omit_perc) {
    rrowl(as.character(row_name), counts, format = "xx")
  } else{
    rrowl(as.character(row_name), Map(function(ci, pi) {
      res <- if (ci == 0) {
        rcell(ci, format = "xx")
      } else {
        rcell(c(ci, pi), format = "xx.xx (xx.xx%)")
      }
    }, counts, counts / col_N))
  }

  tbl <-  rtable(
    rheader(rrowl("", colnames(col_by))),
    tbl_row
  )
  header_add_N(tbl, col_N)
}


#' Count unique elements per cell in a vector with subgroups
#'
#' This is a wrapper function for \code{t_el_count_unique} useful for tabulating unique values
#' in subgroups.
#'
#' @inheritParams t_el_count_unique
#' @inheritParams argument_convention
#' @param subgroups A logical data.frame with rows matching the length of \code{x}. Each
#'   column represents a subset to be applied to \code{x} and \code{col_by} prior to
#'   tabulating the unique elements per cell. Label attributes are used for the row names
#'   if they exist. NAs are cast to \code{FALSE}.
#'
#' @return an \code{rtable}
#'
#' @export
#'
#' @seealso \code{\link{t_el_count_unique}}, \code{\link{t_el_events_per_term_id}}
#'
#' @examples
#'
#' # define analysis subgroups
#' sbs <- data.frame("grp1" = c(TRUE, TRUE, FALSE, FALSE),
#'                  "grp2" = c(FALSE, TRUE, TRUE, FALSE),
#'                  "grp3" = c(FALSE, FALSE, FALSE, FALSE),
#'                  "grp4" = c(NA, FALSE, NA, TRUE))
#'
#' t_count_unique(x = c(1:4), col_by = factor(c("A", "A", "B", "B")), subgroups = sbs,
#'   col_N = c(4, 10))
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' # define analysis subgroups
#' ANL <- ADAE %>%
#'  dplyr::transmute(
#'     SER = AESER == 'Y' ,
#'     REL = AEREL == 'Y',
#'     GR5 =  AETOXGR == '5'
#'   ) %>%
#'   var_relabel(
#'     SER = 'Serious AE',
#'     REL = 'Relasted AE',
#'     GR5 = 'Grade 5 AE'
#'   )
#'
#' # number of unqiue patients with events:
#' t_count_unique(ADAE$USUBJID, col_by = ADAE$ARM, subgroups = ANL,
#'   col_N = table(ADSL$ARM), total = "All")
#'
#' # omit percent
#' tbl <- t_count_unique(ADAE$USUBJID, col_by = ADAE$ARM, subgroups = ANL,
#'   col_N = table(ADSL$ARM), denominator = "omit")
#' tbl
#'
#' # add descriptive row
#' insert_rrow(indent(tbl, 1), rrow("Total number of patients with at least one"))
#'
t_count_unique <- function(x,
                           col_by,
                           subgroups,
                           col_N = NULL, # nolint
                           total = NULL,
                           na_rm = TRUE,
                           denominator = c("N", "omit"),
                           table_tree = FALSE) {

  denominator <- match.arg(denominator)

  stopifnot(
    is.null(total) || is_character_single(total),
    is.atomic(x),
    all(vapply(subgroups, is.logical, FUN.VALUE = logical(1))),
    is_logical_single(na_rm)
  )
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by) #nolintr
  if (!is.null(total)) {
    col_by <- by_add_total(col_by, label = total)
    col_N <- col_N_add_total(col_N) #nolintr
    total <- NULL
  }
  check_col_by(x, col_by, col_N, min_num_levels = 1)
  check_same_n(x = x, subgroups = subgroups)

  x_list <- lapply(subgroups, subset, x = x, drop = FALSE)
  col_by_list <- lapply(subgroups, subset, x = col_by, drop = FALSE)

  # one child per column of x
  children <- Map(function(xi, cbi, node_name) {

    node(
      name = invisible_node_name(node_name),
      content = if (length(xi) == 0 && nrow(cbi) == 0) {
        header_add_N(
          rtable(
            header = rheader(rrowl("", colnames(cbi))),
            rrowl(node_name, get_N(cbi))
          ),
          col_N
        )

      } else {

        t_el_count_unique(x = xi,
                          col_by = cbi,
                          col_N = col_N,
                          total = NULL,
                          na_rm = na_rm,
                          denominator = denominator,
                          row_name = node_name)

        }
      ,
      children = list(),
      format_data = list(content_indent = 0)
    )
  }, x_list, col_by_list, var_labels(subgroups, fill = TRUE))

  tree <- invisible_node(children = children, format_data = list(children_gap = 0))

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }

}
