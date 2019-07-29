#' Basic Events Table
#'
#' This function summarizes number of unique subjects with events and total number of events.
#' It creates basic summary of events and can be used for any events data like Adverse Events,
#' concomitant medication, medical history, etc.
#' Implementation examples are to apply \code{t_events_per_term_id} on Adverse Event data
#' to create Adverse Events summary table
#' (\code{AET02},
#' \href{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet02.html#example-report-outputs-aet02-aet02}{
#' STREAM2.x},
#' \href{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027342}{STREAM1.17} ),
#' or apply on Concomitant Medication data to create Concomitant Treatment summary table
#' (\code{CMT01},
#' \href{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_cmt01.html#example-report-outputs-cmt01-cmt01}{
#' STREAM2.x},
#' \href{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027342}{STREAM1.17}).
#'
#' @inheritParams argument_convention
#' @param terms character or factor vector, or dataframe to represent events information;
#'   Currently \code{terms} can only be a vector or dataframe with 1 or 2 columns.
#'   For \code{terms} with 2 columns, 1st column should represent higher level term and 2nd
#'   column should be lower level term.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term.
#' @param id vector of subject identifier. Length of \code{id} must be the same as the
#'   length or number of rows of \code{terms}.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param event_type type of event that is summarized (e.g. adverse event,
#'   treatment). Default is "event".
#'
#' @details
#' \code{t_events_per_term_id} includes percentages based on the total number of subjects
#' in the column heading (i.e. \code{"N=nnn"}). \code{col_N} can be explicitly specified to
#' get N for percentage calculation from either events dataset or additional dataset such as
#' subject level dataset. See the example.
#'
#' Multiple events within a subject of the same term (if \code{terms} is one level) or lower level term
#' (if \code{terms} is two levels) are counted once when counting number of subjects.
#'
#' \code{t_events_per_term_id} doesn't deal with data with any non-complete records (has \code{NA}),
#' e.g. if any terms are missing. Impute missing values before using \code{t_events_per_term_id}.
#'
#' \code{t_events_per_term_id} orders data by "All Patients" column from the most commonly
#'  reported higher level term to the least frequent one. Within each group of higher level term,
#'  it sorts by decreasing frequency of lower level term. It brakes ties using \code{terms} names in
#'  alphabetical order.
#'
#' \if{html}{
#'
#' The data is split and table functions are applied to leaf nodes as follows:
#'
#' \figure{lt_events_per_term_id_2.png}{options: alt="lt_events_per_term_id_2 layout"}
#' }
#'
#'
#' @return an \code{\link{rtable}} object.
#'
#' @export
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_count_unique}}, \code{\link{t_el_events_per_term_id}},
#'   \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#' t_events_per_term_id(
#'  terms = with_label(factor(c("t1", "t1", "t2", "t2", "t2")), "Term"),
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")) %>% by_add_total("All Patients"),
#'  col_N = col_N_add_total(c(2, 4, 10))
#' )
#'
#' t_events_per_term_id(
#'  terms = with_label(factor(c("t1", "t1", "t2", "t2", "t2")), "Term"),
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#'
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4, seed = 2)
#'
#' t_events_per_term_id(
#'   terms = as_factor_keep_attributes(ADAE$AEDECOD),
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   event_type = "adverse event"
#' )
#'
#' t_events_per_term_id(
#'   terms = as_factor_keep_attributes(ADAE$AEDECOD),
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#'   event_type = "adverse event"
#' )
#'
#' t_events_per_term_id(
#'   terms = ADAE[, c("AEBODSYS", "AEDECOD")] %>% map(as_factor_keep_attributes),
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#' )
#'
#' ADSL <- radsl(10, seed = 1)
#' ADCM <- radcm(ADSL, 5, seed = 4)
#' ADCM <- ADCM %>%
#'   dplyr::filter(ATIREL == "CONCOMITANT")
#'
#' t_events_per_term_id(
#'   terms = ADCM[, c("CMCLAS", "CMDECOD")] %>% map(as_factor_keep_attributes),
#'   id = ADCM$USUBJID,
#'   col_by = ADCM$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#'   event_type = "treatment"
#' )
#'
#'
#' # Table Tree
#'
#' summary(t_events_per_term_id(
#'   terms = as_factor_keep_attributes(ADAE$AEDECOD),
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#'   table_tree = TRUE
#' ))
#'
#' tbls <- t_events_per_term_id(
#'   terms = ADAE[, c("AEBODSYS", "AEDECOD")] %>% map(as_factor_keep_attributes),
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM %>% by_add_total("All Patients"),
#'   col_N = col_N_add_total(table(ADSL$ARM)),
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' tbls[[1]]
#' tbls[['cl A']]
t_events_per_term_id <- function(terms,
                                 id,
                                 col_by,
                                 col_N = NULL, # nolint
                                 event_type = "event",
                                 table_tree = FALSE) {
  if (is.atomic(terms)) {
    terms <- list(terms)
  }
  stopifnot(is.list(terms))

  #todo: assign them to header row in tree
  #terms_header <- vapply(terms, label, character(1)) #nolintr

  total_events <- paste0("Total number of ", event_type, "s")
  subjects_with_events <- paste("Total number of patients with at least one", event_type)

  tree <- rsplit_to_tree(
    list(id = id, col_by = col_by),
    terms,
    drop_empty_levels = TRUE
  )
  tree@name <- paste0("- Any ", event_type, " -")
  tree@format_data$children_indent <- 0

  tree <- rapply_tree(
    tree,
    function(name, content, path) {
      if (length(path) == length(terms) + 1) {
        # don't show everything for leaves
        # this is a hack as total_events row.name is name
        list(name = invisible_node_name(name), content = t_el_events_per_term_id(
          id = content$id,
          col_by = content$col_by,
          col_N = col_N,
          total_events = NULL,
          subjects_with_events = name
        ))
      } else {
        list(name = name, content = t_el_events_per_term_id(
          id = content$id,
          col_by = content$col_by,
          col_N = col_N,
          total_events = total_events,
          subjects_with_events = subjects_with_events
        ))
      }
    }
  )

  # update format_data to display last nodes with less spacing
  tree <- full_apply_at_depth(
    tree,
    function(node) {
      node@format_data <- list(children_gap = 0, gap_to_children = 0)
      node
    },
    depth = length(terms) - 1
  )

  # todo: possibly sort with rsort_tree
  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

# Elementary Tables Used for AE tables ----
#' Count Unique Elements Per Cell
#'
#' \code{t_count_unique} counts the number of unique elements per cell.
#'
#' @inheritParams argument_convention
#' @param x a vector
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
#' @seealso \code{\link{t_events_per_term_id}}, \code{\link{t_el_events_per_term_id}},
#'   \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#'
#' t_count_unique(
#'  x = paste("id", c(1, 4, 2, 3, 3), sep = "-"),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#'
#' t_count_unique(
#'  x = c(1, 4, 2, 3, 3, NA),
#'  col_by = factor(c("A", "A", "B", "C", "C", "C")),
#'  col_N = c(2, 4, 10),
#'  row_name = "Unique Records",
#'  na_rm = FALSE
#' )
t_count_unique <- function(x,
                           col_by,
                           col_N = NULL, # nolint
                           na_rm = TRUE,
                           row_name = "number of unique elements") {
  stopifnot(is.atomic(x)) # todo: is this enough?
  col_by <- col_by_to_matrix(col_by, x)
  col_N <- col_N %||% get_N(col_by)
  check_col_by(x, col_by, col_N, min_num_levels = 1)

  counts <- vapply(col_by, function(rows) {
    xi <- x[rows]
    if (na_rm) {
      xi <- na.omit(xi)
    }
    length(unique(xi))
  }, numeric(1))
  percentage <- counts / col_N

  tbl <-  rtable(
    rheader(rrowl("", colnames(col_by))),
    rrowl(as.character(row_name), Map(c, counts, percentage), format = "xx.xx (xx.xx%)")
  )
  header_add_N(tbl, col_N)
}

#' Summary table for events
#'
#' \code{t_el_events_per_term_id} counts the number of unique elements per cell.
#'
#' @inheritParams argument_convention
#' @param id unique subject identifier. If a particular subject has no adverse
#'   event then that information needs to be added to the \code{col_N} argument.
#' @param total_events character string that will be used as a label in the row
#'   for the total event count. If this is \code{NULL} then this row will be
#'   removed.
#' @param subjects_with_events character string that will be used as a label in
#'   the row for the total number with at least one event. If this is
#'   \code{NULL} then this row will be removed.
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
#' @seealso \code{\link{t_count_unique}}, \code{\link{t_events_per_term_id}},
#'   \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#' t_el_events_per_term_id(
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#' #todo: rename total_events -> total_events_label
#' #todo: rename subjects_with_events -> subjects_with_events_label
t_el_events_per_term_id <- function(id,
                                    col_by,
                                    col_N = NULL,
                                    total_events = "Total number of events",
                                    subjects_with_events = "Total number of patients with at least one adverse event") {
  col_by <- col_by_to_matrix(col_by, x = id)
  col_N <- col_N %||% get_N(col_by)
  check_col_by(x = id, col_by, col_N, min_num_levels = 1)

  # subjects with at least one adverse advent (AE): we use the id as some subjects can have more than one AE
  tbl_at_least_one <- if (!is.null(subjects_with_events)) {
    t_count_unique(
      x = id,
      col_by = col_by,
      col_N = col_N,
      row_name = subjects_with_events
    )
  } else {
    NULL
  }

  tbl_events <- if (!is.null(total_events)) {
    res <- rtable(
      header = rheader(rrowl("", colnames(col_by))),
      rrowl(total_events, get_N(col_by))
    )
    header_add_N(res, col_N)
  } else {
    empty_rtable()
  }

  # if tbl_at_least_one is NULL, then incorrect rbind function is called, so we assign empty_rtable() above
  rbind(tbl_at_least_one, tbl_events)
}
