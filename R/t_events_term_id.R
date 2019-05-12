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
#' @return an \code{\link{rtable}} object.
#'
#' @export
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_count_unique}}, \code{\link{t_events_summary}},
#'   \code{\link{lt_events_per_term_id_2}}, \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(10, seed = 1)
#' ADAE <- radae(ADSL, 4, seed = 2)
#'
#' t_events_per_term_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = NULL,
#'   event_type = "adverse event"
#' )
#'
#' t_events_per_term_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   event_type = "adverse event"
#' )
#'
#' t_events_per_term_id(
#'   terms = ADAE[, c("AEBODSYS", "AEDECOD")],
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients"
#' )
#'
#' ADSL <- radsl(10, seed = 1)
#' ADCM <- radcm(ADSL, 5, seed = 4)
#' ADCM <- ADCM %>%
#'   dplyr::filter(ATIREL == "CONCOMITANT")
#'
#' t_events_per_term_id(
#'   terms = ADCM[, c("CMCLAS", "CMDECOD")],
#'   id = ADCM$USUBJID,
#'   col_by = ADCM$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   event_type = "treatment"
#' )
#'
#'
#' # Table Tree
#'
#' t_events_per_term_id(
#'   terms = ADAE$AEDECOD,
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   table_tree = TRUE
#' )
#'
#'
#' tbls <- t_events_per_term_id(
#'   terms = ADAE[, c("AEBODSYS", "AEDECOD")],
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients",
#'   table_tree = TRUE
#' )
#' summary(tbls)
#' names(tbls)
#' tbls[[1]]
#' rbind_table_tree(tbls)
#'
t_events_per_term_id <- function(terms,
                                 id,
                                 col_by,
                                 col_N, # nolint
                                 total = "All Patients",
                                 event_type = "event",
                                 table_tree = FALSE) {
  stopifnot(!is.null(terms))
  check_col_by(col_by, col_N, 1, total)

  if (is.data.frame(terms) && ncol(terms) == 1) {
    terms <- terms[[1]]
  }

  total_events <- paste0("Total number of ", event_type, "s")
  subjects_with_events <- paste("Total number of patients with at least one", event_type)

  order_indecies <- if (is.null(total)) {
    c(0, 1)
  } else {
    c(nlevels(col_by) + 1, 1)
  }

  tbls <- if (is.atomic(terms)) {

    df <- data.frame(term = terms, id = id, col_by = col_by, stringsAsFactors = FALSE)

    if (!is.null(total)) {
      .t <- add_total(x = df, col_by = col_by, total_level = total, col_N = col_N)
      col_N <- .t$col_N # nolint
      df <- data.frame(
        term = .t$x$term,
        id = paste(.t$x$id, "-", .t$col_by),
        col_by = .t$col_by,
        stringsAsFactors = FALSE
      )
    }

    tbl <- t_events_summary(
      term = df$term,
      id = df$id,
      col_by = df$col_by,
      col_N = col_N,
      total_events = total_events,
      subjects_with_events = subjects_with_events
    )

    if (nrow(tbl) > 2) {
      ord_rrows <- order_rrows(tbl[-c(1, 2), ], indices = order_indecies, decreasing = TRUE)
      tbl[c(1, 2, ord_rrows + 2), ]
    } else {
      tbl
    }

  } else if (ncol(terms) == 2) {

    l_tbls <- lt_events_per_term_id_2(
      terms = terms,
      id = id,
      col_by = col_by,
      col_N = col_N,
      total = total,
      event_type = event_type
    )

    l_tbls_o <- lapply(l_tbls, function(tbl) {
      if (nrow(tbl) > 3) {
        ord_rrows <- order_rrows(tbl[-c(1:3), ], indices = order_indecies, decreasing = TRUE)
        tbl[c(1:3, ord_rrows + 3), ] # as label row exists
      } else {
        tbl
      }
    })

    sort_rtables(l_tbls_o, indices = c(2, order_indecies), decreasing = TRUE)

  } else {
    stop("currently only one or two terms are supported")
  }

  if (table_tree) {
    table_tree(tbls)
  } else {
    rbind_table_tree(tbls)
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
#' @seealso \code{\link{t_events_per_term_id}}, \code{\link{t_events_summary}},
#'   \code{\link{lt_events_per_term_id_2}}, \code{\link{t_events_per_term_grade_id}}
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
#'  indent = 1,
#'  na_rm = FALSE
#' )
t_count_unique <- function(x,
                           col_by,
                           col_N, # nolint
                           na_rm = TRUE,
                           row_name = "number of unique elements",
                           indent = 0) {
  check_col_by(col_by, col_N, 1)

  counts <- vapply(split(x, col_by), function(xi) {
    if (na_rm) {
      xi <- na.omit(xi)
    }
    length(unique(xi))
  }, numeric(1))
  percentage <- counts / col_N

  tbl <-  rtable(
    rheader(rrowl("", levels(col_by))),
    rrowl(as.character(row_name), Map(c, counts, percentage), format = "xx.xx (xx.xx%)", indent = indent)
  )

  header_add_N(tbl, col_N)
}

#' Summary table for events
#'
#' \code{t_events_summary} counts the number of unique elements per cell.
#'
#' @inheritParams argument_convention
#' @param term a character vector with optional label attribute
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
#'   \code{\link{lt_events_per_term_id_2}}, \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#' t_events_summary(
#'  term = with_label(c("t1", "t1", "t2", "t2", "t2"), "Term"),
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#'
#' t_events_summary(
#'  term = with_label(c("t1", "t1", "t2", "t2", "t2"), "Term"),
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10),
#'  total_events = NULL
#' )
#'
#' t_events_summary(
#'  term = NULL,
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
t_events_summary <- function(term,
                             id,
                             col_by,
                             col_N, # nolint
                             total_events = "Total number of events",
                             subjects_with_events = "Total number of patients with at least one adverse event") {
  stopifnot(nlevels(col_by) == length(col_N))

  df <- if (is.null(term)) {
    data.frame(id = id, col_by = col_by, stringsAsFactors = FALSE)
  } else {
    data.frame(x = term, id = id, col_by = col_by, stringsAsFactors = FALSE)
  }

  if (any(is.na(df))) {
    stop("no NA allowed in x, id, and col_by")
  }

  term_label <- if (is.null(label(term))) {
    deparse(substitute(label))
  } else {
    label(term)
  }

  tbl_events <- if (!is.null(total_events)) {
    rtable(
      header = rheader(rrowl("", levels(col_by))),
      rrowl(total_events, table(df$col_by))
    )
  } else {
    NULL
  }

  tbl_at_least_one <- if (!is.null(subjects_with_events)) {
    t_count_unique(
      x = df$id,
      col_by = df$col_by,
      col_N = col_N,
      row_name = subjects_with_events,
      indent = 0
    )
  } else {
    NULL
  }

  tbls <- if (!is.null(df$x)) {
    tmp_tbls <- lapply(split(df, df$x), function(x) {
      t_count_unique(
        x = x$id,
        col_by = x$col_by,
        col_N = col_N,
        row_name = x$x[1],
        indent = 0
      )
    })
    rbindl_rtables(tmp_tbls)
  } else {
    NULL
  }

  tbl <-  rbind(tbl_at_least_one, tbl_events, tbls)

  # add label
  header(tbl) <- rheader(
    header(tbl)[[1]],
    rrowl(term_label, header(tbl)[[2]])
  )

  tbl
}

# Create Nested Lists of Tables that Compose Events tables ----
#' List of Events Terms Tables
#'
#' \code{lt_events_per_term_grade_id_2} returns a nested list of events tables
#' by unique id (\code{\link{t_count_unique}}).
#'
#' @inheritParams t_events_per_term_id
#'
#' @details
#' \if{html}{
#'
#' The data is split and table functions are applied to leaf nodes as follows:
#'
#' \figure{lt_events_per_term_id_2.png}{options: alt="lt_events_per_term_id_2 layout"}
#' }
#'
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#'
#' @seealso \code{\link{t_events_per_term_id}}, \code{\link{t_count_unique}},
#'   \code{\link{t_events_summary}}, \code{\link{t_events_per_term_grade_id}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(10 , seed = 1)
#' ADAE <- radae(ADSL, seed = 2)
#'
#' l_tbls <- tern:::lt_events_per_term_id_2(
#'   terms = ADAE[, c("AEBODSYS", "AEDECOD")],
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = "All Patients"
#' )
#' rbindl_rtables(l_tbls)
#'
#' l_tbls <- tern:::lt_events_per_term_id_2(
#'   terms = ADAE[, c("AEBODSYS", "AEDECOD")],
#'   id = ADAE$USUBJID,
#'   col_by = ADAE$ARM,
#'   col_N = table(ADSL$ARM),
#'   total = NULL
#' )
#'
#' rbindl_rtables(l_tbls)
lt_events_per_term_id_2 <- function(terms,
                                    id,
                                    col_by,
                                    col_N, # nolint
                                    total = "All Patients",
                                    event_type = "event") {
  check_col_by(col_by, col_N, min_num_levels = 1, total)
  stopifnot(
    !any(terms == "", na.rm = TRUE),
    is.data.frame(terms) && ncol(terms) == 2
  )

  class_label <- label(terms[[1]])
  term_label <- label(terms[[2]])
  if (is.null(class_label)) {
    class_label <- deparse(substitute(class))
  }
  if (is.null(term_label)) {
    term_label <- deparse(substitute(term))
  }

  total_events <- paste0("Total number of ", event_type, "s")
  subjects_with_events <- paste("Total number of patients with at least one", event_type)

  # data prep
  df <- data.frame(
    class = terms[[1]],
    term = terms[[2]],
    id = id,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  if (!is.null(total)) {
    .t <- add_total(x = df, col_by = col_by, total_level = total, col_N = col_N)
    col_N <- .t$col_N # nolint
    df <- data.frame(
      class = .t$x$class,
      term = .t$x$term,
      id = paste(.t$x$id, "-", .t$col_by),
      col_by = .t$col_by,
      stringsAsFactors = FALSE
    )
  }

  if (any(is.na(df))) {
    stop("partial missing data in rows of [class, term] is currently not supported")
  }


  # class and term chunks
  top_label <- paste0("- Any ", event_type, " -")
  df_class <- c(
    setNames(list(df[, -2]), top_label),
    split(df, df$class)
  )

  #  "Overall total number of events"
  tbl_header <- rheader(
    rrowl(class_label, levels(df$col_by))
  )

  tbls <- Map(function(df_terms, class_i) {

    tbl_i <- t_events_summary(
      term = df_terms$term,
      id = df_terms$id,
      col_by = df_terms$col_by,
      col_N = col_N,
      total_events = total_events,
      subjects_with_events = subjects_with_events
    )

    header(tbl_i) <- rheader(
      header(tbl_i)[[1]],
      rrowl(term_label, col_N, format = "(N=xx)", indent = 1)
    )
    tbl_i

  }, df_class, names(df_class))

  row.names(tbls[[1]])[1:2] <- c(subjects_with_events, total_events)

  tbls
}
