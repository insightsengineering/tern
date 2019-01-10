#' Basic Events Table
#'
#' This function summarizes number of unique subjects with events and total number of events.
#' It creates basic summary of events and can be used for any events data like Adverse Events,
#' concomitant medication, medical history, etc.
#' Implementation examples are to apply \code{t_events_per_term_id} on Adverse Event data 
#' to create Adverse Events summary table 
#' (AET02, \href{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet02.html#example-report-outputs-aet02-aet02}{STREAM2.x},
#' \href{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027342}{STREAM1.17} ),
#' or apply on Concomitatant Medication data to create Concomitant Treatment summary table
#' (CMT01, \href{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_cmt01.html#example-report-outputs-cmt01-cmt01}{STREAM2.x},
#' \href{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027342}{STREAM1.17}).
#'
#' 
#' @inheritParams lt_events_per_term_id_2
#' @param terms character or factor vector, or dataframe to represent events information; 
#'   Currently \code{terms} can only be a vector or dataframe with 1 or 2 columns.
#'   For \code{terms} with 2 columns, 1st column should represent higher level term and 2nd
#'   column should be lower level term.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term.
#' 
#' @details 
#' \code{t_events_per_term_id} includes percentages based on the total number of subjects 
#' in the column heading (i.e. "N=nnn"). \code{col_N} can be explicitly specified to
#' get N for percentage calculation from either events dataset or additional dataset such as
#' subject level dataset. See the example.  
#' 
#' Multiple events within a subject of the same term (if \code{terms} is one level) or lower level term 
#' (if \code{terms} is two levels) are counted once when counting number of subjects. 
#' 
#' \code{t_events_per_term_id} doesn't deal with data with any non-complete records (has NA's), 
#' e.g. if any terms are missing. Impute missing values before using \code{t_events_per_term_id}.
#'      
#' \code{t_events_per_term_id} orders data by "All Patients" column from the most commonly
#'  reported higher level term to the least frequent one. Within each group of higher level term, 
#'  it sorts by decreasing frequency of lower level term. It brakes ties using \code{terms} names in alphabetical order. 
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
#'  
#' @examples 
#'
#' library(dplyr)
#' library(random.cdisc.data)
#' 
#' ASL <- radsl(10, seed = 1)
#' AAE <- radae(ASL, 4, seed = 2)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID")) %>%
#'   var_relabel(
#'     AEBODSYS = 'MedDRA System Organ Class',
#'     AEDECOD = 'MedDRA Preferred Term',
#'     AETOXGR = 'GRADE'
#' )
#' 
#' t_events_per_term_id(
#'   terms = ANL$AEDECOD,
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = NULL,
#'   event_type = "adverse event"
#' )
#' 
#' t_events_per_term_id(
#'   terms = ANL$AEDECOD,
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients",
#'   event_type = "adverse event"
#' )
#' 
#' t_events_per_term_id(
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients"
#' )
#' 
#' 
#' ASL <- radsl(10, seed = 1)
#' ACM <- radcm(ASL, 5, seed = 4)
#' ANL <- left_join(ACM, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID")) %>%
#'        filter(CMTIREL == "CONCOMITANT") %>%
#'   var_relabel(
#'     CMCLAS = 'Medication Class',
#'     CMDECOD = 'Standardized Medication Name'
#' )    
#' t_events_per_term_id(
#'   terms = ANL[, c("CMCLAS", "CMDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients",
#'   event_type = "treatment"
#' ) 
#' 
t_events_per_term_id <- function(terms, id, col_by, col_N, total = "All Patients", 
                                 event_type = "event") {
  
  check_col_by(col_by, col_N, 1, total)
  if (is.null(terms)) stop("terms can't be NULL")
  if (is.data.frame(terms) && ncol(terms) == 1) {
    terms <- terms[[1]]
  }  
  
  total_events <- paste0("Total number of ", event_type, "s")
  subjects_with_events <- paste("Total number of patients with at least one", event_type)
  
  order_indecies <- if(is.null(total)) c(0, 1) else c(nlevels(col_by) + 1, 1)
  
  tbls <- if (is.atomic(terms)) {
    
    df <- data.frame(term = terms, id = id, col_by = col_by, stringsAsFactors = FALSE)
    
    if (!is.null(total)) {
      .t <- add_total(x = df, col_by = col_by, total_level = total, col_N = col_N)
      col_N <- .t$col_N
      df <- data.frame(term = .t$x$term, id = paste(.t$x$id, "-", .t$col_by), 
                       col_by = .t$col_by, stringsAsFactors = FALSE)
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
      ord_rrows <- order_rrows(tbl[-c(1,2), ], indices = order_indecies, decreasing = TRUE)
      tbl[c(1, 2, ord_rrows+2), ]
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
  
  recursive_stack_rtables(tbls)    
  
}



# Elementary Tables Used for AE tables ----
#' Count Unique Elements Per Cell
#' 
#' \code{t_count_unique} counts the number of unique elements per cell.
#' 
#' @inheritParams rtables::rtabulate
#' @param x a vector
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param col_N associated number of elements per level of \code{col_by}
#' 
#' @return an rtable
#' 
#' @export
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' 
#' t_count_unique(
#'  x = paste("id", c(1, 4, 2, 3, 3), sep = "-"),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#' 
t_count_unique <- function(x, col_by, col_N, na.rm = TRUE, 
                           row.name = "number of unique elements", indent = 0) {
  
  check_col_by(col_by, col_N, 1)
  
  counts <- vapply(split(x, col_by), function(xi) {
    if (na.rm) xi <- na.omit(xi)
    length(unique(xi))
  }, numeric(1))
  percentage <- counts/col_N
  
  tbl <-  rtable(
    rheader(rrowl("", levels(col_by)) ),
    rrowl(as.character(row.name), Map(c, counts, percentage), format= "xx.xx (xx.xx%)", indent = indent)
  )
  header_add_N(tbl, col_N)
  
}

#' Summary table for events
#' 
#' \code{t_events_summary} counts the number of unique elements per cell.
#' 
#' @param term a character vector with optional label attribute
#' @param id unique subject identifier. If a particular subject has no adverse 
#'   event then that information needs to be added to the \code{col_N} argument.
#' @param col_by group variable that will be used for a column header.
#'   \code{col_by} has to be a factor and can not be missing. See 'Examples'.
#' @param col_N associated number of elements per level of \code{col_by}
#' @param total_events character string that will be used as a label in the row
#'   for the total event count. If this is \code{NULL} then this row will be
#'   removed.
#' @param subjects_with_events character string that will be used as a label in
#'   the row for the total number with at least one event. If this is
#'   \code{NULL} then this row will be removed.
#' 
#' @return an rtable
#' 
#' @export
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' 
#' t_events_summary(
#'  term = with_label(c("t1", "t1", "t2", "t2", "t2"), "Term"),
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#' 
#' 
#' t_events_summary(
#'  term = with_label(c("t1", "t1", "t2", "t2", "t2"), "Term"),
#'  id = c(1, 4, 2, 3, 3),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10),
#'  total_events = NULL
#' )
#' 
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
                             col_N, 
                             total_events = "Total number of events", 
                             subjects_with_events = "Total number of patients with at least one adverse event"){
  
  if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  
  df <- if (is.null(term)) {
    data.frame(id = id, col_by = col_by, stringsAsFactors = FALSE)
  } else {
    data.frame(x = term, id = id, col_by = col_by, stringsAsFactors = FALSE)
  }
  
  if (any(is.na(df))) stop("no NA allowed in x, id, and col_by")
  
  term_label <- if (is.null(label(term))) deparse(substitute(label)) else label(term)
  tbl_events <- if( !is.null(total_events)) {
    rtable(
      header = rheader(rrowl("", levels(col_by))),
      rrowl(total_events, table(df$col_by))
    )
    
  } else {
    NULL
  }
  
  tbl_at_least_one <- if( !is.null(subjects_with_events)) {
    t_count_unique(
      x = df$id,
      col_by = df$col_by,
      col_N = col_N,
      row.name = subjects_with_events,
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
        row.name = x$x[1],
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
#'
#' @param terms term information as character or factor dataframe, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class. Currently \code{terms} can only be a dataframe with 2 columns.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term. 
#' @param id vector of subject identifier. Length of \code{id} must be the same as the
#'   length or number of rows of \code{terms}.
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param total character string that will be used as a label for a column with 
#'  pooled total population, default is "All Patients". If the levels of col_by are 
#'  the only columns of interest then total should be \code{NULL}
#' @param event_type type of event that is summarized (e.g. adverse event,
#'   treatment). Default is "event".
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
#' @examples 
#' 
#' library(dplyr)
#' library(random.cdisc.data)
#' 
#' ASL <- radsl(10 , see = 4 )
#' AAE <- radae(ASL, seed = 4 )
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID")) %>%
#'   var_relabel(
#'     AEBODSYS = 'MedDRA System Organ Class',
#'     AEDECOD = 'MedDRA Preferred Term',
#'     AETOXGR = 'GRADE'
#' )
#' 
#' l_tbls <- tern:::lt_events_per_term_id_2(
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients"
#' )
#' rbindl_rtables(l_tbls)
#' 
#' l_tbls <- tern:::lt_events_per_term_id_2(
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = NULL
#' )
#' rbindl_rtables(l_tbls)
#' 
#' 
lt_events_per_term_id_2 <- function(terms, 
                                    id,  
                                    col_by, 
                                    col_N,
                                    total = "All Patients", 
                                    event_type = "event"){
  
  # check argument validity and consitency
  check_col_by(col_by, col_N, min_num_levels = 1, total)
  
  if (any(terms == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  if(!is.data.frame(terms) || ncol(terms) != 2) 
    stop("terms must be a dataframe with two columns")
  
  class_label <- label(terms[[1]])
  term_label <- label(terms[[2]])
  if(is.null(class_label)) class_label <- deparse(substitute(class))
  if(is.null(term_label)) term_label <- deparse(substitute(term))
  
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
    col_N <- .t$col_N
    df <- data.frame(class = .t$x$class, term = .t$x$term, id = paste(.t$x$id, "-", .t$col_by), 
                     col_by = .t$col_by, stringsAsFactors = FALSE)
  }
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, term] is currently not supported")
  
  
  # list("- Any adverse events -" = list( "- Overall -" = df))
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
    
    tbl_i <- rbind(
      rtable(header = tbl_header, rrow(class_i)),
      indent_table(tbl_i, 1)
    )
    header(tbl_i) <- rheader(
      header(tbl_i)[[1]],
      rrowl(term_label, col_N, format = "(N=xx)", indent = 1)
    )
    tbl_i
    
  }, df_class, names(df_class))
  
  row.names(tbls[[1]])[2:3] <- c(subjects_with_events, total_events)
  
  tbls
}





# Helper Functions Used to Convert the Nested Lists to Single AE tables ----


#' Remove first n rows in a list of lists of rtables
#' 
#' @noRd
#' 
#' @examples 
#' 
#' tbl <- rbind(
#'    rtabulate(iris$Sepal.Length, iris$Species, mean),
#'    rtabulate(iris$Sepal.Length, iris$Species, sd)
#' )
#' 
#' l_tbls <- list(
#'   list(
#'     tbl, tbl, tbl
#'   ),
#'   list(
#'     tbl, tbl
#'   )  
#' )
#' 
#' tern:::nl_remove_n_first_rrows(l_tbls, n = 1, 2)
#' 
nl_remove_n_first_rrows <- function(x, n=1, lower_childindex_threshold = 0) {
  lapply(x, function(xi) {
    i <- 0
    lapply(xi, function(xii) {
      i <<- i + 1
      if (i >= lower_childindex_threshold) xii[-seq(1, n, by = 1),] else xii
    })
  })
}

