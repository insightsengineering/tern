# STREAM AE tables ----

#' Events by Highest Grade Table
#' 
#' This function summarizes number of unique subjects by highest grade and events term(s).
#' Events \code{terms} can be one level term or two level terms (one higher level and one lower level).
#' An implementation example is to apply \code{t_events_per_term_grade_id} on Adverse Event Data 
#' to create Adverse Events by Highest NCI CTCAE grade table 
#' (AET04, \href{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet04.html}{STREAM2.x},
#' \href{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027501}{STREAM1.17}). 
#' 
#' @inheritParams lt_events_per_term_grade_id_2
#' @param terms character or factor vector, or dataframe to represent events information; 
#'   Currently \code{terms} can only be a vector or dataframe with 1 or 2 columns.
#'   For \code{terms} with 2 columns, 1st column should represent higher level term and 2nd
#'   column should be lower level term.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term.
#' 
#' @details 
#' \code{t_events_per_term_grade_id} includes percentages based on the total number of subjects 
#' in the column heading (i.e. "N=nnn"). \code{col_N} can be explicitly specified to
#' get N for percentage calculation from either events dataset or additional dataset like
#' subject level dataset. See the example.  
#' 
#' Multiple events within a subject of the same term (if \code{terms} is one level) or lower level term 
#' (if \code{terms} is two levels) are counted once using the
#'  greatest intensity reported. 
#' 
#' \code{t_events_per_term_grade_id} doesn't deal with data with any non-complete records (has NA's), 
#' e.g. if any terms are missing. Impute missing values before using \code{t_events_per_term_grade_id}.
#'      
#' \code{t_events_per_term_grade_id} orders data by "All Patients" column from the most commonly
#'  reported higher level term to the least frequent one. Within each group of higher level term, 
#'  it sorts by decreasing frequency of lower level term. It brakes ties using \code{terms} names in alphabetical order.   
#' 
#' \code{t_events_per_term_grade_id} fills in \code{col_by} and \code{grade} with \code{0} value 
#' in case there was no events reported for particular \code{col_by} and/or 
#' \code{grade} category. Use \code{grade_levels} to modify the range of existing
#' grades. If data does not have any records with \code{grade} 5 and the intent 
#' is to show only grades 1-4 rows then use \code{grade_levels = 1:4}.
#' 
#'  
#' @return an \code{\link{rtable}} object.
#' 
#' @export
#' 
#' @template author_manukyae
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107 
#'  
#' @examples 
#' 
#' library(dplyr)
#' library(random.cdisc.data)
#' 
#' ASL <- rasl(10, seed = 1)
#' AAE <- raae(ASL, 4, seed = 2)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID")) %>%
#'   var_relabel(
#'     AEBODSYS = 'MedDRA System Organ Class',
#'     AEDECOD = 'MedDRA Preferred Term',
#'     AETOXGR = 'GRADE'
#' )
#' 
#' t_events_per_term_grade_id(
#'   terms = ANL$AEDECOD,
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#' 
#' t_events_per_term_grade_id(
#'   terms = ANL %>% select(AEBODSYS, AEDECOD),
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#' 
#' 
t_events_per_term_grade_id <- function(terms, id, grade, col_by, col_N, total = "All Patients", grade_levels = 1:5) {
  
  if (is.null(terms)) stop("terms can't be NULL")
  if (is.atomic(terms)) {
    terms <- data.frame(term = terms, stringsAsFactors = FALSE)
  } else if (!is.data.frame(terms)) {
    stop("terms needs to be either a vector or a data.frame")
  }
  
  if (ncol(terms) == 1) {
    l_tbls <- lt_events_per_term_grade_id_1(
      term = terms[[1]],
      id = id,
      grade = grade,
      col_by = col_by,
      col_N = col_N,
      total = total,
      grade_levels = grade_levels
    )
    n_cols <- ncol(l_tbls[[1]])
    if(!is.null(total)) n_cols <- n_cols-1
    
    N_total_any <- vapply(l_tbls, function(tbl) {
      a <- 0
      for(i in c(2:n_cols)){
        a <- a + tbl[1, i][1]
      }
      a
    }, numeric(1))
    l_tbls <- l_tbls[c(1, setdiff(order(-N_total_any, names(l_tbls), decreasing = FALSE), 1))]
    
    recursive_stack_rtables(l_tbls) 
  } else if (ncol(terms) == 2) {
    
    l_tbls <- lt_events_per_term_grade_id_2(
      terms = terms,
      id = id,
      grade = grade,
      col_by = col_by,
      col_N = col_N,
      total = total,
      grade_levels = grade_levels
    )
    
    n_cols <- ncol(l_tbls[[1]][[1]])
    if(!is.null(total))
      n_cols <- n_cols-1
    
    l_s_terms <- lapply(l_tbls, function(tbls) {
      
      # sort terms by any grade (sum of col_by levels)
      N_total_any <- vapply(tbls, function(tbl) {
        a <- 0
        for(i in c(2:n_cols)){
          a <- a + tbl[2, i][1]
        }
        a
      }, numeric(1))
      
      tbls[c(1, setdiff(order(-N_total_any, names(tbls), decreasing = FALSE), 1))]
      
    })
    
    # now sort tables by class (sum of col_by levels)
    N_total_overall <- vapply(l_s_terms, function(tbl) {
      a <- 0
      for(i in c(2:n_cols)){
        a <- a + tbl[[1]][2, i][1]
      }
      a
    }, numeric(1))
    
    l_tbls_sorted <- l_s_terms[order(-N_total_overall, names(l_s_terms), decreasing = FALSE)]
    
    # Now Stack them
    recursive_stack_rtables(nl_remove_n_first_rrows(l_tbls_sorted,1,2))
  } else {
    stop("currently one or two terms are summported")
  }
  
}


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
#' ASL <- rasl(10, seed = 1)
#' AAE <- raae(ASL, 4, seed = 2)
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
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients"
#' )
#' 
#' ACM <- racm(ASL, 5, seed = 4)
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
t_events_per_term_id <- function(terms, id, col_by, col_N, total = "All Patients", event_type = "event") {
 
  if (is.null(terms)) stop("terms can't be NULL")
  if (is.data.frame(terms) && ncol(terms) == 1) {
    terms <- terms[[1]]
  }  
  
  total_events = paste0("Total number of ", event_type, "s")
  subjects_with_events = paste("Total number of patients with at least one", event_type)
  
  tbls <- if (is.atomic(terms)){
    
    df <- data.frame(term = terms, id = id, col_by = col_by, stringsAsFactors = FALSE)
    if (!is.null(total)) {
      if (total %in% levels(col_by)) 
        stop(paste('col_by can not have', total, 'group.'))
      
      df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
      col_N <- c(col_N, sum(col_N))
    }
    
    l_tbls <- t_events_summary(
      term = df$term,
      id = df$id,
      col_by = df$col_by,
      col_N = col_N,
      total_events = total_events,
      subjects_with_events = subjects_with_events 
    )
    
    n_cols <- ncol(l_tbls)
    if(!is.null(total)) n_cols <- n_cols-1
    N_total_any <- vapply(l_tbls[3:nrow(l_tbls)], function(tbl) {
      a <- 0
      for(i in c(1:n_cols)){
        a <- a + tbl[[i]][1]
      }
      a
    }, numeric(1))
    
    l_tbls[c(1:2, (sort.int(-N_total_any, index.return=TRUE)[[2]]) + 2)]
    
  } else if (ncol(terms) == 2) {
    l_tbls <- lt_events_per_term_id_2(
      terms = terms,
      id = id,
      col_by = col_by,
      col_N = col_N,
      total = total, 
      event_type = event_type
    )
    
    n_cols <- ncol(l_tbls[[1]])
    if(!is.null(total))
      n_cols <- n_cols-1
    
    l_s_terms <- lapply(l_tbls[-1], function(tbls) {
      # sort terms by any term (sum of col_by levels)
      N_total_any <- vapply(tbls[4:nrow(tbls)], function(tbl) {
        a <- 0
        for(i in c(1:n_cols)){
          a <- a + tbl[[i]][1]
        }
        a
      }, numeric(1))
      
      tbls[c(1:3, (sort.int(-N_total_any, index.return=TRUE)[[2]])+3)]
      
    })
    
    
    # now sort tables by class total (sum of col_by levels)
    N_total_overall <- lapply(l_tbls[-1], function(tbl) {
      a <- 0
      for(i in c(1:n_cols)){
        if(!is.na(tbl[2, i][1])){
          a <- a + tbl[2, i][1]
        }
      }
      a
    })
    
    c(l_tbls[1], l_s_terms[order(-unlist(N_total_overall), names(l_s_terms), decreasing = FALSE)])
    
    
  } else {
    stop("currently only one or two terms are supported")
  }

  # Now Stack them
  recursive_stack_rtables(tbls)    

}


# Elementary Tables Used for AE tables ----

#' Tabulate maximum grade per id by \code{col_by}
#' 
#' \code{t_max_grade_per_id} is used for deriving adverse events tables, these are returned
#' as nested lists. 
#'  
#' @param grade a numeric vector with grade values
#' @param id a vector with id values
#' @param col_by a factor with values used for column names
#' @param col_N a vector with total n for each level of \code{col_by}
#' @param grade_levels a numeric vector used for naming rows for each level of
#'   grade
#' @param any_grade add a row that counts any occurrence, it is named \code{-Any
#'   Grade-} by default
#' 
#' @importFrom stats aggregate
#' @export
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' 
#' t_max_grade_per_id(
#'   grade =  c(1,2,3),
#'   id = c(1,1,1),
#'   col_by = factor(rep("A", 3)),
#'   col_N = 3
#' )
#' 
#' id <- c(1,1,2,2,3,3,3,4)
#' t_max_grade_per_id(
#'   grade =  c(1,2,3,2,1,1,2,3),
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4,3,5,3)
#' )
#' 
#' t_max_grade_per_id(
#'   grade =  c(1,2,3,2,1,1,2,3),
#'   id = id,
#'   col_by = factor(LETTERS[id]),
#'   col_N = c(4,3,5,4)
#' )
#' 
#' \dontrun{
#' # throws an error because each id can only have one col_by
#' t_max_grade_per_id(
#'   grade =  c(1,2,3),
#'   id = c(1,2,2),
#'   col_by = factor(LETTERS[1:3])
#' )
#' }
#' 
#' \dontrun{
#' # throws an error because grade NA is not in grade_levels 
#' t_max_grade_per_id(
#'   grade =  c(1,2,NA),
#'   id = c(1,2,3),
#'   col_by = factor(LETTERS[1:3])
#' )
#' }
#' 
t_max_grade_per_id <- function(grade, id, col_by, col_N,
                               grade_levels = NULL,
                               any_grade = "-Any Grade-") {
  
  if (!is.numeric(grade)) stop("grade is required to be numeric")
  if (any(is.na(id)) || any(is.na(col_by))) stop("no NA allowed in id and col_by")
  
  if (is.null(grade_levels)) grade_levels <- seq(1, max(grade, na.rm = TRUE))
  if (length(setdiff(grade, grade_levels))) stop("grades exist that are not in grade_levels")
  
  df <- data.frame(grade, id, col_by, stringsAsFactors = FALSE)
  
  df_max <- aggregate(grade ~ id + col_by, FUN = max, drop = TRUE, data = df, na.rm = TRUE)
  df_max$fct_grade <- factor(df_max$grade, levels = grade_levels)
  if (any(duplicated(df_max$id))) stop("every id can only have one col_by")
  
  if (is.null(col_N) || !is.null(any_grade)) {
    df_id <- df[!duplicated(df$id), ]    
  }
  
  if (is.null(col_N)) {
    col_N <- table(df_id$col_by)
  }
  
  if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  
  tbl_any <- if (!is.null(any_grade)) {
    rtabulate(
      na.omit(df_id),
      row_by_var = no_by(any_grade),
      col_by_var = "col_by", 
      FUN = count_perc_col_N,
      N = col_N,
      format = "xx (xx.x%)"
    )
  } else {
    NULL
  }
  
  tbl_x <- rtabulate(
    na.omit(df_max),
    row_by_var = "fct_grade",
    col_by_var = "col_by", 
    FUN = count_perc_col_N,
    N = col_N,
    format = "xx (xx.x%)"
  )
  
  tbl <- rbind(tbl_any, tbl_x)
  
  ## add (N=xx) row to header
  header(tbl) <- rheader(
    rrowl("", levels(col_by)),
    rrowl("", unname(col_N), format = "(N=xx)")
  )
  
  tbl
}

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
t_count_unique <- function(x, col_by, col_N = NULL, na.rm = TRUE, row.name = "number of unique elements", indent = 0) {
  
  check_col_by(col_by, 1)
  
  if (is.null(col_N)) {
    col_N <- table(col_by)
  } else {
    if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  }
  
  counts <- vapply(split(x, col_by), function(xi) {
    if (na.rm) xi <- na.omit(xi)
    length(unique(xi))
  }, numeric(1))
  percentage <- counts/col_N
  
  rtable(
    rheader(rrowl("", levels(col_by)), rrowl("", col_N, format = "(N=xx)")),
    rrowl(as.character(row.name), Map(c, counts, percentage), format= "xx.xx (xx.xx%)", indent = indent)
  )
  
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
  
  tbl_header <- rheader(
    rrowl("", levels(col_by)),
    rrowl(term_label, col_N, format = "(N=xx)")
  )

  tbl_events <- if( !is.null(total_events)) {
    rtable(
      header = tbl_header,
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
  
  if (!is.null(tbl_at_least_one)) attr(tbl_at_least_one, "header") <- tbl_header
  
  
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
    do.call(fast_rbind, tmp_tbls)
  } else {
    NULL
  }

  fast_rbind( tbl_at_least_one, tbl_events, tbls) 
}

# Create Nested Lists of Tables that Compose Events tables ----

#' List of Events Terms Tables by Highest Grade 
#' 
#' \code{lt_events_per_term_grade_id_2} returns a nested list of events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#' 
#' @param terms term information as character or factor dataframe, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class. Currently \code{terms} can only be a dataframe with 2 columns.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term. 
#' @param id vector of subject identifier. Length of \code{id} must be the same as the
#'   length or number of rows of \code{terms}.
#' @param grade grade of adverse event as numeric. \code{var_relabel} is used as the character 
#'   string used as a label in the column header for each grade. 
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param total character string that will be used as a label for a column with 
#'  pooled total population, default is "All Patients". If the levels of col_by are 
#'  the only columns of interest then total should be \code{NULL}
#' @param grade_levels numeric, ordered values of possible of grades in a form
#'   of \code{x:y}, default is \code{1:5}.
#' 
#' 
#' @details 
#' \if{html}{
#' 
#' The data is split and table functions are applied to leaf nodes as follows:
#' 
#' \figure{lt_events_per_term_grade_id_2.png}{options: alt="lt_events_per_term_grade_id_2 layout"}
#' }
#' 
#' 
#' @return rtable
#' 
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
#' ASL <- rasl(10, seed = 1)
#' AAE <- raae(ASL, 4, seed = 2)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID"))
#' 
#' l_tbls <- tern:::lt_events_per_term_grade_id_2(
#'   terms = ANL %>% select(AEBODSYS, AEDECOD),
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#' 
#' do.call(tern:::fast_stack_rtables,
#'   tern:::unlist_rtables(
#'     tern:::nl_remove_n_first_rrows(l_tbls)
#'   )
#' )
#'
lt_events_per_term_grade_id_2 <- function(terms, 
                                       id, 
                                       grade, 
                                       col_by, 
                                       col_N,
                                       total = "All Patients",
                                       grade_levels) {
  
  # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("- Overall -" %in% terms)) stop("'- Overall -' is not a valid term, t_ae_ctc reserves it for derivation")
  if (any(total %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(terms == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  if(!is.data.frame(terms) || ncol(terms) != 2) stop("terms must be a dataframe with two columns")
  
  class_label <- label(terms[[1]])
  term_label <- label(terms[[2]])
  grade_label <- label(grade)
  
  if(is.null(term_label)) class_label <- deparse(substitute(class))
  if(is.null(term_label)) term_label <- deparse(substitute(term))
  if(is.null(grade_label)) grade_label <- deparse(substitute(grade))
  
  # data prep
  df <- data.frame(
    class = terms[[1]],
    term = terms[[2]],
    id = id,
    grade = grade,
    col_by = col_by,
    stringsAsFactors = FALSE
  )
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, term, grade] is currently not supported")
  
  # adding All Patients
  if(!is.null(total)){
    if (total %in% levels(col_by)) 
      stop(paste('col_by can not have', total, 'group. t_ae_ctc will derive it.'))
    
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N))
  } 
  
  # start tabulatings
  
  # create nested list with data used for creating the sub tables
  df_class_term <- c(
    list("- Any adverse events -" = list( "- Overall -" = df)),
    lapply(split(df, df$class), function(x) {
      c(
        list("- Overall -" = x),
        split(x, x$term)      
      )
    })
  )
  
  tbl_header <- rheader(
    rrowl(class_label, c(list(NULL), as.list(levels(df$col_by)))),
    rrowl(term_label, c(list(rcell(grade_label, format="xx")), as.list(col_N)), format = "(N=xx)", indent = 1)
  )
  
  
  # now create the tables
  Map(function(df_terms, class_name) {
    Map(function(df_term, term_name) {
      
      tbl_raw <- t_max_grade_per_id(
        grade = df_term$grade,
        id = df_term$id,
        col_by = df_term$col_by,
        col_N = col_N,
        grade_levels = grade_levels,
        any_grade = "- Any Grade -"
      )
      
      ## move rownames to column
      tbl <- row_names_as_col(tbl_raw)
      row.names(tbl)[1] <- term_name
      tbl <- fast_rbind(
        rtable(header(tbl), rrow(class_name)),
        indent_table(tbl, 1)
      )
      attr(tbl, "header") <- tbl_header
      
      tbl
      
    }, df_terms, names(df_terms))
  }, df_class_term, names(df_class_term) )
  
}


#' List of Events Terms Tables By Highest Grade (One Level Term only)
#' 
#' \code{lt_events_per_term_grade_id_1} returns a nested list of events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#' 
#' 
#' @inheritParams lt_events_per_term_grade_id_2
#' @param term term information as character or factor, however factor levels
#'   are not repeated by class, only terms with count > 1 are listed per class.
#'   \code{var_relabel} is used as the character string used as a label in the
#'   column header for each term.
#' 
#' 
#' @details 
#' \if{html}{
#' 
#' The data is split and table functions are applied to leaf nodes as follows:
#' 
#' \figure{lt_events_per_term_grade_id_1.png}{options: alt="lt_events_per_term_grade_id_1 layout"}
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
#' ASL <- rasl(10, seed = 1)
#' AAE <- raae(ASL, 4, seed = 2)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID"))
#' 
#' l_tbls <- tern:::lt_events_per_term_grade_id_1(
#'   term = ANL$AEDECOD,
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#' 
#' do.call(fast_stack_rtables, l_tbls)
#' 
lt_events_per_term_grade_id_1 <- function(term, 
                                 id, 
                                 grade, 
                                 col_by, 
                                 col_N,
                                 total = "All Patients",
                                 grade_levels) {
  
  
  # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  term_label <- attr(term, "label")
  grade_label <- attr(grade, "label")
  
  if(is.null(term_label)) term_label <- deparse(substitute(term))
  if(is.null(grade_label)) grade_label <- deparse(substitute(grade))
  
  # data prep
  df <- data.frame(
    term = term,
    id = id,
    grade = grade,
    col_by = col_by,
    stringsAsFactors = FALSE
  )
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, grade] is currently not supported")
  
  # adding All Patients
  if(!is.null(total)){
    if (total %in% levels(col_by)) 
      stop(paste('col_by can not have', total, 'group. t_ae_ctc will derive it.'))
    
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N)) 
  }
  
  # start tabulating
  
  df_terms <- c(
    list("- Overall -" = df),
    split(df, df$term)      
  )
  
  tbl_header <- rheader(
    rrowl("", c(list(NULL), levels(df$col_by))),
    rrowl(term_label, c(list(rrow(grade_label, format="xx")), col_N), format = "(N=xx)")
  )
  
  Map(function(df_term, term_i) {
    
    tbl_raw <- t_max_grade_per_id(
      grade = df_term$grade,
      id = df_term$id,
      col_by = df_term$col_by,
      col_N = col_N,
      grade_levels = grade_levels,
      any_grade = "- Any Grade -"
    )
    
    tbl <- row_names_as_col(tbl_raw)
    row.names(tbl)[1] <- term_i
    attr(tbl, "header") <- tbl_header
    
    tbl
    
  }, df_terms, names(df_terms))
  
}


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
#' ASL <- rasl(10, seed = 1)
#' AAE <- raae(ASL, 4, seed = 2)
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
#' do.call(fast_stack_rtables, l_tbls)
#' 
#' l_tbls <- tern:::lt_events_per_term_id_2(
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = table(ASL$ARM),
#'   total = NULL
#' )
#' do.call(fast_stack_rtables, l_tbls)
#' 
#' 
lt_events_per_term_id_2 <- function(terms, 
                             id,  
                             col_by, 
                             col_N,
                             total = "All Patients", 
                             event_type = "event"){
  
  # check argument validity and consitency
  check_col_by(col_by, min_num_levels = 1)
  
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
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, term] is currently not supported")
  
  # adding All Patients
  if (!is.null(total)) {
    if (total %in% levels(col_by)) 
      stop(paste('col_by can not have', total, 'group. t_ae will derive it.'))
    
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N))
  }
  
  # list("- Any adverse events -" = list( "- Overall -" = df))
  # class and term chunks
  top_label = paste0("- Any ", event_type, " -")
  df_class <- c(
    setNames(list(df[, -2]), top_label),
    split(df, df$class)
  )
  
  #  "Overall total number of events"
  tbl_header <- rheader(
    rrowl(class_label, levels(df$col_by)),
    rrowl(term_label, col_N, format = "(N=xx)", indent = 1)
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
     
    fast_rbind(
      rtable(header = tbl_header, rrow(class_i)),
      indent_table(tbl_i, 1)
    )
    
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



