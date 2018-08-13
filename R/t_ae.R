# STREAM AE tables ----

#' Adverse Events Table by Highest NCI CTCAE Grade
#' 
#' \code{t_events_per_term_grade_id} returns adverse events sorted by highest NCI (National Cancer
#'  Institute) CTCAE (common terminology criteria for adverse avents) grade.
#' 
#' @param terms term information as character or factor vector or dataframe, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class. Currently \code{terms} can only be a vector or dataframe with 1/2 column.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term.
#' @inheritParams lt_ae_max_grade_class_term
#' 
#' @details 
#' \code{t_events_per_term_grade_id} counts patients according to adverse events (AEs) of greatest
#'  intensity for system organ class (SOC) and overall rows and includes 
#'  percentages based on the total number of patients in the column heading 
#'  (i.e. "N=nnn"). If the intention is to use patients number from subject level
#'  dataset as N for percentage calculation then adeverse events dataset should
#'  be left joined to subject level dataset and the \code{col_by} variable should
#'  be dropped from adverse events dataset, see the example. Otherwise, N will be
#'  derived using adverse events dataset. At the preferred term (PT) level,
#'  multiple events within a patient of the same PT are counted once using the
#'  greatest intensity reported. 
#' 
#' \code{t_events_per_term_grade_id} removes any non-complete records, e.g. if class or term are 
#'  missing. If the intent is to preserve such records, then impute missing 
#'  values before using \code{t_events_per_term_grade_id}.
#'      
#' \code{t_events_per_term_grade_id} orders data by "All Patients" column from the most commonly
#'  reported SOC to the least frequent one. Within SOC, it sorts by decreasing
#'  frequency of PT. It brakes ties using SOC/PT names in alphabetical order.   
#' 
#' \code{t_events_per_term_grade_id} fills in \code{col_by} and \code{grade} with \code{0} value 
#' in case there was no AEs reported for particular \code{col_by} and/or 
#' \code{grade} category. Use \code{grade_levels} to modify the range of existing
#' grades. If data does not have any records with \code{grade} 5 and the intent 
#' is to show only grades 1-4 rows then use \code{grade_levels = 1:4}.
#' 
#' This is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet04)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet04.html})
#' 
#' This is an equivalent of the STREAM 1.17 output \code{\%stream_t_events_bygrade(templates = aet04)}
#'   (\url{<https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027501>})
#'  
#' @return \code{rtable} object
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
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#' 
#' t_events_per_term_grade_id(
#'   terms = ANL %>% select(AEBODSYS, AEDECOD),
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
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
    l_tbls <- lt_ae_max_grade_term(
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
    
    l_tbls <- lt_ae_max_grade_class_term(
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


#' Adverse Events by System Organ Class and/or Preferred Term
#'
#' \code{t_events_per_term_id} returns adverse events summary table that
#' corresponds to STREAM template AET02
#'
#'
#' @inheritParams lt_ae_class_term
#' @param terms term information as character or factor vector or dataframe,
#'   however factor levels are not repeated by class, only terms with count > 1
#'   are listed per class. Currently \code{terms} can only be a vector or
#'   dataframe with 1/2 column. \code{var_relabel} is used as the character
#'   string used as a label in the column header for each term.
#' 
#' @details 
#' This is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet02)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet02.html})
#' 
#' This is an equivalent of the STREAM 1.17 output \code{\%stream_t_events_basic(templates = aet02)}
#'   (\url{https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027342})
#'
#' @return \code{rtable} object 
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
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = NULL
#' )
#' 
#' t_events_per_term_id(
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' 
#' 
t_events_per_term_id <- function(terms, id, col_by, col_N, total = "All Patients") {
 
  if (is.null(terms)) stop("terms can't be NULL")
  if (is(terms, "vector")){
    label <- attr(terms, "label")
    terms <- data.frame(term = terms, stringsAsFactors = FALSE)
    if (!is.null(label)) var_labels(terms) <- label
  }  
  
  if (is(terms, "data.frame") && ncol(terms) == 1){
    l_tbls <- lt_ae_term(
      term = terms,
      id = id,
      col_by = col_by,
      col_N = col_N,
      total = total
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
    l_tbls <- l_tbls[c(1:2, (sort.int(-N_total_any, index.return=TRUE)[[2]]) + 2)]
    return(l_tbls)
  }

  
  
  if(!is.data.frame(terms) || ncol(terms) != 2) stop("terms must be a dataframe with two columns")
  
  l_tbls <- lt_ae_class_term(
    terms = terms,
    id = id,
    col_by = col_by,
    col_N = col_N,
    total = total
  )
  
  n_cols <- ncol(l_tbls[[1]])
  if(!is.null(total))
    n_cols <- n_cols-1
  
  l_s_terms <- lapply(l_tbls, function(tbls) {
    # sort terms by any term (sum of col_by levels)
    N_total_any <- vapply(tbls[2:nrow(tbls)], function(tbl) {
      a <- 0
      for(i in c(1:n_cols)){
        a <- a + tbl[[i]][1]
      }
      a
    }, numeric(1))
    
    tbls[c(1, (sort.int(-N_total_any, index.return=TRUE)[[2]])+1)]
    
  })
  
  
  # now sort tables by class total (sum of col_by levels)
  N_total_overall <- lapply(l_tbls, function(tbl) {
    a <- 0
    for(i in c(1:n_cols)){
      if(!is.na(tbl[2, i][1])){
        a <- a + tbl[2, i][1]
      }
    }
    a
  })
  
  l_tbls_sorted <- l_s_terms[order(-unlist(N_total_overall), names(l_s_terms), decreasing = FALSE)]
  
  
  # Now Stack them
  recursive_stack_rtables(l_tbls_sorted)
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
#'   col_by = factor(rep("A", 3))
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
#'   col_by = factor(LETTERS[id])
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
t_count_unique <- function(x, col_by, col_N = NULL, row.name = "number of unique elements", indent = 0) {
  
  check_col_by(col_by, 1)
  
  if (any(is.na(x))) stop("x does currently not support NAs")
  
  if (is.null(col_N)) {
    col_N <- tapply(col_by, col_by, length)
  } else {
    if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  }
  
  counts <- tapply(x, col_by, function(x)length(unique(x)))
  counts[is.na(counts)] = 0
  percentage <- counts/col_N
  
  rtable(
    rheader(rrowl("",levels(col_by)), rrowl("", col_N, format = "(N=xx)")),
    rrowl(as.character(row.name), Map(c, counts, percentage), format= "xx.xx (xx.xx%)", indent = indent)
  )
  
}

#' Summary table for events
#' 
#' \code{t_events_summary} counts the number of unique elements per cell.
#' 
#' @param x a character vector with optional label attribute
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

  df <- if (is.null(term)) {
    data.frame(id = id, col_by = col_by, stringsAsFactors = FALSE)
  } else {
    data.frame(x = term, id = id, col_by = col_by, stringsAsFactors = FALSE)
  }
  
  if (any(is.na(df))) stop("no NA allowed in x, id, and col_by")
  
  term_label <- if (is.null(label(term))) deparse(substitute(label)) else label(term)

  if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  
  tbl_header <- rheader(
    rrowl("", levels(col_by)),
    rrowl(term_label, col_N, format = "(N=xx)")
  )

  tbl_events <- if( !is.null(total_events)) {
    rtable(
      header = tbl_header,
      rrowl(total_events, tapply(df$col_by, df$col_by, length))
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
    do.call(fast_rbind, tmp_tbls)
  } else {
    NULL
  }

  fast_rbind(tbl_events, tbl_at_least_one, tbls) 
}

# Create Nested Lists of Tables that Compose AE tables ----

#' List of Adverse Events Terms Tables by Highest Grade 
#' 
#' \code{lt_ae_max_grade_class_term} returns a nested list of adverse events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#' 
#' @param terms term information as character or factor dataframe, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class. Currently \code{terms} can only be a dataframe with 2 columns.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term. 
#' @param id unique subject identifier. If a particular subject has no adverse
#'   event then that information needs to be added to the \code{col_N} argument.
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
#' l_tbls <- tern:::lt_ae_max_grade_class_term(
#'   terms = ANL %>% select(AEBODSYS, AEDECOD),
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
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
lt_ae_max_grade_class_term <- function(terms, 
                                       id, 
                                       grade, 
                                       col_by, 
                                       col_N = tapply(col_by, col_by, length),
                                       total = "All Patients",
                                       grade_levels) {
  
  # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("- Overall -" %in% terms)) stop("'- Overall -' is not a valid term, t_ae_ctc reserves it for derivation")
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(terms == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  if(!is.data.frame(terms) || ncol(terms) != 2) stop("terms must be a dataframe with two columns")
  
  class_label <- var_labels(terms)[1]
  term_label <- var_labels(terms)[2]
  grade_label <- attr(grade, "label")
  if(is.na(term_label)) class_label <- deparse(substitute(class))
  if(is.na(term_label)) term_label <- deparse(substitute(term))
  if(is.na(grade_label)) grade_label <- deparse(substitute(grade))
  
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


#' List of Adverse Events Terms Tables By Highest Grade (Term only)
#' 
#' \code{lt_ae_max_grade_term} returns a nested list of adverse events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#' 
#' @param term term information as character or factor, however factor levels
#'   are not repeated by class, only terms with count > 1 are listed per class.
#'   \code{var_relabel} is used as the character string used as a label in the
#'   column header for each term.
#' @inheritParams lt_ae_max_grade_class_term
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
#' l_tbls <- tern:::lt_ae_max_grade_term(
#'   term = ANL$AEDECOD,
#'   id = ANL$USUBJID,
#'   grade = ANL$AETOXGR,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   grade_levels = 1:5
#' )
#' 
#' do.call(fast_stack_rtables, l_tbls)
#' 
lt_ae_max_grade_term <- function(term, 
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


#' List of Adverse Events Terms Tables 
#' 
#' \code{lt_ae_max_grade_class_term} returns a nested list of adverse events tables 
#' by unique id (\code{\link{t_count_unique}}).
#'
#'
#' @param terms term information as character or factor dataframe, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class. Currently \code{terms} can only be a dataframe with 2 columns.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term. 
#' @param id unique subject identifier. If a particular subject has no adverse
#'   event then that information needs to be added to the \code{col_N} argument.
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param col_N numeric vector with information of the number of patients in the
#'   levels of \code{col_by}. This is useful if there are patients that have no
#'   adverse events can be accounted for with this argument.
#' @param total character string that will be used as a label for a column with 
#'  pooled total population, default is "All Patients". If the levels of col_by are 
#'  the only columns of interest then total should be \code{NULL}
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
#' l_tbls <- tern:::lt_ae_class_term(
#'   terms = ANL[, c("AEBODSYS", "AEDECOD")],
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' recursive_stack_rtables(l_tbls)
#' 
lt_ae_class_term <- function(terms, 
                             id,  
                             col_by, 
                             col_N = tapply(col_by, col_by, length),
                             total = "All Patients"){
  
  # check argument validity and consitency
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae  derives All Patients column")
  
  if (any(terms == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  if(!is.data.frame(terms) || ncol(terms) != 2) 
    stop("terms must be a dataframe with two columns")
  
  class_label <- var_labels(terms)[1]
  term_label <- var_labels(terms)[2]
  if(is.na(class_label))
    class_label <- deparse(substitute(class))
  if(is.na(term_label))
    term_label <- deparse(substitute(term))
  
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
  df_class <- c(
    list("- Any Adverse Event -" = df[, -2]),
    split(df, df$class)
  )
  
  #  "Overall total number of events"
  tbl_header <- rheader(
    rrowl(class_label, levels(df$col_by)),
    rrowl(term_label, col_N, format = "(N=xx)", indent = 1)
  )
  
  tbls <- Map(function(df_terms, class_i) {
    
    tbl_start <- rtable(
      header = tbl_header,
      rrow(class_i),
      rrowl("Total number of events", tapply(df_terms$col_by, df_terms$col_by, length) , indent = 1)
    )
    
    tbl_tot_unique <- t_count_unique(
      x = df_terms$id,
      col_by = df_terms$col_by,
      col_N = col_N,
      row.name = "Total number of patients with at least one adverse event",
      indent = 1
    )
    
    tbl_terms <- if (!is.null(df_terms$term)) {
      tbls <- lapply(split(df_terms, factor(df_terms$term, levels = unique(df_terms$term))), function(x) {
        t_count_unique(
          x = x$id,
          col_by = x$col_by,
          col_N = col_N,
          row.name = x$term[1],
          indent = 1
        )
      })
      do.call(fast_rbind, tbls)
    } else {
      NULL
    }
    
    fast_rbind(tbl_start, tbl_tot_unique, tbl_terms)
    
  }, df_class, names(df_class))
  
  
  row.names(tbls[[1]])[2:3] <- c("Overall total number of events", "Overall total number of patients with at least one adverse event")
  
  tbls 
}



#' List of Adverse Events Terms Tables (Term only)
#'
#' \code{lt_ae_term} returns a nested list of adverse events tables by unique id
#'  (\code{\link{t_count_unique}}).
#'
#' @inheritParams lt_ae_class_term 
#' @param terms term information as character or factor dataframe, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class. Currently \code{terms} can only be a dataframe with either 1 column.
#'   \code{var_relabel} is used as the character string used as a label in the column header
#'   for each term
#'
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' 
#' 
#' library(dplyr)
#' library(random.cdisc.data)
#' 
#' ASL <- rasl(10, seed = 1)
#' AAE <- raae(ASL, 4, seed = 2)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("USUBJID", "STUDYID"))
#'  
#' l_tbls <- tern:::lt_ae_term( 
#'   term =  with_label(ANL$AEDECOD, "MedDRA Preferred Term"),
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' 
#' fast_stack_rtables(l_tbls)
#' 
#' \dontrun{
#' #throws an error because terms is not a dataframe 
#' l_tbls <- lt_ae_term( 
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' 
#' }
#' 
lt_ae_term <- function(term, 
                       id, 
                       col_by, 
                       col_N = tapply(col_by, col_by, length),
                       total = "All Patients"){
  
  # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep 
  df <- data.frame(
    term = term,
    id = id,
    col_by = col_by,
    stringsAsFactors = FALSE
  )
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [term] is currently not supported")
  
  # adding All Patients
  if (!is.null(total)) {
    if (total %in% col_by) 
      stop('The total label ,"', total, '" is not allowed as it already exists as a level in col_by')
    
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N))
  }
  
  term_label <- if(is.null(label(term))) deparse(substitute(term)) else label(term)
  tbl_header <- rheader(
    rrowl("", levels(df$col_by)),
    rrowl(term_label, col_N, format = "(N=xx)")
  )
  
  
  tbl_start <- rtable(
    header = tbl_header,
    rrowl("Total number of events", tapply(df$col_by, df$col_by, length))
  )
  
  tbl_tot_unique <- t_count_unique(
    x = df$id,
    col_by = df$col_by,
    col_N = col_N,
    row.name = "Total number of patients with at least one adverse event"
  )
  
  tbl_terms <- if (!is.null(df$term)) {
    tbls <- lapply(split(df, factor(df$term, levels = unique(df$term))), function(x) {
      t_count_unique(
        x = x$id,
        col_by = x$col_by,
        col_N = col_N,
        row.name = x$term[1]
      )
    })
    do.call(fast_rbind, tbls)
  } else {
    NULL
  }
  
  fast_rbind(tbl_start, tbl_tot_unique, tbl_terms)
  
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


#' Stack Tables Stored in a nested list of depth 2
#' 
#' \code{recursive_stack_rtables} expects a list with lists of rtables to be stacked. Sometimes
#' these tables have repeated information at the top and hence the first n rows
#' can be optionally removed from the tables that are not first in the lists.
#' 
#' @param x list with lists of rtables
#' 
#' @return rtable
#' 
#' @template author_waddella
#' 
#' @export
#' 
#' @examples
#' 
#' l_tbls <- list(
#'   list(
#'      rtabulate(iris$Sepal.Length, iris$Species, mean),
#'      rtabulate(iris$Sepal.Length, iris$Species, sd)
#'   ),
#'   list(
#'      rtabulate(iris$Sepal.Width, iris$Species, mean),
#'      rtabulate(iris$Sepal.Width, iris$Species, sd)
#'   ),
#'   list(
#'      rtabulate(iris$Petal.Length, iris$Species, mean),
#'      rtabulate(iris$Petal.Length, iris$Species, sd)
#'   )   
#' )
#' 
#' recursive_stack_rtables(l_tbls)
#' 
recursive_stack_rtables <- function(x) {
  
  tbls <- unlist_rtables(x)
  
  if (!all(vapply(tbls, is, logical(1), "rtable"))) stop("not all elements are rtables")
  
  do.call(fast_stack_rtables, tbls)
  
}
