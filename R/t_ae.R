
#' Adverse Events by System Organ Class and Preferred Term
#'
#' \code{t_ae} returns adverse events summary table that corresponds to STREAM template AET02
#' 
#' @inheritParams lt_ae_class_term
#' 
#' @details this is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet02)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet02.html})
#' @details this is an equivalent of the STREAM 1.17 output \code{\%stream_t_events_basic(templates = aet02)}
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
#' # Simple example
#' library(tibble)
#' library(dplyr)
#' 
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = factor(paste("ARM", LETTERS[rep(c(1,2), c(3,7))]))
#' )
#' 
#' 
#' ae_lookup <- tribble(
#'   ~CLASS,         ~TERM,   ~GRADE,
#'   "cl A",   "trm A_1/2",        1,
#'   "cl A",   "trm A_2/2",        2,  
#'   "cl B",   "trm B_1/3",        2,
#'   "cl B",   "trm B_2/3",        3,
#'   "cl B",   "trm B_3/3",        1,
#'   "cl C",   "trm C_1/1",        1
#' )
#' 
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
#'   ),
#'   ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
#' )
#' 
#' ANL <- left_join(AAE, ASL, by = "USUBJID")
#' 
#' tbl <- t_ae(
#'   class = ANL$CLASS,
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' 
#' tbl
#' 
#' # Example using dummy data
#' library(random.cdisc.data)
#' library(dplyr)
#' 
#' ASL <- radam("ASL", N = 10)
#' AAE <- radam("AAE", ADSL = ASL)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("STUDYID", "USUBJID"))
#' 
#' tbl <- with(ANL,
#'             t_ae(
#'               class = AEBODSYS,
#'               term =  AEDECOD,
#'               id = USUBJID,
#'               col_by = factor(ARM),
#'               total = "All Patients"
#'             )
#' )
#' 
#' tbl
#' 
t_ae <- function(class, term, id, col_by, col_N, total = "All Patients") {
  
  
  if (missing(col_N)) col_N <- tapply(col_by, col_by, length)
  
  l_tbls <- lt_ae_class_term(
    class = class,
    term = term,
    id = id,
    col_by = col_by,
    col_N = col_N,
    total = total,
    class_label = 'MedDRA System Organ Class',
    term_label = 'MedDRA Preferred Term'
  )
  
  n_cols <- ncol(l_tbls[[1]])
  if(!is.null(total))
    n_cols <- n_cols-1
  
  l_s_terms <- lapply(l_tbls, function(tbls) {
    # sort terms by any term (sum of col_by levels)
    N_total_any <- vapply(tbls[2:nrow(tbls)], function(tbl) {
      a <- 0
      for(i in c(1:n_cols)){
        if(!is.na(tbl[[i]][1])){
          a <- a + tbl[[i]][1]
        }
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
#' This function is used for deriving adverse events tables, these are returned
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
t_max_grade_per_id <- function(grade, id, col_by, col_N = NULL,
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
#' Count the number of unique elements per cell.
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
  
  counts <- tapply(x, col_by, function(x) length(unique(x)))
  percentage <- counts/col_N
  
  rtable(
    rheader(rrowl("",levels(col_by)), rrowl("", col_N, format = "(N=xx)")),
    rrowl(row.name, Map(c, counts, percentage), format= "xx.xx (xx.xx%)", indent = indent)
  )
  
}


# Create Nested Lists of Tables that Compose AE tables ----

#' List of Adverse Events Terms Tables by Highest Grade 
#' 
#' \code{lt_ae_max_grade_class_term} returns a nested list of adverse events tables by max
#' grade (\code{\link{t_max_grade_per_id}}).
#' 
#' @param class class information as character or factor vector
#' @param term term information as character or factor vector, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class
#' @param id unique subject identifier. If a particular subject has no adverse
#'   event then that information needs to be added to the \code{col_N} argument.
#' @param grade grade of adverse event as numeric
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
#' @param class_label character string that will be used as a label in the column 
#'   heade for the class
#' @param term_label character string that will be used as a label in the column 
#'   heade for the term
#' @param grade_label character string that will be used as a label in the column 
#'   heade for the grade
#'   
#' @return rtable
#' 
#' @export
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' # Simple example
#' library(tibble)
#' library(dplyr)
#' 
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = paste("ARM", LETTERS[rep(c(1,2), c(3,7))])
#' )
#' 
#' ae_lookup <- tribble(
#'   ~CLASS,         ~TERM,   ~GRADE,
#'   "cl A",   "trm A_1/2",        1,
#'   "cl A",   "trm A_2/2",        2,  
#'   "cl B",   "trm B_1/3",        2,
#'   "cl B",   "trm B_2/3",        3,
#'   "cl B",   "trm B_3/3",        1,
#'   "cl C",   "trm C_1/1",        1
#' )
#' 
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
#'   ),
#'   ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
#' )
#' 
#' ANL <- left_join(AAE, ASL[, c("USUBJID", "ARM")], by ="USUBJID")
#' 
#' l_tbls <- lt_ae_max_grade_class_term(
#'   class = ANL$CLASS,
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = factor(ANL$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   grade_levels = 1:5,
#'   class_label = 'MedDRA System Organ Class',
#'   term_label = 'MedDRA Preferred Term'
#' )
#' 
#' do.call(tern:::fast_stack_rtables,
#'   tern:::unlist_rtables(
#'     tern:::nl_remove_n_first_rrows(l_tbls)
#'   )
#' )
#'
#' \dontrun{
#' ASL <- osprey::rADSL
#' AAE <- osprey::rADAE
#'   
#' head(AAE)
#'   
#' l_tbls <- lt_ae_max_grade_class_term(
#'   class = AAE$AESOC,
#'   term =  AAE$AEDECOD,
#'   id = AAE$USUBJID,
#'   grade = as.numeric(AAE$AETOXGR),
#'   col_by = factor(AAE$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   grade_levels = 1:5,
#'   class_label = 'MedDRA System Organ Class',
#'   term_label = 'MedDRA Preferred Term'
#' )
#' 
#' tbls2 <- unlist(l_tbls, recursive = FALSE)
#' 
#' which(sapply(tbls2, class) != "rtable")
#' tbl_out <- do.call(tern:::fast_rbind, Filter(function(x) is(x, "rtable"), tbls2))
#' 
#' Viewer(tbl_out)
#' print(tbl_out) 
#' 
#' 
#' tbl <- stack_rtables_d2=(l_tbls)
#' 
#' Viewer(tbl  )
#'   
#' }
lt_ae_max_grade_class_term <- function(
  class, 
  term, 
  id, 
  grade, 
  col_by, 
  col_N = tapply(col_by, col_by, length),
  total = "All Patients",
  grade_levels,
  class_label,
  term_label,
  grade_label) {
  
  
  if (missing(class_label)) class_label <- deparse(substitute(class))
  if (missing(term_label)) term_label <- deparse(substitute(term))
  if (missing(grade_label)) grade_label <- deparse(substitute(grade))
  
  
  # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("- Overall -" %in% term)) stop("'- Overall -' is not a valid term, t_ae_ctc reserves it for derivation")
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(class == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep
  df <- data.frame(
    class = class,
    term = term,
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
#' @inheritParams lt_ae_max_grade_class_term
#' 
#' @export
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' # Simple example
#' library(tibble)
#' library(dplyr)
#' 
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = factor(paste("ARM", LETTERS[rep(c(1,2), c(3,7))]))
#' )
#' 
#' ae_lookup <- tribble(
#'   ~CLASS,         ~TERM,   ~GRADE,
#'   "cl A",   "trm A_1/2",        1,
#'   "cl A",   "trm A_2/2",        2,  
#'   "cl B",   "trm B_1/3",        2,
#'   "cl B",   "trm B_2/3",        3,
#'   "cl B",   "trm B_3/3",        1,
#'   "cl C",   "trm C_1/1",        1
#' )
#' 
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
#'   ),
#'   ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
#' )
#' 
#' ANL <- left_join(AAE, ASL[, c("USUBJID", "ARM")], by ="USUBJID")
#' 
#' l_tbls <- lt_ae_max_grade_term(
#'   term = ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   grade_levels = 1:5,
#'   term_label = 'MedDRA Preferred Term',
#' )
#' 
#' do.call(fast_stack_rtables, l_tbls)
#' 
#' 
#' 
lt_ae_max_grade_term <- function(term, 
                                 id, 
                                 grade, 
                                 col_by, 
                                 col_N = tapply(col_by, col_by, length),
                                 total = "All Patients",
                                 grade_levels,
                                 term_label,
                                 grade_label) {
  
  
  if (missing(term_label)) term_label <- deparse(substitute(term))
  if (missing(grade_label)) grade_label <- deparse(substitute(grade))
  
  # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  
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
#' @param class class information as character or factor vector
#' @param term term information as character or factor vector, however factor
#'   levels are not repeated by class, only terms with count > 1 are listed per
#'   class
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
#' @param class_label character string that will be used as a label in the column 
#'   heade for the class
#' @param term_label character string that will be used as a label in the column 
#'   heade for the term
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
#' # Simple example
#' library(tibble)
#' library(dplyr)
#' 
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = factor(paste("ARM", LETTERS[rep(c(1,2), c(3,7))]))
#' )
#' 
#' 
#' ae_lookup <- tribble(
#'   ~CLASS,         ~TERM,   ~GRADE,
#'   "cl A",   "trm A_1/2",        1,
#'   "cl A",   "trm A_2/2",        2,  
#'   "cl B",   "trm B_1/3",        2,
#'   "cl B",   "trm B_2/3",        3,
#'   "cl B",   "trm B_3/3",        1,
#'   "cl C",   "trm C_1/1",        1
#' )
#' 
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
#'   ),
#'   ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
#' )
#' 
#' ANL <- left_join(AAE, ASL[, c("USUBJID", "ARM")], by ="USUBJID") 
#' 
#' l_tbls <- lt_ae_class_term(
#'   class = ANL$CLASS,
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' recursive_stack_rtables(l_tbls)
#' 
lt_ae_class_term <- function(class, term, id,  col_by, 
                             col_N = tapply(col_by, col_by, length),
                             total = "All Patients",
                             class_label,
                             term_label){
  
  if (missing(class_label)) class_label <- deparse(substitute(class))
  if (missing(term_label)) term_label <- deparse(substitute(term))
  
  # check argument validity and consitency
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae  derives All Patients column")
  
  if (any(class == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep
  df <- data.frame(
    class = class,
    term = term,
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
#'
#' @export
#' 
#' @template author_waddella
#' @template author_zhanc107
#' @template author_wangh107
#' @template author_qit3
#' 
#' @examples 
#' library(tibble)
#' library(dplyr)
#' 
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = factor(paste("ARM", LETTERS[rep(c(1,2), c(3,7))]))
#' )
#' 
#' 
#' ae_lookup <- tribble(
#'   ~CLASS,         ~TERM,   ~GRADE,
#'   "cl A",   "trm A_1/2",        1,
#'   "cl A",   "trm A_2/2",        2,  
#'   "cl B",   "trm B_1/3",        2,
#'   "cl B",   "trm B_2/3",        3,
#'   "cl B",   "trm B_3/3",        1,
#'   "cl C",   "trm C_1/1",        1
#' )
#' 
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
#'   ),
#'   ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
#' )
#' 
#' ANL <- left_join(AAE, ASL[, c("USUBJID", "ARM")], by ="USUBJID") 
#' 
#' 
#' l_tbls <- lt_ae_term( 
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   term_label = "MedDRA Preferred Term"
#' )
#' 
#' fast_stack_rtables(l_tbls)
#' 
lt_ae_term <- function(term, 
                       id, 
                       col_by, 
                       col_N = tapply(col_by, col_by, length),
                       total = "All Patients", 
                       term_label){
  
  if (missing(term_label)) term_label <- deparse(substitute(term))

    # check argument validity and consitency 
  check_col_by(col_by, min_num_levels = 1)
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae  derives All Patients column")
  
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
    if (total %in% levels(col_by)) 
      stop(paste('col_by can not have', total, 'group. t_ae will derive it.'))
    
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N))
  }
  
  
  tbl_header <- rheader(
    rrowl("", levels(df$col_by)),
    rrowl(term_label, col_N, format = "(N=xx)", indent = 1)
  )
  
  
  tbl_start <- rtable(
    header = tbl_header,
    rrowl("Total number of events", tapply(df$col_by, df$col_by, length) , indent = 1)
  )

  tbl_tot_unique <- t_count_unique(
    x = df$id,
    col_by = df$col_by,
    col_N = col_N,
    row.name = "Total number of patients with at least one adverse event",
    indent = 1
  )
  
  tbl_terms <- if (!is.null(df$term)) {
    tbls <- lapply(split(df, factor(df$term, levels = unique(df$term))), function(x) {
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
  
}


# Helper Functions Used to Convert the Nested Lists to Single AE tables ----


#' remove firs n rows in a list of lists of rtables
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
#' This function expects a list with lists of rtables to be stacked. Sometimes
#' these tables have repeated information at the top and hence the firs n rows
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


