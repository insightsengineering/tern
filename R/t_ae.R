

#' list of tables
#' 
#' 
#' @export
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
#' l_tbls <- lt_ae_max_grade_d2(
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
#' stack_rtables_d2(l_tbls)
#'
#' 'MedDRA System Organ Class'
#'  'MedDRA Preferred Term'

lt_ae_max_grade_d2 <- function(class, 
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
  
  
  # check argument validity and consitency ----------------------------------
  check_col_by(col_by, min_num_levels = 1)
  if (total %in% levels(col_by)) 
    stop(paste('col_by can not have', total, 'group. t_ae_cts will derive it.'))
  
  if (any("- Overall -" %in% term)) stop("'- Overall -' is not a valid term, t_ae_ctc reserves it for derivation")
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(class == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep ---------------------------------------------------------------
  df <- data.frame(class = class,
                   term = term,
                   id = id,
                   grade = grade,
                   col_by = col_by,
                   stringsAsFactors = FALSE)
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, term, grade] is currently not supported")
  
  # adding All Patients
  if(!is.null(total)){
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N)) 
  }
  
  # start tabulating --------------------------------------------------------
  
  
  # class and term chunks
  
  df_by_class <- split(df, df$class)

  
  l_t_class_terms <- Map(function(df_cl, class_i) {
    
    df_by_term <- c(
      list("- Overall -" = df_cl),
      split(df_cl, df_cl$term)      
    )
    
    l_t_terms <- Map(function(df_term, term_i) {
      
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
      
      row.names(tbl)[1] <- term_i
      
      attr(tbl, "header")[[2]][[1]] <- rcell(grade_label)
      attr(tbl, "header")[[1]][[1]] <- rcell(NULL)
      
      ## add class term
      tbl <- fast_rbind(
        rtable(header(tbl), rrow(class_i)),
        indent_table(tbl, 1)
      )
      attr(attr(tbl, "header")[[1]], "row.name") <- class_label
      attr(attr(tbl, "header")[[2]], "row.name") <- term_label
      attr(attr(tbl, "header")[[2]], "indent") <- 1
      
      tbl
      
    }, df_by_term, names(df_by_term))
    
    
  }, df_by_class, names(df_by_class) )
  
  ###### Add any adverse event
  tbl_overall <- t_max_grade_per_id(
    grade = df$grade,
    id = df$id,
    col_by = df$col_by,
    col_N = col_N,
    grade_levels = grade_levels,
    any_grade = "- Any Grade -"
  )
  tbl_overall <- row_names_as_col(tbl_overall)
  attr(tbl_overall, "header")[[2]][[1]] <- rcell(grade_label)
  attr(tbl_overall, "header")[[1]][[1]] <- rcell(NULL)
  
  attr(attr(tbl_overall, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl_overall, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl_overall, "header")[[2]], "indent") <- 1
  row.names(tbl_overall)[1] <- "- Overall -"
  tbl_overall <-  fast_rbind(
    rtable(header(tbl_overall), rrow("- Any adverse events -")), 
    indent_table(tbl_overall, 1)
  )
  
  c(
    list("- Any adverse events -" = tbl_overall),
    l_t_class_terms
  )
  
}


#' list of tables (d1)
#' 
#' Creates a list of tables with a depth of 1.
#' 
#' 
#' @export
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
#' l_tbls <- lt_ae_max_grade_d1(
#'   term = ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = factor(ANL$ARM),
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
lt_ae_max_grade_d1 <- function(term, 
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
  
  # check argument validity and consitency ----------------------------------
  check_col_by(col_by, min_num_levels = 1)
  if (total %in% levels(col_by)) 
    stop(paste('col_by can not have', total, 'group. t_ae_cts will derive it.'))
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  
  # data prep ---------------------------------------------------------------
  df <- data.frame(term = term,
                   id = id,
                   grade = grade,
                   col_by = col_by,
                   stringsAsFactors = FALSE)
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, grade] is currently not supported")
  
  # adding All Patients
  if(!is.null(total)){
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N)) 
  }
  
  # start tabulating --------------------------------------------------------
  # term chunk
    df_by_term <- split(df, df$term)

    
    l_t_term <- Map(function(df_term, term_i) {
      
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
      
      row.names(tbl)[1] <- term_i
      
      attr(tbl, "header")[[2]][[1]] <- rcell(grade_label)
      attr(tbl, "header")[[1]][[1]] <- rcell(NULL)
      
      attr(attr(tbl, "header")[[1]], "row.name") <- term_label
      attr(attr(tbl, "header")[[2]], "indent") <- 1
      
      tbl
      
    }, df_by_term, names(df_by_term))
    
    ###### Add any adverse event
    tbl_overall <- t_max_grade_per_id(
      grade = df$grade,
      id = df$id,
      col_by = df$col_by,
      col_N = col_N,
      grade_levels = grade_levels,
      any_grade = "- Any Grade -"
    )
    tbl_overall <- row_names_as_col(tbl_overall)
    attr(tbl_overall, "header")[[2]][[1]] <- rcell(grade_label)
    attr(tbl_overall, "header")[[1]][[1]] <- rcell(NULL)
    
    attr(attr(tbl_overall, "header")[[1]], "row.name") <- term_label
    attr(attr(tbl_overall, "header")[[2]], "indent") <- 1
    row.names(tbl_overall)[1] <- "- Overall -"
    tbl_overall <-  fast_rbind(
      rtable(header(tbl_overall), rrow("- Any adverse events -")), 
      indent_table(tbl_overall, 1)
    )
    c(
      list("- Any adverse events -" =  tbl_overall),
      l_t_term
    )
  
}


#' stack rtables in nested list depth 2 (-1 row)
#' 
#' @export
#' 
#' 
#' 
stack_rtables_d2 <- function(x) {
  nam <- names(x)
  x1 <- x[nam[!(nam %in% c("- Any adverse events -",
                           "Total number of patients with at least one adverse event",
                           "Overall total number of events") )]]
  x2 <- x[nam[nam %in% c("- Any adverse events -",
                         "Total number of patients with at least one adverse event",
                         "Overall total number of events")]]

  l_d1 <- lapply(x1, function(xi) {

    
    tbls <- Map(function(tbl, i) {
      if (i == 1) tbl else tbl[-1, ]
    }, xi, seq_along(xi))
    
    do.call(fast_stack_rtables, tbls)
  })
  
  do.call(fast_stack_rtables, c(x2, l_d1))
}

#' Count unique id
#' 
#' @examples 
#' tbl <- t_one_count_per_id(
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length)
#' )
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' t_one_count_per_id(
#'  id = paste("id", c(1, 4, 2, 3, 3), sep = "-"),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#' 
t_one_count_per_id <- function(id, col_by, col_N){
  if (any(is.na(id)) || any(is.na(col_by))) stop("no NA allowed in id and col_by")
  if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  df <- data.frame(
    id = id,
    col_by = col_by,
    stringsAsFactors = FALSE
  )
  df <- df[!duplicated(df$id), ]
  tbl_x <- rtabulate(
    na.omit(df),
    row_by_var = no_by(""),
    col_by_var = "col_by", 
    FUN = count_perc_col_N,
    N = col_N,
    format = "xx (xx.x%)"
  )
  header(tbl_x) <- rheader(
    rrowl("", levels(df$col_by)),
    rrowl("", unname(col_N), format = "(N=xx)")
  )
  tbl_x
}

 


#' Count events
#' 
#' @examples 
#' tbl <- t_event_count(
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length)
#' )
#' 
#' @export
#' 
#' @examples 
#' 
#' t_event_count(
#'  id = paste("id", c(1, 4, 2, 3, 3), sep = "-"),
#'  col_by = factor(c("A", "A", "B", "C", "C")),
#'  col_N = c(2, 4, 10)
#' )
#' 
t_event_count <- function(id, col_by, col_N){
  if (any(is.na(id)) || any(is.na(col_by))) stop("no NA allowed in id and col_by")
  if (nlevels(col_by) != length(col_N)) stop("dimension missmatch levels(col_by) and length of col_N")
  df <- data.frame(
    id = id,
    col_by = col_by,
    stringsAsFactors = FALSE
  )
  tbl_x <- rtabulate(
    na.omit(df),
    row_by_var = no_by(""),
    col_by_var = "col_by", 
    FUN = count_col_N,
    N = col_N,
    format = "xx"
  )
  header(tbl_x) <- rheader(
    rrowl("", levels(df$col_by)),
    rrowl("", unname(col_N), format = "(N=xx)")
  )
  tbl_x
}


#' By class By Term
#'
#'
#' @export
#' 
#' @examples 
#' 
#' # Simple example
#' library(tibble)
#' library(dplyr)
#' 
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = paste("ARM", LETTERS[rep(c(1,2), c(3,7))])
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
#'   col_by = factor(ANL$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
#' )
#' stack_rtables_d2(l_tbls)
#' 
lt_ae_class_term <- function(class, term, id,  col_by, 
                             col_N = tapply(col_by, col_by, length),
                             total = "All Patients",
                             class_label,
                             term_label){
  if (missing(class_label)) class_label <- deparse(substitute(class))
  if (missing(term_label)) term_label <- deparse(substitute(term))
  # check argument validity and consitency ----------------------------------
  check_col_by(col_by, min_num_levels = 1)
  if (total %in% levels(col_by)) 
    stop(paste('col_by can not have', total, 'group. t_ae will derive it.'))

  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae  derives All Patients column")
  
  if (any(class == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep ---------------------------------------------------------------
  df <- data.frame(class = class,
                   term = term,
                   id = id,
                   col_by = col_by,
                   stringsAsFactors = FALSE)
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [class, term] is currently not supported")
  
  # adding All Patients
  df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
  
  col_N <- c(col_N, sum(col_N))
  
  # class and term chunks
  
  df_by_class <- split(df, df$class)
  

  l_t_class_terms <- Map(function(df_cl, class_i) {
  tbl_cl_event <- t_event_count(
    id = df_cl$id,
    col_by = df_cl$col_by,
    col_N = col_N)
  attr(attr(tbl_cl_event, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl_cl_event, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl_cl_event, "header")[[2]], "indent") <- 1
  row.names(tbl_cl_event)[1] <- "Total number of events"
  tbl_cl_event <- fast_rbind(
    rtable(header(tbl_cl_event), rrow(class_i)),
    indent_table(tbl_cl_event, 1)
  )
  
  tbl_cl_per_id <- t_one_count_per_id(
    id = df_cl$id,
    col_by = df_cl$col_by,
    col_N = col_N
  )  
  attr(attr(tbl_cl_per_id, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl_cl_per_id, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl_cl_per_id, "header")[[2]], "indent") <- 1
  row.names(tbl_cl_per_id)[1] <- "Total number of patients with at least one adverse event"
  tbl_cl_per_id <- fast_rbind(
    rtable(header(tbl_cl_per_id), rrow(class_i)),
    indent_table(tbl_cl_per_id, 1)
  )
    df_by_term <- split(df_cl, df_cl$term) 
    
    l_t_terms <- Map(function(df_term, term_i) {

      tbl <- t_one_count_per_id(
        id = df_term$id,
        col_by = df_term$col_by,
        col_N = col_N
      )
      row.names(tbl)[1] <- term_i
      ## add class term
      tbl <- fast_rbind(
        rtable(header(tbl), rrow(class_i)),
        indent_table(tbl, 1)
      )
      attr(attr(tbl, "header")[[1]], "row.name") <- class_label
      attr(attr(tbl, "header")[[2]], "row.name") <- term_label
      attr(attr(tbl, "header")[[2]], "indent") <- 1
      
      
      tbl
      
      
    }, df_by_term, names(df_by_term))
    
  l_t_terms <- c(list("Total number of patients with at least one adverse event" = tbl_cl_per_id,
                      "Total number of events" = tbl_cl_event),
                 l_t_terms)  
  }, df_by_class, names(df_by_class) )
  
  #### Add overall total
  tbl_event <- t_event_count(
    id = df$id,
    col_by = df$col_by,
    col_N = col_N)
  attr(attr(tbl_event, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl_event, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl_event, "header")[[2]], "indent") <- 1
  row.names(tbl_event)[1] <- "Overall total number of events"
  
  
  tbl_per_id <- t_one_count_per_id(
    id = df$id,
    col_by = df$col_by,
    col_N = col_N
  )  
  attr(attr(tbl_per_id, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl_per_id, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl_per_id, "header")[[2]], "indent") <- 1
  row.names(tbl_per_id)[1] <- "Total number of patients with at least one adverse event"
  c(list(
    "Total number of patients with at least one adverse event" = tbl_per_id,
    "Overall total number of events" = tbl_event
  ), l_t_class_terms)
}

 


#' Only By Term
#'
#' @examples 
#' 
#' 
#' l_tbls <- lt_ae_term( 
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   term_label = "MedDRA Preferred Term"
#' )
#' do.call(fast_stack_rtables, l_tbls)
#' @export

lt_ae_term <- function(term, id,  col_by, 
                             col_N = tapply(col_by, col_by, length),
                             total = "All Patients", 
                             term_label){
 
  if (missing(term_label)) term_label <- deparse(substitute(term))
  # check argument validity and consitency ----------------------------------
  check_col_by(col_by, min_num_levels = 1)
  if (total %in% levels(col_by)) 
    stop(paste('col_by can not have', total, 'group. t_ae will derive it.'))
  
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae  derives All Patients column")
  
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep ---------------------------------------------------------------
  df <- data.frame(term = term,
                   id = id,
                   col_by = col_by,
                   stringsAsFactors = FALSE)
  
  if (any(is.na(df)))
    stop("partial missing data in rows of [term] is currently not supported")
  
  # adding All Patients
  df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
  
  col_N <- c(col_N, sum(col_N))
  
  # term chunks
 
  
  df_by_term <- split(df, df$term) 
  
  l_t_terms <- Map(function(df_term, term_i) {
    
    tbl <- t_one_count_per_id(
      id = df_term$id,
      col_by = df_term$col_by,
      col_N = col_N
    )
    row.names(tbl)[1] <- term_i
    attr(attr(tbl, "header")[[2]], "row.name") <- term_label
    tbl
  }, df_by_term, names(df_by_term))
  
  #### Add overall total
  tbl_event <- t_event_count(
    id = df$id,
    col_by = df$col_by,
    col_N = col_N)
  attr(attr(tbl_event, "header")[[2]], "row.name") <- term_label
  row.names(tbl_event)[1] <- "Overall total number of events"
  
  
  tbl_per_id <- t_one_count_per_id(
    id = df$id,
    col_by = df$col_by,
    col_N = col_N
  )  
  attr(attr(tbl_per_id, "header")[[2]], "row.name") <- term_label
  row.names(tbl_per_id)[1] <- "Total number of patients with at least one adverse event"
  c(list(
    "Total number of patients with at least one adverse event" = tbl_per_id,
    "Overall total number of events" = tbl_event
  ), l_t_terms)
}

