

#' List of Adverse Events Terms Tables by Highest Grade 
#' 
#' \code{lt_ae_grade} returns a nested list of adverse events tables by max
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
#'  pooled total population, default is "All Patients".
#' @param grade_levels numeric, ordered values of possible of grades in a form
#'   of \code{x:y}, default is \code{1:5}.
#'   
#' @return rtable
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
#' l_tbls <- lt_ae_max_grade_per_id_per_class_term(
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
#' l_tbls <- lt_ae_max_grade_per_id_per_class_term(
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
lt_ae_max_grade_per_id_per_class_term <- function(
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
  
  
  # check argument validity and consitency ----------------------------------
  check_col_by(col_by, min_num_levels = 1)
  if (total %in% levels(col_by)) 
    stop(paste('col_by can not have', total, 'group. t_ae_cts will derive it.'))
  
  if (any("- Overall -" %in% term)) stop("'- Overall -' is not a valid term, t_ae_ctc reserves it for derivation")
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc derives All Patients column")
  
  if (any(class == "", na.rm = TRUE)) stop("empty string is not a valid class, please use NA if data is missing")
  if (any(term == "", na.rm = TRUE)) stop("empty string is not a valid term, please use NA if data is missing")
  
  # data prep ---------------------------------------------------------------
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
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N))
  } 
  
  # start tabulatings ---------------------------------------------------------------
  
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
  l_t_class_terms <- Map(function(df_terms, class_name) {
    l_t_terms <- Map(function(df_term, term_name) {
      
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
  
  l_t_class_terms
  
}


#' List of Adverse Events Terms Tables 
#' 
#' Returns a nested list of adverse events tables by max grade
#' (\code{\link{t_max_grade_per_id}}).
#' 
#' @inheritParams lt_ae_max_grade_per_id_per_class_term
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
#' l_tbls <- lt_ae_max_grade_per_id_per_term(
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
lt_ae_max_grade_per_id_per_term <- function(term, 
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
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    col_N <- c(col_N, sum(col_N)) 
  }
  
  # start tabulating --------------------------------------------------------
  
  df_terms <- c(
    list("- Overall -" = df),
    split(df, df$term)      
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
    
    attr(attr(tbl, "header")[[1]], "row.name") <- term_label
    attr(attr(tbl, "header")[[2]], "indent") <- 1
    
    tbl
    
  }, df_terms, names(df_terms))
  
  l_t_terms
}



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

#' map siblings of child
#' 
#' @noRd
#' 
#' @examples 
#' 
#' 
map_rhs_siblings_of_child <- function(tree, child_depth, child_nr, FUN, ...) {
 
  if (child_depth < 1) stop("child_depth must be >= 1")
  
  depth <- 0
  
  map_in_tree <- function(x) {
    depth <<- depth + 1
    if (depth == child_depth) {
       c(x[1:child_nr], lapply(x[seq(child_nr, length(x))], FUN, ...))
    } else {
      map_in_tree(x)
    }
  }
  
  map_in_tree(x)
}


#' Stack Tables Stored in a nested list of depth 2
#' 
#' This function expects a list with lists of rtables to be stacked. Sometimes
#' these tables have repeated information at the top and hence the firs n rows
#' can be optionally removed from the tables that are not first in the lists.
#' 
#' @param x list with lists of rtables
#' @param n number of rows to remove from tables that that are not first in the
#'   nested lists
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

