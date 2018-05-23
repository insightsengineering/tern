#' Adverse Events Table by Highest NCI CTCAE Grade
#' 
#' \code{t_ae_ctc} returns adverse events sorted by highest NCI (National Cancer
#'  Institute) CTCAE (common terminology criteria for adverse avents) grade.
#' 
#' @param class system organ class variable.
#' @param term preferred term variable.
#' @param id unique subject identifier variable. If a particular subject has no
#'   adverse event then the subject \code{id} should be listed where
#'   \code{class} and \code{term} should be set to missing (i.e. \code{NA}).
#' @param grade grade of adverse event variable.
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param total character string that will be used as a label for a column with 
#'  pooled total population, default is "All Patients".
#' @param grade_levels ordered values of possible of grades in a form of
#'   \code{x:y}, default is \code{1:5}. This assures a proper fill in for
#'   grades, see 'Details'.
#' 
#' @details 
#' \code{t_ae_ctc} counts patients according to adverse events (AEs) of greatest
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
#' \code{t_ae_ctc} removes any non-complete records, e.g. if class or term are 
#'  missing. If the intent is to preserve such records, then impute missing 
#'  values before using \code{t_ae_ctc}.
#'      
#' \code{t_ae_ctc} orders data by "All Patients" column from the most commonly
#'  reported SOC to the least frequent one. Within SOC, it sorts by decreasing
#'  frequency of PT. It brakes ties using SOC/PT names in alphabetical order.   
#' 
#' \code{t_ae_ctc} fills in \code{col_by} and \code{grade} with \code{0} value 
#' in case there was no AEs reported for particular \code{col_by} and/or 
#' \code{grade} category. Use \code{grade_levels} to modify the range of existing
#' grades. If data does not have any records with \code{grade} 5 and the intent 
#' is to show only grades 1-4 rows then use \code{grade_levels = 1:4}.
#'  
#' @export
#' 
#' @template author_manukyae
#' @template author_waddella
#'  
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
#' ~CLASS,         ~TERM,   ~GRADE,
#' "cl A",   "trm A_1/2",        1,
#' "cl A",   "trm A_2/2",        2,  
#' "cl B",   "trm B_1/3",        2,
#' "cl B",   "trm B_2/3",        3,
#' "cl B",   "trm B_3/3",        1,
#' "cl C",   "trm C_1/1",        1
#' )
#' 
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
#'   ),
#'   ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
#' )
#' 
#' ANL <- left_join(ASL, AAE, by = "USUBJID")
#' 
#' tbl <- t_ae_ctc(
#'   class = ANL$CLASS,
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:3
#' )
#' 
#' tbl
#' 
#' 
#' library(random.cdisc.data)
#' library(dplyr)
#' 
#' ASL <- radam("ASL", N = 10)
#' AAE <- radam("AAE", ADSL = ASL)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("STUDYID", "USUBJID"))
#' 
#' tbl <- with(ANL,
#'             t_ae_ctc(
#'               class = AEBODSYS,
#'               term =  AEDECOD,
#'               id = USUBJID,
#'               grade = AETOXGR,
#'               col_by = factor(ARM),
#'               total = "All Patients",
#'               grade_levels = 1:5
#'             )
#' )
#' 
#' tbl
#' 
t_ae_ctc <- function(class, term, id, grade, col_by, total = "All Patients", grade_levels = 1:5) {
  
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
                   subjid = id,
                   gradev = grade,
                   col_by = col_by,
                   stringsAsFactors = FALSE)
  
  # adding All Patients
  df <- duplicate_with_var(df, subjid = paste(df$subjid, "-", total), col_by = total)
  
  # total N for column header
  N <- tapply(df$subjid, df$col_by, function(x) (sum(!duplicated(x))))
  
  # need to remove extra records that came from subject level data
  # when left join was done. also any record that is missing class or term
  df <- na.omit(df)
  
  # start tabulating --------------------------------------------------------
  n_cols <- nlevels(col_by)
  
  # class and term chunks
  l_t_class_terms <- lapply(split(df, df$class), function(df_s_cl) {
    
    df_s_cl_term <- c(
      list("- Overall -" = df_s_cl),
      split(df_s_cl, df_s_cl$term)      
    )
    
    l_t_terms <- lapply(df_s_cl_term, function(df_i) {
      
      t_max_grade_per_id(
        grade = df_i$gradev,
        id = df_i$subjid,
        col_by = df_i$col_by,
        col_N = N,
        grade_levels = grade_levels,
        any_grade = "- Any Grade -"
      )
      
    })
    
    # sort terms by any grade and total
    N_total_any <- vapply(l_t_terms, function(tbl) {
      tbl[1, n_cols + 1][1]
    }, numeric(1))
    
    l_t_terms[c(1, setdiff(order(-N_total_any, names(l_t_terms), decreasing = FALSE), 1))]
  })
  
  # now sort tables
  N_total_overall <- vapply(l_t_class_terms, function(tbl) {
    tbl[[1]][1, n_cols + 1][1]
  }, numeric(1))
  
  l_t_class_terms_sorted <- l_t_class_terms[order(-N_total_overall, names(l_t_class_terms), decreasing = FALSE)]
  
  tbl_overall <- t_max_grade_per_id(
    grade = df$gradev,
    id = df$subjid,
    col_by = df$col_by,
    col_N = N,
    grade_levels = grade_levels,
    any_grade = "- Any Grade -"
  )
  
  tbls_all <- c(
    list("- Any adverse events -" = list("- Overall -" = tbl_overall)),
    l_t_class_terms_sorted
  )
  
  tbls_class <- Map(function(tbls_i, class_i) {
    lt1 <- Map(shift_label_table, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables, lt1)
    add_ae_class(indent_table(t2, 1), class_i)
  }, tbls_all, names(tbls_all))
  
  
  tbl <- do.call(stack_rtables, tbls_class)
  
  attr(attr(tbl, "header")[[1]], "row.name") <- 'MedDRA System Organ Class'
  attr(attr(tbl, "header")[[2]], "row.name") <- 'MedDRA Preferred Term'
  attr(attr(tbl, "header")[[2]], "indent") <- 1
  
  attr(tbl, "header")[[2]][[1]] <- rcell('NCI CTCAE Grade')
  attr(tbl, "header")[[1]][[1]] <- rcell(NULL)
  
  tbl
}



add_ae_class <- function(tbl, class) {
  rbind(
    rtable(header(tbl), rrow(class)),
    tbl
  )
}



#' Tabulate maximum grade per id by \code{col_by}
#' 
#' This function is used for deriving adverse events tables.
#'  
#' @param grade a numeric vector with grade values
#' @param id an vector with id values
#' @param col_by a factor with values used for column names
#' @param col_N a vector with total n for each level of \code{col_by}
#' @param grade_levels a numeric vector used for naming rows for each level of
#'   grade
#' @param any_grade add a row that counts any occurrence, it is named \code{-Any
#'   Grade-} by default
#' 
#' @export
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
  
  header(tbl) <- rheader(
    rrowl("", levels(col_by)),
    rrowl("", unname(col_N), format = "(N=xx)")
  )
  
  tbl
}


