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
  
  
  
  
  # sort terms by any grade and total
  N_total_any <- vapply(l_t_terms, function(tbl) {
    tbl[1, n_cols + 1][1]
  }, numeric(1))
  
  l_t_terms[c(1, setdiff(order(-N_total_any, names(l_t_terms), decreasing = FALSE), 1))]
  
  # now sort tables by class total
  N_total_overall <- vapply(l_t_class_terms, function(tbl) {
    tbl[[1]][1, n_cols + 1][1]
  }, numeric(1))
  
  l_t_class_terms_sorted <- l_t_class_terms[order(-N_total_overall, names(l_t_class_terms), decreasing = FALSE)]
  
  
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
#' @importFrom stats aggregate
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
  
  ## add (N=xx) row to header
  header(tbl) <- rheader(
    rrowl("", levels(col_by)),
    rrowl("", unname(col_N), format = "(N=xx)")
  )
  
  tbl
}


