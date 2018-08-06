
#' Adverse Events Table by Highest NCI CTCAE Grade
#' 
#' \code{t_ae_ctc} returns adverse events sorted by highest NCI (National Cancer
#'  Institute) CTCAE (common terminology criteria for adverse avents) grade.
#' 
#' @inheritParams lt_ae_max_grade_class_term
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
#' This is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet04)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet04.html})
#' @details this is an equivalent of the STREAM 1.17 output \code{\%stream_t_events_bygrade(templates = aet04)}
#'   (\url{<https://rochewiki.roche.com/confluence/pages/viewpage.action?pageId=294027501>})
#'  
#' @return \code{rtable} object
#' 
#' @export
#' 
#' @template author_manukyae
#' @template author_waddella
#' @template author_zhanc107
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
#' tbl <- t_ae_ctc(
#'   class = ANL$CLASS,
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = ANL$ARM,
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients",
#'   grade_levels = 1:3
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
t_ae_ctc <- function(class, term, id, grade, col_by, col_N, total = "All Patients", grade_levels = 1:5) {
  
  
  if (missing(col_N)) col_N <- tapply(col_by, col_by, length)
  
  l_tbls <- lt_ae_max_grade_class_term(
    class = class,
    term = term,
    id = id,
    grade = grade,
    col_by = col_by,
    col_N = col_N,
    total = total,
    grade_levels = grade_levels,
    class_label = 'MedDRA System Organ Class',
    term_label = 'MedDRA Preferred Term',
    grade_label = "Grade"
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
}
