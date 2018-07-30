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
#' ANL <- left_join(AAE, ASL[, c("USUBJID", "ARM")], by ="USUBJID")
#' 
#' tbl <- t_ae(
#'   class = ANL$CLASS,
#'   term =  ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = factor(ANL$ARM),
#'   col_N = tapply(ASL$ARM, ASL$ARM, length),
#'   total = "All Patients"
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
#' 
#' 
#' 
#' 
#' 
#' 
#' ##
#' 
#' 
#' t_ae(class ~ col_by | id, data = df)
#' 
#' t_ae(class )
#' 
#' 
#' 
#' t_ae(formula = class * term ~ col_by, data = ANL, with_any = TRUE)
#' 
#' t_ae_ctc(formula = class * term * grade ~ col_by | usubjid, data = ANL)
#'
#' split(data, interaction(class, term, grade))
#' y ~ x
#' 
#' 
#' t_ae_overall()
#' 
#' 
t_ae <- function(class, term, id, grade, col_by, 
                 col_N = tapply(col_by, col_by, length),
                 total = "All Patients",
                 class_label,
                 term_label,
                 grade_label
                 ) {
  
  
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
  df <- duplicate_with_var(df, subjid = paste(df$subjid, "-", total), col_by = total)
  
  
  # start tabulating --------------------------------------------------------

  
  # class and term chunks
  l_t_class_terms <- lapply(split(df, df$class), function(df_s_cl) {
    
    df_s_cl_term <- c(
      list("- Overall -" = df_s_cl),
      split(df_s_cl, df_s_cl$term)      
    )
    
    l_t_terms <- Map(function(df_i, term_i) {
      
      tbl_raw <- t_max_grade_per_id(
        grade = df_i$grade,
        id = df_i$id,
        col_by = df_i$col_by,
        col_N = col_N,
        grade_levels = grade_levels,
        any_grade = "- Any Grade -"
      )
      
      tbl <- row_names_as_col(tbl_raw)
      
      row.names(tbl)[1] <- term_i

      attr(tbl, "header")[[2]][[1]] <- rcell(grade_label)
      attr(tbl, "header")[[1]][[1]] <- rcell(NULL)
      tbl
      
    }, df_s_cl_term, names(df_s_cl_term))
    
  })
  
  
  ## sorting

  
  # sort terms by any grade and total
  l_t_class_sterms <- lapply(l_t_class_terms, function(l_t_terms) {
    ## TODO: if no TOTAL sum over row
    N_total_any <- vapply(l_t_terms, function(tbl) {
      tbl[1, ncol(tbl)][1]
    }, numeric(1))
    l_t_terms[c(1, setdiff(order(-N_total_any, names(l_t_terms), decreasing = FALSE), 1))]
  })
  
  
  # now sort class
  N_total_overall <- vapply(l_t_class_sterms, function(l_t_terms) {
    tbl_any <- l_t_terms[[1]]
    tbl_any[1, ncol(tbl_any)][1]
  }, numeric(1))
  l_t_sclass_sterm <- l_t_class_sterms[order(-N_total_overall, names(l_t_class_terms), decreasing = FALSE)]
  
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
  
  tbls_all <- c(
    list("- Any adverse events -" = list("- Overall -" = tbl_overall)),
    l_t_sclass_sterm
  )
  
  
  
  ## Either return list of tables or stacked tables
  
  if (TRUE) {
    tbls <- Map(function(l_tbls_class, class_name) {
      tbl_class <- do.call(fast_rbind, l_tbls_class)
      tbl <- fast_rbind(
        rtable(header(tbl_class), rrow(class_name)),
        indent_table(tbl_class, 1)
      )
      attr(attr(tbl, "header")[[1]], "row.name") <- 'MedDRA System Organ Class'
      attr(attr(tbl, "header")[[2]], "row.name") <- 'MedDRA Preferred Term'
      attr(attr(tbl, "header")[[2]], "indent") <- 1
      tbl
    }, tbls_all, names(tbls_all))
    
    do.call(fast_rbind, tbls)
    
  } else {
    tbls_all
  }
  
}