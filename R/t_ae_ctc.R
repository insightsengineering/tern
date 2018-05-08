#' Adverse Events Table by Highest NCI CTCAE Grade
#' 
#' \code{t_ae_ctc} returns adverse events sorted by highest NCI CTCAE grade.
#' 
#' @param class system organ class variable.
#' @param term preferred term variable.
#' @param id unique subject identifier variable.
#' @param grade grade of adverse event variable.
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param total character string that will be used as a label for a column with 
#'  pooled total population, default is "All Patients".
#' @param grade_range range of grades in a form of \code{c(x, y)}, default is 
#'  \code{c(1, 5)}. This only has effect on which grades are displayed and 
#'  assures a proper fill in for grades, otherwise no sub-setting of data is 
#'  done. See 'Details'.
#' 
#' @details 
#' \code{t_ae_ctc} counts patients according to adverse events (AEs) of greatest
#'  intensity for system organ class (SOC) and overall rows and includes 
#'  percentages based on the total number of patients in the column heading 
#'  (i.e. "N=nnn"). If the inention is to use patients number from subject level
#'  dataset as N for percentage calculation then adeverse events dataset should
#'  be left joined to subject level dataset and the \code{col_by} variable should
#'  be dropped from adverse events dataset, see the example. Otherwise, N will be
#'  derived using adverse events dataset. At the preferred term (PT) level,
#'  multiple events within a patient of the same PT are counted once using the
#'  greatest intensity reported. 
#' 
#' \code{t_ae_ctc} orders data by "All Patients" column from the most commonly
#'  reported SOC to the least frequent one. Within SOC, it sorts by decreasing
#'  frequency of PT.   
#' 
#' \code{t_ae_ctc} removes any non-complete records, e.g. if class or term are 
#'  missing.
#'  
#' \code{t_ae_ctc} fills in \code{col_by} and \code{grade} with \code{0} value 
#' in case there was no AEs reported for particular \code{col_by} and/or 
#' \code{grade} category. Use \code{grade_range} to modify displayed grades, e.g. 
#'  \code{grade_range = c(3, 5)} will only display grades 3, 4, and 5. Please be
#'  aware that this is only for display purposes and does not sub-set data. One 
#'  needs to sub-set data before providing it to \code{t_ae_ctc}.
#'  
#' @export
#' 
#' @template author_manukyae
#'  
#'  
#' @examples 
#' # Simple example
#' library(tibble)
#' library(dplyr)
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = paste("ARM", LETTERS[rep(c(1,2), c(3,7))])
#' )
#' 
#' 
ae_lookup <- tribble(
~CLASS,         ~TERM,   ~GRADE,
"cl A",   "trm A_1/2",        1,
"cl A",   "trm A_2/2",        2,  
"cl B",   "trm B_1/3",        2,
"cl B",   "trm B_2/3",        3,
"cl B",   "trm B_3/3",        1,
"cl C",   "trm C_1/1",        1
)

AAE <- cbind(
  tibble(
    USUBJID = ASL$USUBJID[c(2,2,2,3,3,4,4,4,4,5,6,6,7,7)]
  ),
  ae_lookup[c(1,1,2,6,4,2,2,3,4,2,1,5,4,6),]
)

ANL <- left_join(ASL, AAE, by = "USUBJID")

tbl <- t_ae_ctc(
  class = ANL$CLASS,
  term =  ANL$TERM,
  id = ANL$USUBJID,
  grade = ANL$GRADE,
  col_by = factor(ANL$ARM),
  total = "All Patients",
  grade_range = c(1, 3)
)

tbl
#' 
#' 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' AAE <- radam("AAE", ADSL = ASL)
#' 
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("STUDYID", "USUBJID"))
#' 
#' tbl <- with(ANL,
#'             t_ae_ctc(
#'               class = ANL$AEBODSYS,
#'               term =  ANL$AEDECOD,
#'               id = ANL$USUBJID,
#'               grade = ANL$AETOXGR,
#'               col_by = factor(ANL$ARM),
#'               total = "All Patients",
#'               grade_range = c(1, 5)
#'             )
#' )
#' 
#' tbl
#' 
#' \dontrun{
#' # the following example should reproduce table t_ae_ctc_ATEZOREL_SENBX.out
#' file.show('/opt/BIOSTAT/prod/cdpt7805/s28363v/reports/t_ae_ctc_ATEZOREL_SENBX.out')
#' 
#' library(rocheBCE)
#' AAE <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat')
#' ASL <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat')
#' 
#' # filter subject-level dataset
#' asl <- ASL[ASL$SAFFL == "Y", c("USUBJID", "TUMTYPE", "TRTSDTM", "TRT02AN")]
#' asl$TUMTYPE <- ifelse(asl$TUMTYPE == '', 'OTHER', asl$TUMTYPE)
#' asl <- asl[!(asl$TRT02AN %in% c(7, 8)) & 
#' (as.Date(asl$TRTSDTM) <= as.Date('2016-10-12')), c('USUBJID', 'TUMTYPE')]
#' 
#' # filter adverse events dataset and drop the col_by variable
#' aae <- AAE[AAE$TRTEMFL == 'Y' & AAE$ANLFL == 'Y' & AAE$AEREL1 == 'Y', !names(AAE) %in% 'TUMTYPE']
#' 
#' # left join subject-level dataset with adverse events
#' aae <- merge(asl, aae, by = c('USUBJID'), all.x = TRUE)
#'
#' tbl <- t_ae_ctc(
#'  class = aae$AEBODSYS,
#'  term =  aae$AEDECOD,
#'  id = aae$USUBJID,
#'  grade = aae$AETOXGR,
#'  col_by = factor(aae$TUMTYPE),
#'  total = "All Patients",
#'  grade_range = c(1, 5)
#' )
#' 
#' Viewer(tbl)
#' }
t_ae_ctc <- function(class, term, id, grade, col_by, total = "All Patients", grade_range) {
  
  # check argument validity and consitency ----------------------------------
  if (!is.vector(grade_range) || !length(grade_range) == 2) 
    stop("grade_range needs to be a numeric vector with 2 values")
  
  if (grade_range[1] > min(grade, na.rm = TRUE) || grade_range[2] < max(grade, na.rm = TRUE))
    stop("some grade values are not in grade_range")
  
  check_col_by(col_by, min_num_levels = 1)
  if (total %in% levels(col_by)) 
    stop(paste('col_by can not have', total, 'group. t_ae_cts will derive it.'))
  
  if (any("- Overall -" %in% term)) stop("'- Overall -' is not a valid term")
  
  grade_levels <- seq(grade_range[1], grade_range[2])
  
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
  is_class_or_term_missing <- is.na(df$class) | is.na(df$term) | df$class == '' | df$term == ''
  df$class[is_class_or_term_missing] <- NA
  df$term[is_class_or_term_missing] <- NA
  
  df <- na.omit(df)
  
  # need to duplicate rows for each class to calculate overall class chunks
  #df <- duplicate_with_var(df, term = "- Overall -")
  
  # start tabulating --------------------------------------------------------
  
  # class and term chunks
  df_s <- lapply(split(df, df$class), function(dfi) split(dfi, dfi$term))
  
  # df_cl_term <- df_s[[1]][[1]]
  # term_i <- names(df_s[[1]])[1]
  l_t_terms <- lapply(df_s, function(df_s_cl) {
     Map(
      function(df_i, term_i) {
        
        # any grade row
        tbl_all <- rtabulate(
          df_i,
          row_by_var = no_by('- Any Grade -'),
          col_by_var = "col_by", 
          FUN = ae_ctc_count_perc,
          format = "xx (xx.x%)",
          indent = 1,
          N = N
        )
        
        # need the highest grade per patient and grade variable should be a vector
        df_i_max_grade <- aggregate(gradev ~ col_by + subjid, data = df_i, FUN = max,
                                    drop = TRUE, na.rm = TRUE)
        # need factor grade for rtabulate
        df_i_max_grade$grade_f <- factor(df_i_max_grade$gradev, levels = grade_levels)
        
        # by grades
        tbl_by_grade <- rtabulate(
          df_i_max_grade,
          row_by_var = "grade_f",
          col_by_var = "col_by", 
          FUN = ae_ctc_count_perc,
          format = "xx (xx.x%)",
          indent = 2,
          N = N
        )
        
        rbind(tbl_all, tbl_by_grade)
      },
      df_s_cl, names(df_s_cl)
    )
  })
  
  # now sort tables
  
  # sorting by total counts and then by terms
  order_term_class(l_t_term_raw)
  
  # sorting by total counts and then by class
  l_t_terms_classes_sorted <- order_term_class(l_t_terms, classord = TRUE)
  
  # stack term chunks together within class
  l_t_terms_classes_sorted_stacked <- lapply(l_t_terms_classes_sorted, function(x) {
    Reduce(rbind, x)
  })
  
  # need class on a separate row
  l_t_terms_classes_sorted_soc <- sapply(names(l_t_terms_classes_sorted_stacked), simplify = FALSE,
                                         function(x) {
                                           rbind(
                                             rtable(header = names(N), rrow(), rrow(x)),
                                             l_t_terms_classes_sorted_stacked[[x]]
                                           )})
  
  # stack altogether
  pre_final <- Reduce(rbind, l_t_terms_classes_sorted_soc)
  
  # Any adverse events ------------------------------------------------------
  AnyAE <- DeriveCore(df, nms = '- Any adverse events -')  
  
  # Format header by including N = ------------------------------------------
  
  final <- rbind(
    rtable(
      header = c(names(N)),
      rrow('MedDRA System Organ Class'),
      rrow('MedDRA Preferred Term', indent = 1),
      rrow('NCI CTCAE Grade', indent = indmax),
      rrow()
    ),
    AnyAE,
    pre_final
  )
  
  header(final) = c(NULL, paste0(names(N), '\n', '(N=', N, ')'))
  
  final
  
}


# checks if there is any case and derives counts (percentage), otherwise 0
ae_ctc_count_perc <- function(x_cell, N) {
  N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
  if (N_i > 0) {
    n <- sum(!duplicated(x_cell$subjid)) # number of patients with at least one ae
    n * c(1, 1 / N[x_cell$col_by[1]]) # obtaining the total and getting percentage
  } else {
    rcell(0, format = "xx")
  }
}


# need to order by overall soc/term counts
# need total from any grade all patients, the last column of the first row
#  is all patients count
# break ties by alphabetical order of pt
ae_ctc_order_term_class <- function(term_list, classord = FALSE) {
  PTVector <- vapply(names(term_list),
                     function(x) {
                       # need for sorting the class
                       if (classord) l_t_terms[[x]][[1]][1, length(N)][1]
                       # need for sorting terms 
                       else  term_list[[x]][1, length(N)][1]
                     },
                     FUN.VALUE = numeric(1))
  
  PTOrder <- PTVector[order( - PTVector, names(PTVector))]
  list_ordered <- term_list[names(PTOrder)]
  list_ordered
}

