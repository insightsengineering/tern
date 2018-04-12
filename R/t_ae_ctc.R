#' Adverse Events by Highest NCI CTCAE Grade Table 
#' 
#' \code{t_ae_ctc} returns Adverse Events by Highest NCI CTCAE Grade Table.
#' 
#' @author Edgar Manukyan (manukyae)
#' 
#' @param class character string representing name of system organ class variable.
#' @param term character string representing name of preferred term variable.
#' @param id character string representing name of unique subject identifier
#'  variable.
#' @param grade character string representing name of grade of adverse event 
#'  variable.
#' @param col_by character string representing name of group variable that will 
#'  be used for a column header. \code{col_by}, can not be missing.
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
#'  
#' @examples 
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
    stop("grade_range needs to be a vector with 2 values")
  
  check_col_by(col_by, min_num_levels = 2)
  if (total %in% levels(col_by)) stop('...')
  
  # data prep ---------------------------------------------------------------
  
  df <- data.frame(class = class,
                   term = term,
                   subjid = id,
                   gradev = grade,
                   col_by = col_by,
                   stringsAsFactors = FALSE)

  # adding All Patients
  dfall <- df
  dfall$subjid <- paste0(dfall$subjid, '-ALL')
  dfall$col_by <- total
  
  df <- rbind(df, dfall)
  
  # total N for column header
  N <- tapply(df$subjid, df$col_by, function(x) (sum(!duplicated(x))))
  
  # need to remove extra records that came from subject level data
  # also any record that is missing soc or term
  df$class <- ifelse(df$class == '', NA, df$class)
  df$term <- ifelse(df$class == '', NA, df$term)
  
  df <- na.omit(df)
  
  # need to duplicate rows for each class to calculate overall class chunks
  df_overall_class <- df
  df_overall_class$term <- '- Overall -'
  
  df <- rbind(df, df_overall_class)
  
  # need the length of longest term for proper indentation
  term_max_length <- max(nchar(trimws(unique(df$term))))
  
  # start tabulating --------------------------------------------------------
  
  ## funcitons 
  
  # checks if there is any case and derives counts (percentage), otherwise 0
  count_perc <- function(x_cell) {
    N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
    if (N_i > 0) {
      n <- sum(!duplicated(x_cell$subjid)) # number of patients with at least one ae
      n * c(1, 1 / N[x_cell$col_by[1]]) # obtaining the total and getting percentage
    } else {
      rcell(0, format = "xx")
    }
  }
  
  # core function 
  
  # derives the any grade and by grades rows
  DeriveCore <- function(dt, nms) {
    # any grade row  
    # need to make gap between term and -any grade- to be equal term_max_length
    # after concat of 1, term
    tbl_all <- rtabulate(dt, row_by_var = no_by(paste(nms, paste0(rep(' ', term_max_length - nchar(trimws(nms))), collapse = ''), '- any grade -	')),
                         col_by_var = "col_by", count_perc, format = "xx (xx.x%)",
                         indent = 1)
    
    # by grades rows
    # need the highest grade per patient and grade variable should be a vector
    df_i_max_grade <- aggregate(gradev ~ col_by + subjid, data = dt, FUN = max,
                                drop = TRUE, na.rm = TRUE)
    # need factor grade for rtabulate
    df_i_max_grade$grade_f <- factor(df_i_max_grade$gradev,
                                     levels = seq(grade_range[1],
                                                  grade_range[2], by = 1))
    tbl_by_grade <- rtabulate(df_i_max_grade, row_by_var = "grade_f",
                              col_by_var = "col_by", count_perc, format = "xx (xx.x%)",
                              # 19 difference was the closest 
                              indent = term_max_length - 19)
    
    rbind(tbl_all, tbl_by_grade)
  }
  
  
  # sort function 
  
  # need to order by overall soc/term counts
  # need total from any grade all patients, the last column of the first row
  #  is all patients count
  # break ties by alphabetical order of pt
  order_term_class <- function(term_list, classord = FALSE) {
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
  
  ## end of functions
  
  # class and term chunks
  
  l_t_terms <- lapply(split(df, df$class), function(df_cl) {
    
    terms_s <- split(df_cl, df_cl$term)
    
    l_t_term_raw <- Map(
      function(df_cl_term, nms) {DeriveCore(df_cl_term, nms)},
      terms_s,
      names(terms_s)
    )
    
    # sorting by total counts and then by terms
    order_term_class(l_t_term_raw)
  })
  
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
                                             rtable(header = names(N), rrowl(x, rep(' ', length(N)))),
                                             l_t_terms_classes_sorted_stacked[[x]]
                                           )})
  
  # need to stacke altogether
  pre_final <- Reduce(rbind, l_t_terms_classes_sorted_soc)
  
  # Any adverse events ------------------------------------------------------
  AnyAE <- DeriveCore(df, nms = '- Any adverse events -')  
  
  # Format header by including N = ------------------------------------------
  
  final <- rbind(
    rtable(header = c(names(N)),
           rrowl(NULL, paste0('(N=', N, ')')),
           rrowl('MedDRA System Organ Class', rep(' ', length(N))),
           rrowl('MedDRA Preferred Term', rep(' ', length(N)), indent = 1),
           rrowl('NCI CTCAE Grade', rep(' ', length(N)), indent = term_max_length - 19)),
    AnyAE,
    pre_final
  )
  
  final
}
