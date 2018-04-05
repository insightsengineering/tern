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
#'  be used for a column header. \code{col_by} can not be missing.
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
#'  derived usind adverse events dataset. At the preferred term (PT) level,
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
#' \dontrun
#' {
#' 
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
#' # filter adverse events dataset
#' aae <- AAE[AAE$TRTEMFL == 'Y' & AAE$ANLFL == 'Y' & AAE$AEREL1 == 'Y' & trimws(AAE$TUMTYPE) != 'NON-SMALL CELL LUNG',
#'  !names(AAE) %in% 'TUMTYPE']
#' 
#' # left join subject-level dataset with adverse events
#' aae <- merge(asl, aae, by = c('USUBJID'), all.x = TRUE)
#'
#' tbl <- t_ae_ctc(
#'  class = aae$AEBODSYS,
#'  term =  aae$AEDECOD,
#'  id = aae$USUBJID,
#'  grade = aae$AETOXGR,
#'  col_by = aae$TUMTYPE,
#'  total = "All Patients",
#'  grade_range = c(1, 5)
#' )
#' 
#' Viewer(tbl)
#' }
#' 
t_ae_ctc <- function(class, term, id, grade, col_by, total = "All Patients", grade_range) {
  
# check argument validity and consitency ----------------------------------
  
  if (length(class) == 0) stop("there are no records in aae for this subset")
  if (!is.vector(grade_range) || !length(grade_range) == 2) 
    stop("grade_range needs to be a vector with 2 values")
  if (any(is.na(col_by)) | sum(col_by == '') > 0) stop("No NA's are allowed for col_by")
  
  # indentation number for grades
  indent_num <- 15
  

# data prep ---------------------------------------------------------------
  
  df <- data.frame(class = class,
                           term = term,
                           subjid = id,
                           gradev = grade,
                           col_by = col_by,
                           stringsAsFactors = FALSE)
  
  # adding All Patients
  dfall <- df
  dfall$subjid <- paste(dfall$subjid, '-ALL')
  dfall$col_by <- 'All Patients'
  
  df <- rbind(df, dfall)
  
  # all patients column should be factor and the rightmost column
  df$col_by <- factor(df$col_by,
                        c(sort(unique(df$col_by)[unique(df$col_by) != 'All Patients']),
                          'All Patients'))
  
  # total N for column header
  N <- tapply(df$subjid, df$col_by, function(x) (sum(!duplicated(x))))
  
  
  # need to remove extra records that came from subject level data
  # also any record that is missing soc or term
  df$class <- ifelse(df$class == '', NA, df$class)
  df$term <- ifelse(df$class == '', NA, df$term)

  df <- na.omit(df)
  
  # start tabulating --------------------------------------------------------
  
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
  
  # core function -----------------------------------------------------------
  
  # derives the any grade and by grades rows
  DeriveCore <- function(dt) {
    # any grade row  
    tbl_all <- rtabulate(dt, row_by_var = no_by("- any grade - "),
                         col_by_var = "col_by", count_perc, format = "xx (xx.x%)",
                         indent = indent_num)
    
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
                              indent = indent_num)
    
    rbind(tbl_all, tbl_by_grade)
  }
  
# Any adverse events ------------------------------------------------------
  
  AnyAE <- rbind(
    rtable(header = names(N), rrowl('- Any adverse events -', rep(' ', length(N)))),
    DeriveCore(df)
  )
  
# SOC Overall chunks ---------------------------------------------------------

  # sapply preserves names of term as names of list items
  SocChunks_list <- sapply(unique(df$class), simplify = FALSE,
                      function(SocName) DeriveCore(df[df$class == SocName,]))
  
  # need to order by overall soc counts
  # need total from any grade all patients, the last column of the first row
  #  is all patients count
  # break ties by alphabetical order of socs
  SocVector <- vapply(names(SocChunks_list),
                      function(x) SocChunks_list[[x]][1, length(N)][1],
                      FUN.VALUE = numeric(1))
  
  SocOrder <- SocVector[order(-SocVector, names(SocVector))]
  
  # rebuilding the list by choosing items in the desired order
  SocChunks_list_ordered <- SocChunks_list[names(SocOrder)]
  
  # need soc and - overall - on a separate row
  SocChunks <- sapply(names(SocChunks_list_ordered), simplify = FALSE,
         function(x) {
           rbind(
             rtable(header = names(N), rrowl(x, rep(' ', length(N)))),
             rtable(header = names(N),
                    rrowl('- Overall -', rep(' ', length(N)), indent = 1)),
             SocChunks_list_ordered[[x]]
           )})
  
# PT chunks ---------------------------------------------------------------

  # there is a potential that the same PT can be under different SOC so need
  # to combine the soc and pt as a new name and this will also help with linking
  # back to soc for sorting and stacking
  df$socv_ptv <- paste0(df$class, '@', df$term)

  ###### THE SLOWESET PART #####
  # sapply preserves names of term as names of list items
  PTChunks_list <- sapply(unique(df$socv_ptv), simplify = FALSE,
                           function(SocName) DeriveCore(df[df$socv_ptv == SocName,]))
  

  # need to sort pt within soc and place them under soc chunks
  # need total from any grade all patients, the last column of the first row is
  # all patients count
  # break ties by alphabetical order of socs
  PTVector <- vapply(names(PTChunks_list),
                     function(x) PTChunks_list[[x]][1, length(N)][1],
                     FUN.VALUE = numeric(1))
  PTOrder <- PTVector[order(-PTVector, names(PTVector))]
  # rebuilding the list by choosing items in the desired order
  PTChunks_list_ordered <- PTChunks_list[names(PTOrder)]
  # need a copy to preserve soc pt combo name for attribute name2
  PTChunks_list_ordered_for_attr <- PTChunks_list_ordered
  
  # for display purposes need to get rid of the soc part, creating name2 attrib
  # which will be used after merging
  names(PTChunks_list_ordered) <- sub('^(.{1,})(@)(.{1,}$)', '\\3',
                                      names(PTChunks_list_ordered))
  
  # need PT on a separate row
  PTChunks <- sapply(
    names(PTChunks_list_ordered), simplify = FALSE,
      function(x) {
        rbind(
          rtable(header = names(N), rrowl(x, rep(' ', length(N)), indent = 1)),
          PTChunks_list_ordered[[x]])
        })
  # reattaching soc as a name2 attr for merging with soc
  for (i in seq_along(PTChunks)) {
    attr(PTChunks[[i]], 'name2') <- sub('^(.{1,})(@)(.{1,}$)', '\\1',
                                        names(PTChunks_list_ordered_for_attr))[i]
  }
  
# soc and pt together -----------------------------------------------------

  ##### SECOND SLOWEST PART #########
  # build a 3rd list that will use soc names from soc list to choose from 
  # soc list (SocChunks) and pt list (PTChunks) in an order from soc list
  
  SocPTChunks <- sapply(names(SocChunks), function(x){
    rbind(
      SocChunks[[x]],
      # extracting soc part from combined soc pt name and checking against soc
      # need to rbind together so that it becomes one rtable
      Reduce(rbind, PTChunks[vapply(PTChunks, function(x) attr(x, 'name2'),
                                    character(1)) == x])
    )
  }, simplify = FALSE)

# Format header by including N = ------------------------------------------

  final <- rbind(
    rtable(header = c(names(N)),
           rrowl(NULL, paste0('(N=', N, ')')),
           rrowl('MedDRA System Organ Class', rep(' ', length(N))),
           rrowl('MedDRA Preferred Term', rep(' ', length(N)), indent = 1),
           rrowl('NCI CTCAE Grade', rep(' ', length(N)), indent = indent_num)),
    AnyAE,
    Reduce(rbind, SocPTChunks)
  )
  
  final
}
