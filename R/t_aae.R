#' Adverse Events Table
#' 
#' 
#' 
#' @author Edgar
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' devtools::install_github("Rpackages/rgda", host = "https://github.roche.com/api/v3")
#' 
#' library(rtables)
#' 
#' AAE <- rgda::get_data("BCE", "/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat")
#' ASL <- rgda::get_data("BCE", "/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat")
#' 
#' 
#' # -------------------
#' # the following example should reproduce table
#' file.show('/opt/BIOSTAT/prod/s28363v/reports/t_ae_ctc_ALLWD_SE.out')
#' library(rocheBCE)
#' AAE <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat')
#' ASL <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat')
#' save(AAE, ASL, file = 'InputVads.Rdata')
#'
#' load('InputVads.Rdata')
#' 
#' # safety-evaluable patients
#' asl <- ASL[ASL$SAFFL == "Y", c("USUBJID", "TRT02AN")]
#' 
#' # treatment-emergent AEs
#' aae <- AAE[AAE$TRTEMFL == 'Y' & AAE$ANLFL == 'Y', !names(AAE) %in% ('TRT02AN')]
#' 
#' 
#' tbl <- t_aae(
#'    asl = asl,
#'    aae = aae,
#'    usubjid = "USUBJID",
#'    soc = "AEBODSYS",
#'    pt = "AEDECOD",
#'    rawpt = "AETERM",
#'    grade = "AETOXGR",
#'    grade_range = c(1,5),
#'    col_by = "TRT02AN"
#' )
#' 
#' Viewer(tbl)
t_aae <- function(asl, aae, usubjid, soc, pt, rawpt, grade, grade_range, col_by) {
  
# check argument validity and consisency ----------------------------------

  if (any(is.na(asl[[col_by]]))) stop("In asl data no NA's allowed in col_by")

# data prep ---------------------------------------------------------------

  # need all patients column for sorting and display
  aslALL <- asl
  aslALL[[usubjid]] <- paste(aslALL[[usubjid]], '-ALL')
  aslALL[[col_by]] <- 99
  asl <- rbind(asl, aslALL)
  
  # need all patients column for sorting and display
  aaeALL <- aae
  aaeALL[[usubjid]] <- paste(aaeALL[[usubjid]], '-ALL')
  aae <- rbind(aae, aaeALL)
  
  aae <- merge(asl, aae, by = 'USUBJID')
  
  # AEs with missing soc or pt code
  aae[[soc]] <- ifelse(aae[[soc]] == '',  'UNCODED', aae[[soc]])
  aae[[pt]] <- ifelse(aae[[pt]] == '',  paste0(aae[[rawpt]], '*'), aae[[pt]])
  
  usubjidv <- aae[[usubjid]]
  socv <- aae[[soc]]
  ptv <- aae[[pt]]
  gradev <- as.numeric(aae[[grade]])
  col_byv <- aae[[col_by]]
  
  df <- data.frame(usubjidv, socv, ptv, gradev, col_by = factor(col_byv), stringsAsFactors = FALSE)
  
  # total N for each group from subject level dataset
  # double brackets are used to get a vector instead of 1 column df
  # otherwise tapply does not work
  N <- tapply(asl[[usubjid]], asl[[col_by]], function(x) (length(!duplicated(x))))
  

# start tabulating --------------------------------------------------------


  # checks if there is any case and derives counts (percentage), otherwise 0
  count_perc <- function(x_cell) {
    N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
    if (N_i > 0) {
      n <- sum(!duplicated(x_cell$usubjid)) # number of patients with at least one ae
      n * c(1, 1 / N[x_cell$col_by[1]]) # obtaining the total and getting percentage
    } else {
      rcell(0, format = "xx")
    }
  }
  

# core function -----------------------------------------------------------

  # derives the any grade and by grades rows
  DeriveCore <- function(dt) {
    # any grade row  
    tbl_all <- rtabulate(dt, row_by_var = no_by("- any grade - "), col_by_var = "col_by", count_perc, format = "xx (xx.x%)")
    
    # by grades rows
    # need the highest grade per patient and grade variable should be a vector
    df_i_max_grade <- aggregate(gradev ~ col_by + usubjidv, data = dt, FUN = max, drop = TRUE, na.rm = TRUE)
    # need factor grade for rtabulate
    df_i_max_grade$grade_f <- factor(df_i_max_grade$gradev, levels = seq(grade_range[1], grade_range[2], by = 1))
    tbl_by_grade <- rtabulate(df_i_max_grade, row_by_var = "grade_f", col_by_var = "col_by", count_perc, format = "xx (xx.x%)")
    
    rbind(tbl_all, tbl_by_grade)
  }
  

# Any adverse events ------------------------------------------------------

  
  AnyAE <- rbind(
    rtable(header = names(N), rrowl('- Any adverse events -', rep(' ', length(N)))),
    DeriveCore(df)
  )
  

# SOC chunks --------------------------------------------------------------

  # sapply preserves names of term as names of list items
  SocChunks_list <- sapply(unique(df$socv), simplify = FALSE,
                      function(SocName) DeriveCore(df[df$socv == SocName,]))
  
  # need to order by overall soc counts
  # need total from any grade all patients, the last column of the first row is all patients count
  # break ties by alphabetical order of socs
  SocVector <- vapply(names(SocChunks_list), function(x) SocChunks_list[[x]][1, length(N)][1], FUN.VALUE = numeric(1))
  SocOrder <- SocVector[order(-SocVector, names(SocVector))]
  # rebuilding the list by choosing items in the desired order
  SocChunks_list_ordered <- SocChunks_list[names(SocOrder)]
  
  # need soc and - overall - on a separate row
  SocChunks <- sapply(names(SocChunks_list_ordered), simplify = FALSE,
         function(x) {
           rbind(
             rtable(header = names(N), rrowl(x, rep(' ', length(N)))),
             rtable(header = names(N), rrowl('- Overall -', rep(' ', length(N)))),
             SocChunks_list_ordered[[x]]
           )})
  
# PT chunks ---------------------------------------------------------------

  # there is a potential that the same PT can be under different SOC so need
  # to combine the soc and pt as a new name and this will also help with linking
  # back to soc for sorting and stacking
  df$socv_ptv <- paste0(df$socv, '@', df$ptv)

  ###### THE SLOWESET PART #####
  # sapply preserves names of term as names of list items
  PTChunks_list <- sapply(unique(df$socv_ptv), simplify = FALSE,
                           function(SocName) DeriveCore(df[df$socv_ptv == SocName,]))
  

  # need to sort pt within soc and place them under soc chunks
  # need total from any grade all patients, the last column of the first row is all patients count
  # break ties by alphabetical order of socs
  PTVector <- vapply(names(PTChunks_list), function(x) PTChunks_list[[x]][1, length(N)][1], FUN.VALUE = numeric(1))
  PTOrder <- PTVector[order(-PTVector, names(PTVector))]
  # rebuilding the list by choosing items in the desired order
  PTChunks_list_ordered <- PTChunks_list[names(PTOrder)]
  # need a copy to preserve soc pt combo name for attribute name2
  PTChunks_list_ordered_for_attr <- PTChunks_list_ordered
  
  # for display purposes need to get rid of the soc part, creating name2 attrib
  # which will be used after merging
  names(PTChunks_list_ordered) <- sub('^(.{1,})(@)(.{1,}$)', '\\3', names(PTChunks_list_ordered))
  
  # need PT on a separate row
  PTChunks <- sapply(names(PTChunks_list_ordered), simplify = FALSE,
                      function(x) {
                        rbind(
                          rtable(header = names(N), rrowl(x, rep(' ', length(N)))),
                          PTChunks_list_ordered[[x]]
                        )})
  # reattaching soc as a name2 attr for merging with soc
  for (i in seq_along(PTChunks)) {
    attr(PTChunks[[i]], 'name2') <- sub('^(.{1,})(@)(.{1,}$)', '\\1', names(PTChunks_list_ordered_for_attr))[i]
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
      Reduce(rbind, PTChunks[vapply(PTChunks, function(x) attr(x, 'name2'), character(1)) == x])
    )
  }, simplify = FALSE)

  rbind(
    AnyAE,
    Reduce(rbind, SocPTChunks)
  )
  
}
