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
#' 
#' library(rocheBCE)
#' AAE <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat')
#' ASL <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat')
#' save(AAE, ASL, file = 'InputVads.Rdata')
#'
#' library(rtables)
#' load('InputVads.Rdata')
#' 
#' 
#' # need all patients column for sorting and display
#' aslALL <- ASL
#' aslALL$USUBJID <- paste(aslALL$USUBJID, '-ALL')
#' aslALL$TRT02AN <- 99
#' 
#' ASLALL <- rbind(ASL, aslALL)
#' 
#' # safety-evaluable patients
#' asl <- ASLALL[ASLALL$SAFFL == "Y", c("USUBJID", "TRT02AN")]
#' 
#' # AAE
#' # need all patients column for sorting and display
#' aaeALL <- AAE
#' aaeALL$USUBJID <- paste(aaeALL$USUBJID, '-ALL')
#' AAEALL <- rbind(AAE, aaeALL)
#' # treatment-emergent AEs
#' AAEALL$AEBODSYS <- ifelse(AAEALL$AEBODSYS == '',  'UNCODED', AAEALL$AEBODSYS)
#' AAEALL$AEDECOD <- ifelse(AAEALL$AEDECOD == '',  paste0(AAEALL$AETERM, '*'), AAEALL$AEDECOD)
#' aae <- AAEALL[AAEALL$TRTEMFL == 'Y' & AAEALL$ANLFL == 'Y', !names(AAE) %in% ('TRT02AN')]
#' aae <- merge(asl, aae, by = 'USUBJID')
#' 
#' tbl <- t_aae(
#'    asl = asl,
#'    aae = aae,
#'    usubjid = "USUBJID",
#'    soc = "AEBODSYS",
#'    grade = "AETOXGR",
#'    grade_range = c(1,5),
#'    col_by = "TRT02AN"
#' )
#' 
#' Viewer(tbl)
t_aae <- function(asl, aae, usubjid, soc, grade, grade_range, col_by) {
  
  # check argument validity and consisency
  if (any(is.na(aae[[col_by]]))) stop("In aae data no NA's allowed in col_by")
  if (any(is.na(asl[[col_by]]))) stop("In asl data no NA's allowed in col_by")
  
  # data prep
  usubjidv <- aae[[usubjid]]
  socv <- aae[[soc]]
  gradev <- as.numeric(aae[[grade]])
  col_byv <- aae[[col_by]]
  
  df <- data.frame(usubjidv, socv, gradev, col_by = factor(col_byv), stringsAsFactors = FALSE)
  
  # total N for each group from subject level dataset
  # double brackets are used to get a vector instead of 1 column df
  # otherwise tapply does not work
  N <- tapply(asl[[usubjid]], asl[[col_by]], function(x) (length(!duplicated(x))))
  
  # start tabulating

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
  
  dt <- df[df$socv == df$socv[1],]
  
  # derives the any grade and by grades rows
  DeriveCore <- function(dt) {
    # any grade row  
    tbl_all <- rtabulate(dt, row_by_var = no_by("- any grade - "), col_by_var = "col_by", count_perc, format = "xx (xx.x%)")
    
    # by grades rows
    # need the highest grade per patient and grade variable should be a vector
    df_i_max_grade <- aggregate(gradev ~ col_by + usubjidv, data = dt, FUN = max, drop = TRUE, na.rm=TRUE)
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

  # sapply preserves names of soc
  SocChunks_list <- sapply(unique(df$socv), simplify = FALSE,
                      function(SocName) DeriveCore(df[df$socv == SocName,]))
  
  # need to order by overall soc counts
  # need total from any grade all patients, the last column of the first row is all patients count
  # break ties by alphabetical order of socs
  SocVector <- vapply(names(SocChunks_list), function(x) SocChunks_list[[x]][1, length(N)][1], FUN.VALUE = numeric(1))
  SocOrder <- SocVector[order(-SocVector, names(SocVector))]
  # names are already in SocChunks_list because of that using USE.NAMES = FALSE
  SocChunks_list_ordered <- sapply(names(SocOrder), function(x) SocChunks_list[x], USE.NAMES = FALSE)
  
  
  # need soc and - overall - on a separate row
  SocChunks <- sapply(names(SocChunks_list_ordered), simplify = FALSE,
         function(x) {
           rbind(
             rtable(header = names(N), rrowl(x, rep(' ', length(N)))),
             rtable(header = names(N), rrowl('- Overall -', rep(' ', length(N)))),
             SocChunks_list_ordered[[x]]
           )})
  
  rbind(
    AnyAE,
    socs <- Reduce(rbind, SocChunks)
  )
}







