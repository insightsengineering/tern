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
#' library(rtables)
#' AAE <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat')
#' ASL <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat')
#'
#' save(AAE, ASL, file = 'InputVads.Rdata')
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
#'    col_by = "TRT02AN",
#'    usubjid = "USUBJID",
#'    soc = "AEBODSYS",
#'    grade = "AETOXGR",
#'    grade_range = c(1,5),
#' )
t_aae <- function(asl, usubjid, soc, grade, grade_range, col_by) {
  
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

  #TODO a subset for testing needs to be removed
  df_i <- df[df$socv == df$socv[1],]
  
  
  # x_cell <- split(df_i, df_i$col_by)[[1]]
  count_perc <- function(x_cell) {
    N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
    if (isTRUE(N_i > 0)) {
      n <- sum(!duplicated(x_cell$usubjid)) # number of patients with at least one ae
      n * c(1, 1 / N_i)        
    } else {
      rcell(c(0, 0), format = "xx (xx%)")
    }
  }
  
  
  count_perc <- function(x_cell) {
    .GlobalEnv$xxxx <- x_cell
    N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
    if (N_i > 0) {
      n <- sum(!duplicated(x_cell$usubjid)) # number of patients with at least one ae
      n * c(1, 1 / N[x_cell$col_by[1]]) # obtaining the total and getting percentage
    } else {
      rcell(0, format = "xx")
    }
  }
  
  tbl_all <- rtabulate(df_i, row_by_var = no_by("any grade"), col_by_var = "col_by", count_perc, format = "xx (xx.xx%)")
  
  df_i_max_grade <- aggregate(gradev ~ col_by + usubjidv, data = df_i, FUN = max, drop = TRUE, na.rm=TRUE)
  df_i_max_grade$grade_f <- factor(df_i_max_grade$gradev, levels = seq(grade_range[1], grade_range[2], by = 1))
  tbl_by_grade <- rtabulate(df_i_max_grade, row_by_var = "grade_f", col_by_var = "col_by", count_perc, format = "xx (xx.xx%)")
}

rbind(
  tbl_all,
  tbl_by_grade
)

Viewer(tbl_all)

# TotN <- table(asl$TRT02AN)
# 
# rtabulate(x = asl, col_by_var = "TRT02AN", row_by_var = no_by('NCI CTCAE Grade'), function(x) {
#   paste0("N=", nrow(x))
# })
# 
# # - Any adverse events - by grade-----------------------------------------
# 
# # need highest grade per patient
# b1 <- aggregate(AETOXGR ~ TRT02AN + USUBJID, data = aae, FUN = max, drop = TRUE)
# # need factor for rtabulate
# b1$AETOXGRF <- as.factor(b1$AETOXGR)
# x <- rtabulate(x = b1, col_by_var = "TRT02AN", row_by_var = "AETOXGRF", function(x) {
#   
#   
#   
#   tmp <- c(nrow(x), nrow(x)/TotN[unique(x$TRT02AN)])
#   
#   .GlobalEnv$xxxx <- tmp
#   
#   tmp
#   
#   length(xxxx)
#   
# }, format = "xx.xx (xx.xx%)")
# 
# str(x[1,1])
# 
# # soc by grade-------------------------------------------------------------
# 
# # need highest grade per soc per patient
# df <- aggregate(AETOXGR ~ TRT02AN + AEBODSYS + USUBJID, data = aae, FUN = max, drop = TRUE)
# # need factor for rtabulate
# df$AETOXGRF <- as.factor(df$AETOXGR)
# 
# df.sp <- split(df, df$AEBODSYS, drop = FALSE)
# 
# df.sp.tab <- lapply(df.sp, function(df_i) {
#   rtabulate(x = df_i, col_by_var = "TRT02AN", row_by_var = "AETOXGRF", FUN = nrow)
# })
# 
# 
# rtabulate(df.s[[1]]$USUBJID, col_by,  function(x_cell_usubjid) length(x_cell_ubsubjid) * c(1, 1/N_ARM))
# 
# 
# 
# 
# df <- data.frame(soc, grade, col_by)
# 
# df.s <- split(df, soc, drop=FALSE)
# 
# 
# rbind(
#   rtabulate(df.s[[1]]$USUBJID, col_by,  function(x_cell_usubjid) length(x_cell_ubsubjid) * c(1, 1/N_ARM)),
#   rtabulate(df.s[[1]], row_by_var = "grade", col_by_var = "col_by", function(x_cell) {
#     nrow(x_cell) * c(1, 1/N_ARM)
#   }, format = "xx xx.xx%")
# )
# 
# 
# 
# t_aae <- function(soc, pt, grade, col_by) {
#   
#   df <- data.frame(soc, grade, col_by) 
#   
# 
#   tbl_all <- rtabulate()
#   
#   tbls_parts <- lapply(df, soc )
#   
#   rbind(
#     # tbl all
#     
#     # tbls by soc
#     
#   )
#   
#   
#   
# }
# 


# 
# t_aae <- function() {
#   
#   ## ARM always needs to be a factor
#   
#   ## do not use AAE and ASL as arguments
#   ## think of cdisc independent arguments
#   
#   df_tot_at_lease_one_ae <- merge(ASL[, c("USUBJID", "STUDYID")], AAE[, ...]) # aae USUBJID has NA
#   
#   tbl_at_lease_one_ae <- rtabulate(df_tot_at_lease_one_ae$"<var with NA>", function(x) {
#     sum(x, na.rm = TRUE) * c(1, 1/length(x))
#   }, row.name = "...")
#   
#   ## 
#   tbl_n_ae <- rtabulate(AAE$USUBJID, col_by = AAE$ARM, length)
# 
#   
#   ## Now create a list of tables, one table per term
#   ## for now lets just create class - term structure
#   ## we can look later into generalizing this to any depth
#   df <- AAE[, c("USUBJID", "AESOC", "ARM", "AETERM")]
#   
#   # use factor levels for ordering
#   df.s <- split(df, AAE$AESOC)
#   
#   lapply(df.s, function(df_i) {
#     rtabulate(df_i, col_var = "ARM", )
#   })
# }







