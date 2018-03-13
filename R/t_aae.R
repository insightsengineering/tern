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
#' 
#' AAE <- rgda::get_data("BCE", "/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat")
#' ASL <- rgda::get_data("BCE", "/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat")
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
#' asl$TRT02AN <- as.factor(asl$TRT02AN)
#' 
#' # AAE
#' # need all patients column for sorting and display
#' aaeALL <- AAE
#' aaeALL$USUBJID <- paste(aaeALL$USUBJID, '-ALL')
#' AAEALL <- rbind(AAE, aaeALL)
#' # treatment-emergent AEs
#' AAEALL$AEBODSYS <- ifelse(AAEALL$AEBODSYS == '',  'UNCODED', AAEALL$AEBODSYS)
#' AAEALL$AEDECOD <- ifelse(AAEALL$AEDECOD == '',  paste0(AAEALL$AETERM, '*'), AAEALL$AEDECOD)
#' aae <- AAEALL[AAEALL$TRTEMFL == 'Y' & AAEALL$ANLFL == 'Y', c("USUBJID", "AEBODSYS", "AEDECOD", "AETOXGR")]
#' aae <- merge(asl, aae, by = 'USUBJID')
#' aae$SOC_AEDECOD <- paste0(aae$AEBODSYS, '@', aae$AEDECOD)
#' aae$TRT02AN <- as.factor(aae$TRT02AN)

TotN <- table(asl$TRT02AN)

rtabulate(x = asl, col_by_var = "TRT02AN", row_by_var = no_by('NCI CTCAE Grade'), function(x) {
  paste0("N=", nrow(x))
})

# - Any adverse events - by grade-----------------------------------------

# need highest grade per patient
b1 <- aggregate(AETOXGR ~ TRT02AN + USUBJID, data = aae, FUN = max, drop = TRUE)
# need factor for rtabulate
b1$AETOXGRF <- as.factor(b1$AETOXGR)
rtabulate(x = b1, col_by_var = "TRT02AN", row_by_var = "AETOXGRF", function(x){
  c(nrow(x), nrow(x)/TotN[unique(x$TRT02AN)])}
# , format = "xx.xx (xx.xx%)"
)


# soc by grade-------------------------------------------------------------

# need highest grade per soc per patient
df <- aggregate(AETOXGR ~ TRT02AN + AEBODSYS + USUBJID, data = aae, FUN = max, drop = TRUE)
# need factor for rtabulate
df$AETOXGRF <- as.factor(df$AETOXGR)

df.sp <- split(df, df$AEBODSYS, drop = FALSE)

df.sp.tab <- lapply(df.sp, function(df_i) {
  rtabulate(x = df_i, col_by_var = "TRT02AN", row_by_var = "AETOXGRF", FUN = nrow)
})

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







