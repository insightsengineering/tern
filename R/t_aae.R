
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
#' 
#' AAE <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/xaae.sas7bdat')
#' ASL <- read_bce('/opt/BIOSTAT/prod/s28363v/libraries/asl.sas7bdat')
#'
#' save(AAE, ASL, file = 'InputVads.Rdata')
#' load('InputVads.Rdata')
#' 
#' }
#' 
#' help(p = rtables)
#' 
#' 


load('InputVads.Rdata')
library(rtables)

# safety-evaluable patients
asl <- ASL[
  ASL$SAFFL == 'Y'
  , c("USUBJID", "TRT02AN")
]

asl$TRT02AN <- as.factor(asl$TRT02AN)
levs <- c('Stage 1\nCohort 1'
, 'Stage 1\nCohort 2'
, 'Stage 1\nCohort 3'
, 'Stage 2\nMCRC'
, 'Stage 2\nNSCLC'
, 'Stage 2\nMELANOMA'
, 'Stage 2\nBIOPSY'
, 'Stage 2\nBIOPSY ALT')

levels(asl$TRT02AN) <- c(levs)


# treatment-emergent AEs
aae <- AAE[AAE$TRTEMFL == 'Y', c("USUBJID", "AEBODSYS", "AEDECOD", "AETOXGR")]

aae <- merge(asl, aae, by = 'USUBJID')

# N for totals and header
TotNandHead <- table(asl$TRT02AN)

# build a header with N counts
# "group\n(N=x)" format

head <- vapply(
  seq_along(TotNandHead)
  , function(x) {
    paste0(names(TotNandHead)[x], '\n', '(N=', TotNandHead[x], ')')
  }
  , FUN.VALUE = character(1)
)

head <- append(c('MedDRA System Organ Class\n  MedDRA Preferred Term', 'NCI CTCAE Grade'), head)
names(head) <- paste0('var', seq_along(1:length(head)))

# - Any Grade -
# number of subjects (%)
AnyGrade <- mapply(
  function(aepats, totals){
    count <- as.integer(length(unique(aepats)))
    percent <- round(count / totals, 2)
    paste(count, percent)
    
  }
  , aepats = split(aae$USUBJID, aae$TRT02AN)
  , totals = TotNandHead
)


AnyGrade <- append(c('- Any adverse events -', '- Any Grade -'), AnyGrade)

# grade rows
# Number of patients in GroupX with highest grade of all AEs equals one, two, ...
# need to keep the hight grade for each patient

MaxToxGr <- vapply(
  split(aae$AETOXGR, aae$USUBJID)
  , function(x){
    max(as.integer(x))
  }
  , FUN.VALUE = integer(1)
)

data.frame(USUBJID = names(MaxToxGr), AETOXGR = MaxToxGr, row.names = NULL)




dt <- as.data.frame(rbind(head, AnyGrade), stringsAsFactors = FALSE)




# 
# # preparing anygrade row for parsing as rrow, c(x, x)
# t <- vapply(
#   AnyGrade
#   , function(x){
#     paste0('c(', x[1], ', ', x[2], ')')
#   }
#   , FUN.VALUE = character(1)
# )
# 
# 
# tx <- paste0("rrow('- Any adverse events -', '- Any Grade -', ", paste0(t[1:2], collapse = ', '), ')')
# 
# 
# # header
# # "MedDRA System Organ Class\n  MedDRA Preferred Term"
# # we need this as a column header for first column
# rtable(
#   header = c("\n\nNCI CTCAE Grade", head0[1:2])
#   , format = "xx (xx.xx%)"
#   , rrow('- Any adverse events -', '- Any Grade -', c(1, 2), c(1, 2))
#   , eval(parse(text = "rrow('- Any adverse events -', '- Any Grade -', c(1, 2), c(1, 2))"))
#   , eval(parse(text = tx))
# )
# 
# 
# eval(parse(text = "rrow('- Any adverse events -', '- Any Grade -', c(1, 2), c(1, 2)"))
# 
# rtable(
#   header = c("Treatement\nN=100", "Comparison\nN=300"),
#   format = "xx (xx.xx%)",
#   rrow("A", c(104, .2), c(100, .4)),
#   rrow("B", c(23, .4), c(43, .5))
# )




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










