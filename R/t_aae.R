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


# data prep ---------------------------------------------------------------


load('InputVads.Rdata')
library(rtables)

# precision for rounding
RoundPrec <- 4

# safety-evaluable patients
asl <- ASL[
  ASL$SAFFL == 'Y'
  , c("USUBJID", "TRT02AN")
]

asl$TRT02AN <- as.factor(asl$TRT02AN)
levs <- c('Stage 1 Cohort 1'
, 'Stage 1 Cohort 2'
, 'Stage 1 Cohort 3'
, 'Stage 2 MCRC'
, 'Stage 2 NSCLC'
, 'Stage 2 MELANOMA'
, 'Stage 2 BIOPSY'
, 'Stage 2 BIOPSY ALT')

levels(asl$TRT02AN) <- c(levs)


# treatment-emergent AEs
AAE$AEBODSYS <- ifelse(AAE$AEBODSYS == '',  'UNCODED', AAE$AEBODSYS)
AAE$AEDECOD <- ifelse(AAE$AEDECOD == '',  AAE$AETERM, AAE$AEDECOD)

aae <- AAE[AAE$TRTEMFL == 'Y' & AAE$ANLFL == 'Y', c("USUBJID", "AEBODSYS", "AEDECOD", "AETOXGR")]

aae <- merge(asl, aae, by = 'USUBJID')

# Table section -----------------------------------------------------------


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
######################

# - Any Grade -
# number of subjects (%)
AnyGrade <- mapply(
  function(aepats, totals){
    count <- as.integer(length(unique(aepats)))
    percent <- round(count / totals, RoundPrec)
    paste(count, percent)
    
  }
  , aepats = split(aae$USUBJID, aae$TRT02AN)
  , totals = TotNandHead
)


AnyGrade <- append(c('- Any adverse events -', '- Any Grade -'), AnyGrade)
######################


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

# need group names from asl
d1 <- merge(
  asl
  , data.frame(USUBJID = names(MaxToxGr), AETOXGR = factor(MaxToxGr), row.names = NULL)
  , all.x = TRUE
)

l1 <- Map(
  function(aepats, totals){
    count <- table(aepats)
    percent <- round(count / totals, RoundPrec)
    paste(count, percent)
  }
  , aepats = split(d1$AETOXGR, d1$TRT02AN)
  , totals = TotNandHead
)

AnyGradeByGrade <- cbind('', levels(d1$AETOXGR), Reduce(cbind, l1))
######################

# - Overall - - Any Grade - SOC
# number of subjects (%)

GetAnyGradeBySMTH <- function(splvar) {
  
  # split subject id by groups
  AnyGradeSoc <- vapply(
    split(aae$USUBJID, aae[c(splvar, 'TRT02AN')])
    , function(x){
      length(unique(x))
    }
    , FUN.VALUE = numeric(1)
  )
  
  d <- data.frame(grps = names(AnyGradeSoc), vals = AnyGradeSoc, row.names = NULL)
  
  # separating group and arm into columns
  d$arm <- gsub('(^.*\\.)', replacement = '', x = d$grps)
  d$soc <- gsub('(\\..*)', replacement = '', x = d$grps)
  # need percentage out of N
  d <- merge(d, data.frame(arm = names(TotNandHead), total = TotNandHead))
  d$vals <- paste(d$vals, round(d$vals / d$total.Freq, RoundPrec))
  
  # transpose long to wide to have arms as columns
  ld <- Map(
    function(x){
      df <- d[d$arm == x, 'vals']
      df
    }
    , x = levels(asl$TRT02AN)
  )
  cbind(unique(d$soc), '- Any Grade -', Reduce(cbind, ld))
}



AnyGradeSoc <-GetAnyGradeBySMTH(splvar = 'AEBODSYS')
######################

# - Overall - - Any Grade - pref term

AnyGradePT <-GetAnyGradeBySMTH(splvar = 'AEDECOD')

######################

# grade rows by soc, pt

MaxToxGr <- vapply(
  split(aae$AETOXGR, aae[c('AEBODSYS', 'USUBJID')])
  , function(x){
      if (length(x) != 0) {
        max(as.integer(x))
      } else -99L
  }
  , FUN.VALUE = integer(1)
)

# separating group and arm into columns
d <- data.frame(grps = names(MaxToxGr), vals = MaxToxGr, row.names = NULL)
d$soc <- gsub('(\\..*)', replacement = '', x = d$grps)
d$USUBJID <- gsub('(^[^.]*)(\\.)(.*)', replacement = '\\3', x = d$grps)
d$vals <- ifelse(d$vals == -99, NA, d$vals)
d$vals <- factor(d$vals, levels = c('1', '2', '3', '4', '5'))

# need group names from asl
d1 <- merge(
  data.frame(arm = asl$TRT02AN, USUBJID = asl$USUBJID)
  , data.frame(USUBJID = d$USUBJID, soc = d$soc, AETOXGR = d$vals, row.names = NULL)
  , all.x = TRUE
)

l1 <- Map(
  function(aepats, totals){
    count <- table(aepats)
    percent <- round(count / totals, RoundPrec)
    paste(count, percent)
  }
  , aepats = split(d1$AETOXGR, d1[c('arm', 'soc')])
  , totals = TotNandHead
)


df <- as.data.frame(cbind(grps = names(l1), Reduce(rbind, l1)))
# separating group and arm into columns
df$arm <- gsub('(\\..*)', replacement = '', x = df$grps)
df$soc <- gsub('(^[^.]*)(\\.)(.*)', replacement = '\\3', x = df$grps)
df <- df[, !names(df) %in% 'grps']
names(df) <- c(paste0('V', 1:5), 'arm', 'soc')


df2 <- reshape(df, direction = "long", varying = 1:5, sep = "")
df2$val <- as.character(df2$V)


# transpose long to wide to have arms as columns
ld <- Map(
  function(x){
    df <- df2[df2$arm == x, c('val')]
    df
  }
  , x = levels(asl$TRT02AN)
)

OverallGrades <- cbind(unique(df2$soc), 1:5, Reduce(cbind, ld))

######################

final <- rbind(head, AnyGrade, AnyGradeByGrade, AnyGradeSoc, AnyGradePT, OverallGrades)


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










