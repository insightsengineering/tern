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
# library(rtables)

# precision for rounding
RoundPrec <- 4

# need all patients column for sorting and display
aslALL <- ASL
aslALL$USUBJID <- paste(aslALL$USUBJID, '-ALL')
aslALL$TRT02AN <- 99

ASLALL <- rbind(ASL, aslALL)

# safety-evaluable patients
asl <- ASLALL[
  ASLALL$SAFFL == 'Y'
  , c("USUBJID", "TRT02AN")
]


asl$TRT02AN <- as.factor(asl$TRT02AN)
levs <- c(
  'Stage 1 Cohort 1'
  , 'Stage 1 Cohort 2'
  , 'Stage 1 Cohort 3'
  , 'Stage 2 MCRC'
  , 'Stage 2 NSCLC'
  , 'Stage 2 MELANOMA'
  , 'Stage 2 BIOPSY'
  , 'Stage 2 BIOPSY ALT'
  , 'All Patients'
)

levels(asl$TRT02AN) <- c(levs)

# AAE
# need all patients column for sorting and display
aaeALL <- AAE
aaeALL$USUBJID <- paste(aaeALL$USUBJID, '-ALL')
AAEALL <- rbind(AAE, aaeALL)
# treatment-emergent AEs
AAEALL$AEBODSYS <- ifelse(AAEALL$AEBODSYS == '',  'UNCODED', AAEALL$AEBODSYS)
AAEALL$AEDECOD <- ifelse(AAEALL$AEDECOD == '',  paste0(AAEALL$AETERM, '*'), AAEALL$AEDECOD)
aae <- AAEALL[AAEALL$TRTEMFL == 'Y' & AAEALL$ANLFL == 'Y', c("USUBJID", "AEBODSYS", "AEDECOD", "AETOXGR")]
aae <- merge(asl, aae, by = 'USUBJID')
aae$SOC_AEDECOD <- paste0(aae$AEBODSYS, '@', aae$AEDECOD)


# Table section -----------------------------------------------------------

# merges totals to df and pastes count with derived proportion
MergeTotalGetPercent <- function(dt) {
  dt <- merge(dt, TotNandHead)
  # need totals for percentages
  dt$val <- paste(dt$USUBJID, round(dt$USUBJID / dt$total, RoundPrec))
  dt
}

# N for totals and header
TblTot <- table(asl$TRT02AN)
TotNandHead <- as.data.frame(TblTot)
names(TotNandHead) <- c('TRT02AN', 'total')

# build a header with N counts
# "group\n(N=x)" format


# head --------------------------------------------------------------------
head <- vapply(
  seq_along(TblTot)
  , function(x) {
      paste0(names(TblTot)[x], '\n', '(N=', TblTot[x], ')')
  }
  , FUN.VALUE = character(1)
)

head <- append(c('MedDRA System Organ Class\n  MedDRA Preferred Term', 'NCI CTCAE Grade'), head)
Head <- as.data.frame(t(head), stringsAsFactors = FALSE)
Head$ord <- 0
Head$ordPT <- 0
######################


# any adverse event - any grade -------------------------------------------
a1 <- aggregate(USUBJID ~ TRT02AN, data = aae, FUN = function(x) length(unique(x)))
a1 <- MergeTotalGetPercent(a1)
# this order is necessary so that reshape correctly places column and grades
a2 <- a1[order(a1$TRT02AN),]
a2$id <- rep(1, nrow(a2))

a3 <- reshape(
  data = a2[, c("TRT02AN", "val", "id")]
  , idvar = 'id', timevar = 'TRT02AN'
  , direction = 'wide', v.names = 'val'
)

a3 <- a3[, !names(a3) %in% 'id']

AnyGrade <- cbind('- Any adverse events -', '- Any Grade -', a3)
AnyGrade$ord <- 0
AnyGrade$ordPT <- 0
######################


# any adverse event - by grades -------------------------------------------
b1 <- aggregate(AETOXGR ~ TRT02AN + USUBJID, data = aae, FUN = max, drop = TRUE)
b2 <- aggregate(USUBJID ~ TRT02AN + AETOXGR, data = b1, FUN = length, drop = FALSE)
b2 <- MergeTotalGetPercent(b2)
# this order is necessary so that reshape correctly places column and grades
b2 <- b2[order(b2$TRT02AN, b2$AETOXGR),]

b3 <- reshape(
  data = b2[, c("TRT02AN", "AETOXGR", "val")]
  , idvar = c('AETOXGR'), timevar = 'TRT02AN'
  , direction = 'wide', v.names = 'val'
)

AnyAEByGrade <- cbind('', b3)
AnyAEByGrade$ord <- 0
AnyAEByGrade$ordPT <- 0
######################


# soc - any grade ---------------------------------------------------------
c1 <- aggregate(USUBJID ~ TRT02AN + AEBODSYS, data = aae
                , FUN = function(x) length(unique(x))
                , drop = FALSE)
c1 <- MergeTotalGetPercent(c1)
# this order is necessary so that reshape correctly places column and grades
c2 <- c1[order(c1$TRT02AN),]

c3 <- reshape(
  data = c2[, c("TRT02AN", "AEBODSYS", "val")]
  , idvar = 'AEBODSYS', timevar = 'TRT02AN'
  , direction = 'wide', v.names = 'val'
)
# extract total count for ordering
# descending by total patients counts, break ties by soc name 
c3 <- c3[order(-as.integer(sub(pattern = '(^\\d+)(\\s.*)'
               , replacement = '\\1', c3$`val.All Patients`))
               , c3$AEBODSYS),]
c3$ord <- 1:nrow(c3)
c3$ordPT <- 0

AnyGradeSoc <- cbind(c3[1], '- Any Grade -', c3[2:(ncol(c3))])
######################

# pt - any grade ----------------------------------------------------------
d1 <- aggregate(USUBJID ~ TRT02AN + SOC_AEDECOD, data = aae
                , FUN = function(x) length(unique(x))
                , drop = FALSE)

d1 <- MergeTotalGetPercent(d1)
# this order is necessary so that reshape correctly places column and grades
d2 <- d1[order(d1$TRT02AN),]

d3 <- reshape(
  data = d2[, c("TRT02AN", "SOC_AEDECOD", "val")]
  , idvar = 'SOC_AEDECOD', timevar = 'TRT02AN'
  , direction = 'wide', v.names = 'val'
)

# split soc and term
d3$AEBODSYS <- sub(pattern = '(.+)(@)(.+)', '\\1', d3$SOC_AEDECOD)
d3$AEDECOD <- sub(pattern = '(.+)(@)(.+)', '\\3', d3$SOC_AEDECOD)

# merging order from any grade soc
d4 <- merge(d3, AnyGradeSoc[, c("AEBODSYS", "ord")])

# extract total count for ordering
d4 <- d4[order(d4$ord, -as.integer(sub(pattern = '(^\\d+)(\\s.*)'
              , replacement = '\\1', d4$`val.All Patients`))
              , d4$AEDECOD),]
d4$ordPT <- 1:nrow(d4)
d4 <- d4[, !names(d4) %in% c("AEBODSYS", "SOC_AEDECOD")]
d5 <- d4[, c("AEDECOD", names(d4)[!names(d4) %in% c("AEDECOD")])]


AnyGradePT <- cbind(d5[1], '- Any Grade -', d5[2:(ncol(d5))])

######################

# grade rows by soc -------------------------------------------------------
x1 <- aggregate(AETOXGR ~ TRT02AN + AEBODSYS + USUBJID, data = aae, FUN = max, drop = TRUE)
x2 <- aggregate(USUBJID ~ TRT02AN + AEBODSYS + AETOXGR, data = x1, FUN = length, drop = FALSE)
x2 <- MergeTotalGetPercent(x2)
# this order is necessary so that reshape correctly places column and grades
x2 <- x2[order(x2$TRT02AN, x2$AETOXGR),]

x3 <- reshape(
  data = x2[, c("TRT02AN", "AEBODSYS", "AETOXGR", "val")]
  , idvar = c('AEBODSYS', 'AETOXGR'), timevar = 'TRT02AN'
  , direction = 'wide', v.names = 'val'
)

# merging order from any grad soc
x4 <- merge(x3, AnyGradeSoc[, c("AEBODSYS", "ord")])
x4 <- x4[order(x4$AEBODSYS, x4$AETOXGR),]
x4$ordPT <- 0.5
x4$AEBODSYS <- ''

SocByGrade <- x4
#############################

# grade rows by pt --------------------------------------------------------
y1 <- aggregate(AETOXGR ~ TRT02AN + SOC_AEDECOD + USUBJID, data = aae, FUN = max, drop = TRUE)
y2 <- aggregate(USUBJID ~ TRT02AN + SOC_AEDECOD + AETOXGR, data = y1, FUN = length, drop = FALSE)
y2 <- MergeTotalGetPercent(y2)
# this order is necessary so that reshape correctly places column and grades
y2 <- y2[order(y2$TRT02AN, y2$AETOXGR),]

y3 <- reshape(
  data = y2[, c("TRT02AN", "SOC_AEDECOD", "AETOXGR", "val")]
  , idvar = c('SOC_AEDECOD', 'AETOXGR'), timevar = 'TRT02AN'
  , direction = 'wide', v.names = 'val'
)

# split soc and term
y3$AEBODSYS <- sub(pattern = '(.+)(@)(.+)', '\\1', y3$SOC_AEDECOD)
y3$AEDECOD <- sub(pattern = '(.+)(@)(.+)', '\\3', y3$SOC_AEDECOD)

# merging order from any grade soc
y4 <- merge(y3, AnyGradeSoc[, c("AEBODSYS", "ord")])
# merging order from any grade pt
y4 <- merge(y4, AnyGradePT[, c("AEDECOD", "ord", "ordPT")])
y4$ordPT <- y4$ordPT + 0.5
y4 <- y4[order(y4$AEDECOD, y4$AETOXGR),]

y4 <- y4[, !names(y4) %in% c("AEBODSYS", "SOC_AEDECOD")]
y5 <- y4[, c("AEDECOD", names(y4)[!names(y4) %in% c("AEDECOD", "ord", "ordPT")], c("ord", "ordPT"))]
y5$AEDECOD <- ''

PTByGrade <- y5


names(Head) <- paste0('c', 1:length(Head))
names(AnyGrade) <- paste0('c', 1:length(AnyGrade))
names(AnyAEByGrade) <- paste0('c', 1:length(AnyAEByGrade))
names(AnyGradeSoc) <- paste0('c', 1:length(AnyGradeSoc))
names(AnyGradePT) <- paste0('c', 1:length(AnyGradePT))
names(SocByGrade) <- paste0('c', 1:length(SocByGrade))
names(PTByGrade) <- paste0('c', 1:length(PTByGrade))

tt <- rbind(Head, AnyGrade, AnyAEByGrade, AnyGradeSoc, AnyGradePT, SocByGrade, PTByGrade)
tt$c1 <- ifelse(tt$c12 > 0 & tt$c13 == 0, paste0(tt$c1, '\n- Overall -'), tt$c1)
final <- tt[order(tt$c12, tt$c13), !names(tt) %in% c('c12', 'c13')]

View(final)
#############################


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










