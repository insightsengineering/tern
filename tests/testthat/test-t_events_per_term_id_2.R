
context("test adverse events by terms (class and term)")

library(dplyr)

test_that("adverse events by terms (class and term)", {
  
  ANL <- read.table(header = TRUE, sep = ';', stringsAsFactors = FALSE, text = '
                    "USUBJID";"ARM";"AEBODSYS";"AEDECOD"
                    "SHH4429G-S19914-16100";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16100";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16100";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16100";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16100";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16100";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16101";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16102";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16103";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16103";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16103";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16103";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16103";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16103";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16104";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19914-16104";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16104";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19914-16104";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15950";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15950";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15950";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15950";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15950";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15950";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15951";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19916-15951";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19916-15952";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19917-15550";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19917-15551";"Placebo";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"
                    "SHH4429G-S19917-15552";"Active";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"
                    "SHH4429G-S19917-15554";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19918-15251";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15251";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15251";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19918-15252";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19918-15252";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19918-15252";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19918-15253";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15254";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15254";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15254";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19918-15254";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15254";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15255";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19918-15257";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19918-15257";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19920-15300";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19920-15301";"Placebo";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"
                    "SHH4429G-S19921-15150";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19921-15150";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19921-15150";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19921-15150";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19921-15150";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19921-15150";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19923-16250";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19923-16251";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19923-16252";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19926-15350";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19926-15351";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19926-15354";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19927-15503";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19927-15504";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19927-15504";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19927-15504";"Active";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19950-15600";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19950-15601";"Active";"INFECTIONS AND INFESTATIONS";"INFLUENZA"
                    "SHH4429G-S19950-15602";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19950-15602";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19950-15602";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19950-15602";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19950-15602";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19950-15603";"Active";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19950-15603";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19951-15100";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19951-15101";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19951-15101";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19951-15102";"Placebo";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"
                    "SHH4429G-S19951-15102";"Placebo";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19961-15650";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19961-15651";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19961-15651";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19961-15651";"Placebo";"GASTROINTESTINAL DISORDERS";"STOMATITIS"
                    "SHH4429G-S19961-15651";"Placebo";"GASTROINTESTINAL DISORDERS";"ABDOMINAL PAIN"
                    "SHH4429G-S19961-15652";"Placebo";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"
                    "SHH4429G-S19961-15652";"Placebo";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"
                    "SHH4429G-S19961-15653";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19961-15654";"Active";"GASTROINTESTINAL DISORDERS";"VOMITING"
                    "SHH4429G-S19961-15655";"Placebo";"INFECTIONS AND INFESTATIONS";"UPPER RESPIRATORY TRACT INFECTION"'
  )
  
  ASL <- read.table(header = TRUE, sep = ';', stringsAsFactors = FALSE, text = '
                    "USUBJID";"ARM"
                    "SHH4429G-S19914-16100";"Active"
                    "SHH4429G-S19914-16101";"Placebo"
                    "SHH4429G-S19914-16102";"Active"
                    "SHH4429G-S19914-16103";"Placebo"
                    "SHH4429G-S19914-16105";"Active"
                    "SHH4429G-S19914-16104";"Placebo"
                    "SHH4429G-S19916-15950";"Active"
                    "SHH4429G-S19916-15951";"Placebo"
                    "SHH4429G-S19916-15952";"Active"
                    "SHH4429G-S19916-15953";"Active"
                    "SHH4429G-S19916-15954";"Active"
                    "SHH4429G-S19917-15550";"Active"
                    "SHH4429G-S19917-15551";"Placebo"
                    "SHH4429G-S19917-15552";"Active"
                    "SHH4429G-S19917-15553";"Active"
                    "SHH4429G-S19917-15554";"Active"
                    "SHH4429G-S19918-15250";"Placebo"
                    "SHH4429G-S19918-15251";"Active"
                    "SHH4429G-S19918-15252";"Active"
                    "SHH4429G-S19918-15253";"Placebo"
                    "SHH4429G-S19918-15254";"Active"
                    "SHH4429G-S19918-15255";"Active"
                    "SHH4429G-S19918-15256";"Active"
                    "SHH4429G-S19918-15257";"Placebo"
                    "SHH4429G-S19920-15300";"Active"
                    "SHH4429G-S19920-15301";"Placebo"
                    "SHH4429G-S19920-15302";"Placebo"
                    "SHH4429G-S19920-15303";"Placebo"
                    "SHH4429G-S19921-15150";"Placebo"
                    "SHH4429G-S19923-16250";"Placebo"
                    "SHH4429G-S19923-16251";"Active"
                    "SHH4429G-S19923-16252";"Active"
                    "SHH4429G-S19923-16253";"Placebo"
                    "SHH4429G-S19923-16254";"Placebo"
                    "SHH4429G-S19926-15350";"Placebo"
                    "SHH4429G-S19926-15351";"Active"
                    "SHH4429G-S19926-15352";"Active"
                    "SHH4429G-S19926-15353";"Active"
                    "SHH4429G-S19926-15354";"Placebo"
                    "SHH4429G-S19927-15500";"Active"
                    "SHH4429G-S19927-15501";"Placebo"
                    "SHH4429G-S19927-15502";"Active"
                    "SHH4429G-S19927-15503";"Placebo"
                    "SHH4429G-S19927-15504";"Active"
                    "SHH4429G-S19950-15600";"Placebo"
                    "SHH4429G-S19950-15601";"Active"
                    "SHH4429G-S19950-15602";"Active"
                    "SHH4429G-S19950-15603";"Active"
                    "SHH4429G-S19951-15100";"Active"
                    "SHH4429G-S19951-15101";"Placebo"
                    "SHH4429G-S19951-15102";"Placebo"
                    "SHH4429G-S19951-15103";"Placebo"
                    "SHH4429G-S19951-15104";"Placebo"
                    "SHH4429G-S19951-15105";"Placebo"
                    "SHH4429G-S19961-15650";"Active"
                    "SHH4429G-S19961-15651";"Placebo"
                    "SHH4429G-S19961-15652";"Placebo"
                    "SHH4429G-S19961-15653";"Active"
                    "SHH4429G-S19961-15654";"Active"
                    "SHH4429G-S19961-15655";"Placebo"'
  )
  
  tbl_stream <- rtable(
    header = rheader(
      rrowl("", c('Active', 'Placebo', 'All Patients')),
      rrowl("", c('(N=32)', '(N=28)', '(N=60)'))
    ),
    rrow('- Any event -'),
    rrow('Total number of patients with at least one adverse event', rcell(c(23, .719), 'xx (xx.x%)'), rcell(c(19, .679), 'xx (xx.x%)'), rcell(c(42, .700), 'xx (xx.x%)')),
    rrow('Overall total number of events', rcell(c(58)), rcell(c(40)), rcell(c(98))),
    rrow(),
    rrow('GASTROINTESTINAL DISORDERS'),
    rrow('Total number of patients with at least one adverse event', rcell(c(21, .656), 'xx (xx.x%)'), rcell(c(15, .536), 'xx (xx.x%)'), rcell(c(36, .600), 'xx (xx.x%)'), indent = 1),
    rrow('Total number of events', rcell(c(56)), rcell(c(34)), rcell(c(90)), indent = 1),
    rrow('VOMITING', rcell(c(17, .531), 'xx (xx.x%)'), rcell(c(8, .286), 'xx (xx.x%)'), rcell(c(25, .417), 'xx (xx.x%)'), indent = 1),
    rrow('ABNOMINAL PAIN', rcell(c(6, .188), 'xx (xx.x%)'), rcell(c(5, .179), 'xx (xx.x%)'), rcell(c(11, .183), 'xx (xx.x%)'), indent = 1),
    rrow('STOMATITIS', rcell(c(5, .156), 'xx (xx.x%)'), rcell(c(6, .214), 'xx (xx.x%)'), rcell(c(11, .183), 'xx (xx.x%)'), indent = 1),
    rrow(),
    rrow('INFECTIONS AND INFESTATIONS'),
    rrow('Total number of patients with at least one adverse event', rcell(c(2, .063), 'xx (xx.x%)'), rcell(c(5, .179), 'xx (xx.x%)'), rcell(c(7, .117), 'xx (xx.x%)'), indent = 1),
    rrow('Total number of events', rcell(c(2)), rcell(c(6)), rcell(c(8)), indent = 1),
    rrow('UPPER RESPIRATORY TRACT INFECTION', rcell(c(1, .031), 'xx (xx.x%)'), rcell(c(5, .179), 'xx (xx.x%)'), rcell(c(6, .100), 'xx (xx.x%)'), indent = 1),
    rrow('INFLUENZA', rcell(c(1, .031), 'xx (xx.x%)'), rcell(0), rcell(c(1, .017), 'xx (xx.x%)'), indent = 1)
    
  )
  
  attr(attr(tbl_stream, "header")[[1]], "row.name") <- 'MedDRA System Organ Class'
  attr(attr(tbl_stream, "header")[[2]], "row.name") <- 'MedDRA Preferred Term'
  attr(attr(tbl_stream, "header")[[2]], "indent") <- 1
  
  ANL$AEBODSYS[ANL$AEBODSYS == ""] <- NA
  ANL$AEDECOD[ANL$AEDECOD == ""] <- NA
  ANL <- ANL %>%
    var_relabel(
      AEBODSYS = 'MedDRA System Organ Class',
      AEDECOD = 'MedDRA Preferred Term')
  
  tbl <- t_events_per_term_id(terms = ANL[, c("AEBODSYS", "AEDECOD")],
                              id = ANL$USUBJID,
                              col_by = as.factor(ANL$ARM),
                              col_N = table(ASL$ARM),
                              total = "All Patients"
  )
  
  comp <- compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
  
  expect_true(all(comp == "."), "t_events_per_term_id does not provide the same results as stream")
  
})
