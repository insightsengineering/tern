context("test adverse events by terms (class and term)")

library(dplyr)

test_that("adverse events by terms (class and term)", {

  anl <- read.table(header = TRUE, sep = ";", stringsAsFactors = FALSE, text = '
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
  anl$ARM <- factor(anl$ARM, levels = c("Active", "Placebo")) # nolint

  asl <- read.table(header = TRUE, sep = ";", stringsAsFactors = FALSE, text = '
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
  asl$ARM <- factor(asl$ARM, levels = c("Active", "Placebo")) # nolint

  # nolint start
  tbl_stream <- rtable(
    header = rheader(rrowl("", c("Active", "Placebo", "All Patients")), rrowl("", c("(N=32)", "(N=28)", "(N=60)"))),
    rrow("Total number of patients with at least one adverse event", rcell(c(23, .7188), "xx (xx.x%)"), rcell(c(19, .6786), "xx (xx.x%)"), rcell(c(42, .7000), "xx (xx.x%)")),
    rrow("Overall total number of events", rcell(c(58)), rcell(c(40)), rcell(c(98))),
    rrow(),
    rrow("GASTROINTESTINAL DISORDERS"),
    rrow("Total number of patients with at least one adverse event", rcell(c(21, .6562), "xx (xx.x%)"), rcell(c(15, .5357), "xx (xx.x%)"), rcell(c(36, .6000), "xx (xx.x%)"), indent = 1),
    rrow("Total number of events", rcell(c(56)), rcell(c(34)), rcell(c(90)), indent = 1),
    rrow("VOMITING", rcell(c(17, .5312), "xx (xx.x%)"), rcell(c(8, .2857), "xx (xx.x%)"), rcell(c(25, .4167), "xx (xx.x%)"), indent = 1),
    rrow("ABNOMINAL PAIN", rcell(c(6, .188), "xx (xx.x%)"), rcell(c(5, .179), "xx (xx.x%)"), rcell(c(11, .183), "xx (xx.x%)"), indent = 1),
    rrow("STOMATITIS", rcell(c(5, .1562), "xx (xx.x%)"), rcell(c(6, .2143), "xx (xx.x%)"), rcell(c(11, .1833), "xx (xx.x%)"), indent = 1),
    rrow(),
    rrow("INFECTIONS AND INFESTATIONS"),
    rrow("Total number of patients with at least one adverse event", rcell(c(2, .0625), "xx (xx.x%)"), rcell(c(5, .1786), "xx (xx.x%)"), rcell(c(7, .1167), "xx (xx.x%)"), indent = 1),
    rrow("Total number of events", rcell(c(2)), rcell(c(6)), rcell(c(8)), indent = 1),
    rrow("UPPER RESPIRATORY TRACT INFECTION", rcell(c(1, .0312), "xx (xx.x%)"), rcell(c(5, .1786), "xx (xx.x%)"), rcell(c(6, .1000), "xx (xx.x%)"), indent = 1),
    rrow("INFLUENZA", rcell(c(1, .0312), "xx (xx.x%)"), rcell(0), rcell(c(1, .0167), "xx (xx.x%)"), indent = 1)
  )
  # nolint end

  attr(attr(tbl_stream, "header")[[1]], "row.name") <- "MedDRA System Organ Class"
  attr(attr(tbl_stream, "header")[[2]], "row.name") <- "MedDRA Preferred Term"
  attr(attr(tbl_stream, "header")[[2]], "indent") <- 1

  anl$AEBODSYS[anl$AEBODSYS == ""] <- NA
  anl$AEDECOD[anl$AEDECOD == ""] <- NA
  anl <- anl %>%
    var_relabel(
      AEBODSYS = "MedDRA System Organ Class",
      AEDECOD = "MedDRA Preferred Term"
    )

  tbl <- t_events_per_term_id(
    terms = anl[, c("AEBODSYS", "AEDECOD")] %>% map(as_factor_keep_attributes),
    id = anl$USUBJID,
    col_by = anl$ARM,
    total = "All Patients",
    col_N = table(asl$ARM)
  )

  comp <- compare_rtables(tbl, tbl_stream, comp.attr = FALSE)

  expect_true(all(comp == "."), "t_events_per_term_id does not provide the same results as stream")

})
