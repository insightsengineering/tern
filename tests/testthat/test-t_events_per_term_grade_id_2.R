context("test adverse events sorted by highest NCI CTCAE grade (class and term)")

library(dplyr)

test_that("adverse events sorted by highest NCI CTCAE grade (class and term)", {

  anl <- read.table(header = TRUE, sep = ";", stringsAsFactors = FALSE, text = '
                    "USUBJID";"TRT02AN";"AETOXGR";"AEBODSYS";"AEDECOD"
                    "AB12345-266536-2014";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266536-2014";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266536-2014";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266536-2014";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266539-1033";"A";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266539-2011";"B";"";"";""
                    "AB12345-266539-2024";"B";"";"";""
                    "AB12345-266539-2026";"B";"";"";""
                    "AB12345-266539-2509";"B";"";"";""
                    "AB12345-266541-2000";"B";"";"";""
                    "AB12345-266541-2001";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2001";"B";"2";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-266541-2001";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2001";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266541-2001";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-266541-2002";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2005";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2005";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-266541-2017";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2017";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2020";"B";"";"";""
                    "AB12345-266541-2021";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266541-2025";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266541-2025";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266542-1030";"A";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266542-1034";"A";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266542-1034";"A";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-266542-1035";"A";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266542-1035";"A";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-266542-1035";"A";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266542-1035";"A";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-266542-2006";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-266542-2028";"B";"";"";""
                    "AB12345-266542-2029";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266542-2517";"B";"2";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-266592-2038";"B";"";"";""
                    "AB12345-266592-2503";"B";"";"";""
                    "AB12345-266912-1031";"A";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-266912-1032";"A";"";"";""
                    "AB12345-266912-2022";"B";"";"";""
                    "AB12345-266912-2023";"B";"";"";""
                    "AB12345-266912-2500";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-267159-2504";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-267159-2504";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-267159-2504";"B";"3";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-267159-2504";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-267159-2518";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-267161-2013";"B";"";"";""
                    "AB12345-268054-2513";"B";"3";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-268054-2513";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268054-2513";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268136-2015";"B";"";"";""
                    "AB12345-268136-2506";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-268136-2506";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-268136-2507";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268136-2507";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268137-2009";"B";"";"";""
                    "AB12345-268137-2034";"B";"";"";""
                    "AB12345-268137-2501";"B";"";"";""
                    "AB12345-268137-2505";"B";"";"";""
                    "AB12345-268137-2508";"B";"";"";""
                    "AB12345-268141-2007";"B";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268141-2019";"B";"";"";""
                    "AB12345-268141-2511";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-268141-2511";"B";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268141-2519";"B";"2";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-268141-2519";"B";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268141-2519";"B";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268142-2012";"B";"";"";""
                    "AB12345-268142-2016";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-268142-2018";"B";"2";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-268142-2018";"B";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268142-2018";"B";"2";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268142-2018";"B";"1";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-268142-2033";"B";"3";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-268142-2036";"B";"";"";""
                    "AB12345-268142-2037";"B";"";"";""
                    "AB12345-268142-2039";"B";"";"";""
                    "AB12345-268142-2516";"B";"";"";""
                    "AB12345-268144-2004";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268144-2008";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-268144-2008";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268144-2010";"B";"3";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-268144-2010";"B";"1";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-268144-2030";"B";"";"";""
                    "AB12345-268144-2031";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-268144-2032";"B";"2";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-268144-2510";"B";"5";"GASTROINTESTINAL DISORDERS";"SMALL INTESTINAL OBSTRUCTION"
                    "AB12345-268248-2512";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"RASH MACULO-PAPULAR"
                    "AB12345-272188-2003";"B";"1";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-272188-2003";"B";"2";"GASTROINTESTINAL DISORDERS";"DYSPEPSIA"
                    "AB12345-272188-2003";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-272188-2003";"B";"2";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"
                    "AB12345-272188-2514";"B";"2";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-272188-2515";"B";"";"";""
                    "AB12345-275992-2502";"B";"";"";""
                    "AB12345-275992-2520";"B";"1";"GASTROINTESTINAL DISORDERS";"CONSTIPATION"
                    "AB12345-275992-2520";"B";"3";"GASTROINTESTINAL DISORDERS";"ASCITES"
                    "AB12345-275992-2520";"B";"1";"SKIN AND SUBCUTANEOUS TISSUE DISORDERS";"DERMATITIS ACNEIFORM"'
  )

  asl <- unique(anl[, c("USUBJID", "TRT02AN")])

  # nolint start
  tbl_stream <- rtable(
    header = rheader(
      rrowl("", "", c("A", "B", "All Patients")),
      rrowl("", "NCI CTCAE Grade", c("(N=6)", "(N=59)", "(N=65)"))
    ),
    rrow("- Any adverse events -"),
    rrow("- Overall -", "- Any Grade -",
         rcell(c(5, .833), "xx (xx.x%)"), rcell(c(32, .542), "xx (xx.x%)"), rcell(c(37, .569), "xx (xx.x%)"),
         indent = 1),
    rrow("", "1",
         rcell(c(4, .667), "xx (xx.x%)"),  rcell(c(14, .237), "xx (xx.x%)"),  rcell(c(18, .277), "xx (xx.x%)")),
    rrow("", "2",
         rcell(c(1, .167), "xx (xx.x%)"),  rcell(c(12, .203), "xx (xx.x%)"),  rcell(c(13, .200), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)),  rcell(c(5, .085), "xx (xx.x%)"),  rcell(c(5, .077), "xx (xx.x%)")),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(1, .017), "xx (xx.x%)"),  rcell(c(1, .015), "xx (xx.x%)")),
    rrow(),
    rrow("SKIN AND SUBCUTANEOUS TISSUE DISORDERS"),
    rrow("- Overall -", "- Any Grade -",
         rcell(c(5, .833), "xx (xx.x%)"), rcell(c(19, .322), "xx (xx.x%)"), rcell(c(24, .369), "xx (xx.x%)"),
         indent = 1),
    rrow("", "1", rcell(c(4, .667), "xx (xx.x%)"),  rcell(c(9, .153), "xx (xx.x%)"),  rcell(c(13, .200), "xx (xx.x%)")),
    rrow("", "2", rcell(c(1, .167), "xx (xx.x%)"),  rcell(c(8, .136), "xx (xx.x%)"),  rcell(c(9, .138), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)),  rcell(c(2, .034), "xx (xx.x%)"),  rcell(c(2, .031), "xx (xx.x%)")),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow(),
    rrow("DERMATITIS ACNEIFORM", "- Any Grade -",
         rcell(c(1, .167), "xx (xx.x%)"), rcell(c(15, .254), "xx (xx.x%)"), rcell(c(16, .246), "xx (xx.x%)"),
         indent = 1),
    rrow("", "1", rcell(c(1, .167), "xx (xx.x%)"),  rcell(c(8, .136), "xx (xx.x%)"),  rcell(c(9, .138), "xx (xx.x%)")),
    rrow("", "2", rcell(c(0)),  rcell(c(7, .119), "xx (xx.x%)"),  rcell(c(7, .108), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow(),
    rrow("RASH MACULO-PAPULAR", "- Any Grade -",
         rcell(c(4, .667), "xx (xx.x%)"), rcell(c(8, .136), "xx (xx.x%)"), rcell(c(12, .185), "xx (xx.x%)"),
         indent = 1),
    rrow("", "1", rcell(c(3, .500), "xx (xx.x%)"),  rcell(c(4, .068), "xx (xx.x%)"),  rcell(c(7, .108), "xx (xx.x%)")),
    rrow("", "2", rcell(c(1, .167), "xx (xx.x%)"),  rcell(c(2, .034), "xx (xx.x%)"),  rcell(c(3, .046), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)), rcell(c(2, .034), "xx (xx.x%)"), rcell(c(2, .031), "xx (xx.x%)")),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow(),
    rrow("GASTROINTESTINAL DISORDERS"),
    rrow("- Overall -", "- Any Grade -",
         rcell(c(2, .333), "xx (xx.x%)"), rcell(c(19, .322), "xx (xx.x%)"), rcell(c(21, .323), "xx (xx.x%)"),
         indent = 1),
    rrow("", "1", rcell(c(2, .333), "xx (xx.x%)"),  rcell(c(8, .136), "xx (xx.x%)"),  rcell(c(10, .154), "xx (xx.x%)")),
    rrow("", "2", rcell(c(0)),  rcell(c(7, .119), "xx (xx.x%)"),  rcell(c(7, .108), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)),  rcell(c(3, .051), "xx (xx.x%)"),  rcell(c(3, .046), "xx (xx.x%)")),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(1, .017), "xx (xx.x%)"), rcell(c(1, .015), "xx (xx.x%)")),
    rrow(),
    rrow("CONSTIPATION", "- Any Grade -",
         rcell(c(1, .167), "xx (xx.x%)"), rcell(c(11, .186), "xx (xx.x%)"), rcell(c(12, .185), "xx (xx.x%)"),
         indent = 1),
    rrow("", "1", rcell(c(1, .167), "xx (xx.x%)"),  rcell(c(7, .119), "xx (xx.x%)"),  rcell(c(8, .123), "xx (xx.x%)")),
    rrow("", "2", rcell(c(0)), rcell(c(4, .068), "xx (xx.x%)"), rcell(c(4, .062), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow(),
    rrow("ASCITES", "- Any Grade -",
         rcell(c(0)), rcell(c(7, .119), "xx (xx.x%)"), rcell(c(7, .108), "xx (xx.x%)"), indent = 1),
    rrow("", "1", rcell(c(0)), rcell(c(2, .034), "xx (xx.x%)"), rcell(c(2, .031), "xx (xx.x%)")),
    rrow("", "2", rcell(c(0)), rcell(c(2, .034), "xx (xx.x%)"), rcell(c(2, .031), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)), rcell(c(3, .051), "xx (xx.x%)"), rcell(c(3, .046), "xx (xx.x%)")),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow(),
    rrow("DYSPEPSIA", "- Any Grade -",
         rcell(c(2, .333), "xx (xx.x%)"), rcell(c(5, .085), "xx (xx.x%)"), rcell(c(7, .108), "xx (xx.x%)"), indent = 1),
    rrow("", "1", rcell(c(2, .333), "xx (xx.x%)"), rcell(c(3, .051), "xx (xx.x%)"), rcell(c(5, .077), "xx (xx.x%)")),
    rrow("", "2", rcell(c(0)), rcell(c(2, .034), "xx (xx.x%)"), rcell(c(2, .031), "xx (xx.x%)")),
    rrow("", "3", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow(),
    rrow("SMALL INTESTINAL OBSTRUCTION", "- Any Grade -",
         rcell(c(0)), rcell(c(1, .017), "xx (xx.x%)"), rcell(c(1, .015), "xx (xx.x%)"), indent = 1),
    rrow("", "1", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "2", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "3", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "4", rcell(c(0)), rcell(c(0)), rcell(c(0))),
    rrow("", "5", rcell(c(0)), rcell(c(1, .017), "xx (xx.x%)"), rcell(c(1, .015), "xx (xx.x%)"))
  )
  # nolint end

  attr(attr(tbl_stream, "header")[[1]], "row.name") <- "MedDRA System Organ Class"
  attr(attr(tbl_stream, "header")[[2]], "row.name") <- "MedDRA Preferred Term"
  attr(attr(tbl_stream, "header")[[2]], "indent") <- 1

  anl$AEBODSYS[anl$AEBODSYS == ""] <- NA
  anl$AEDECOD[anl$AEDECOD == ""] <- NA
  anl <- anl %>%
    filter(!is.na(AETOXGR)) %>%
    var_relabel(
      AEBODSYS = "MedDRA System Organ Class",
      AEDECOD = "MedDRA Preferred Term",
      AETOXGR = "NCI CTCAE Grade")

  tbl <- t_events_per_term_grade_id(terms = anl %>% select(AEBODSYS, AEDECOD) %>% map(as_factor_keep_attributes),
                                    id = anl$USUBJID,
                                    grade = anl$AETOXGR,
                                    col_by = as.factor(anl$TRT02AN) %>% by_add_total("All Patients"),
                                    col_N = col_N_add_total(table(asl$TRT02AN)),
                                    grade_levels = 1:5)

  comp <- compare_rtables(tbl, tbl_stream, comp.attr = FALSE)

  expect_true(all(comp == "."), "t_events_per_term_grade_id does not provide the same results as stream")

})
