# Test the all variant for AET01 together, including:
# 1) Total number of events satisfying certain criteria.
# 2) Total number of patients with at least one event satisfying certain event criteria.

# Shortcut function.
aet01_count_patients_with_event <- function(lyt, filters, label, indentation = 0L) { #nolint
  count_patients_with_event(lyt,
                            vars = "SUBJID",
                            filters = filters,
                            denom = "N_col",
                            .labels = c(count_fraction = label),
                            .indent_mods = c(count_fraction = indentation)
  )
}

test_that("Test aet01", {
  library(tern)
  library(dplyr)
  library(random.cdisc.data)
  set.seed(99)
  adsl <- radsl(cached = TRUE) %>%
    mutate(
      DCSREAS = as.character(DCSREAS)  # nolint
    )
  adae <- radae(cached = TRUE)
  adsl <- adsl %>%
    mutate(
      DTHFL = case_when(  # nolint
        !is.na(DTHDT) ~ "Y",
        TRUE ~ ""
      ),
      DCSREAS = case_when(  # nolint
        is.na(DCSREAS) ~ "",
        TRUE ~ DCSREAS
      )
    ) %>%
    var_relabel(
      DTHFL = "Subject Death Flag",
      DCSREAS = "Reason for Discontinuation from Study"
    )

  # Create dummy AAG.
  AAG <- tibble(  # nolint
    NAMVAR = c(rep("SMQ01NAM", 2), rep("SMQ02NAM", 2), rep("CQ01NAM", 2)),
    SRCVAR = rep("AEDECOD", 6),
    GRPTYPE = c(rep("SMQ", 4), rep("CUSTOM", 2)),
    CUSTYPE = c(rep("", 4), rep("AEGT", 2)),
    SCOPE = c(rep("BROAD", 2), rep("NARROW", 2), rep("", 2)),
    REFNAME = c(
      rep("C.1.1.1.3/B.2.2.3.1 AESI", 2),
      rep("Y.9.9.9.9/Z.9.9.9.9 AESI", 2),
      rep("D.2.1.5.3/A.1.1.1.1 AESI", 2)
    ), #nolint
    REFTERM = c(
      "dcd C.1.1.1.3", "dcd B.2.2.3.1", "dcd Y.9.9.9.9",
      "dcd Z.9.9.9.9", "dcd D.2.1.5.3", "dcd A.1.1.1.1"
      )
    ) %>%
    var_relabel(
      NAMVAR = "Name of Grouping Flag Variable",
      SRCVAR = "Variable on which Grouping is Based",
      GRPTYPE = "Grouping Definition Type",
      CUSTYPE = "Custom Definition Type",
      SCOPE = "Scope of the query",
      REFNAME = "Grouping Definition Name",
      REFTERM = "Grouping Def. Clause Dictionary Term"
    )

  # Helper function to extract group label from AAG.
  aegrp_label <- function(aegrp, varname){

    stopifnot(c("NAMVAR", "GRPTYPE", "SCOPE", "REFNAME") %in% names(aegrp))
    stopifnot(varname %in% unique(aegrp[["NAMVAR"]]))

    lbl_df <- aegrp[aegrp$NAMVAR == varname, c("GRPTYPE", "SCOPE", "REFNAME")][1, ]

    lbl <- if (lbl_df$GRPTYPE == "SMQ") {
      paste0(lbl_df$REFNAME, " (", lbl_df$GRPTYPE, ") ", " (", tolower(lbl_df$SCOPE), ")")
    } else{
      paste(lbl_df$REFNAME)
    }

    lbl
  }

  # Add additional variables to ADAE.
  adae <- adae %>%
    mutate( # nolint
      AESDTH = sample(c("N", "Y"), size = nrow(adae), replace = TRUE, prob = c(0.99, 0.01)),
      AEACN = sample(
        c("DOSE NOT CHANGED", "DOSE INCREASED", "DRUG INTERRUPTED", "DRUG WITHDRAWN"),
        size = nrow(adae),
        replace = TRUE,
        prob = c(0.68, 0.02, 0.25, 0.05)
      ),
      SMQ01NAM = case_when(
        AEDECOD %in% c("dcd C.1.1.1.3", "dcd B.2.2.3.1") ~ "C.1.1.1.3/B.2.2.3.1 AESI",
        TRUE ~ ""
      ),
      SMQ02NAM = case_when(
        AEDECOD %in% c("dcd Y.9.9.9.9", "dcd Z.9.9.9.9") ~ "Y.9.9.9.9/Z.9.9.9.9 AESI",
        TRUE ~ ""
      ),
      CQ01NAM = case_when(
        AEDECOD %in% c("dcd D.2.1.5.3", "dcd A.1.1.1.1") ~ "D.2.1.5.3/A.1.1.1.1 AESI",
        TRUE ~ ""
      )
    ) %>%
    var_relabel(
      AESDTH = "Results in Death",
      AEACN = "Action Taken with Study Treatment",
      SMQ01NAM = "SMQ 01 Name",
      SMQ02NAM = "SMQ 02 Name",
      CQ01NAM = "Customized Query 01 Name"
    )

  adae <- adae[, -which(colnames(adae) == "ARM")]

  death <- data.frame(SUBJID = adsl$SUBJID, DTHFL = adsl$DTHFL, ARM = adsl$ARM, stringsAsFactors = FALSE)

  adae <- full_join(adae, death, by = "SUBJID")
  adae$AETERM <- as.character(adae$AETERM) # nolint

  adae$TREATMOD <- NA # nolint
  adae$TREATWITHDRAWL <- NA # nolint
  adae$GRADE3_5 <- NA # nolint

  adae$TREATMOD[adae$AEACN %in% c("DOSE REDUCED", "DOSE INCREASED", "DRUG INTERRUPTED")] <- "Y"
  adae$TREATWITHDRAWL[adae$AEACN %in% c("DRUG WITHDRAWN")] <- "Y"
  adae$GRADE3_5[adae$AETOXGR %in% c("3", "4", "5")] <- "Y"


  l <- split_cols_by(lyt = NULL, var = "ARM") %>%
    add_colcounts() %>%
    aet01_count_patients_with_event(
      c("STUDYID" = "AB12345"),
      "Total number of patients with at least one adverse event"
    ) %>%
    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total AEs")
    ) %>%
    aet01_count_patients_with_event(
      c("DTHFL" = "Y"),
      "Total number deaths"
    ) %>%
    aet01_count_patients_with_event(
      c("DCSREAS" = "ADVERSE EVENT"),
      "Total number of patients withdrawn from study due to an AE"
    ) %>%
    aet01_count_patients_with_event(
      c("AESDTH" = "Y"),
      "AE with fatal outcome",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("AESER" = "Y"),
      "Serious AE",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("TREATWITHDRAWL" = "Y", "AESER" = "Y"),
      "Serious AE leading to withdrawal from treatment",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("TREATMOD" = "Y", "AESER" = "Y"),
      "Serious AE leading to dose modification/interruption",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("AEREL" = "Y", "AESER" = "Y"),
      "Related Serious AE",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("TREATWITHDRAWL" = "Y"),
      "AE leading to withdrawal from treatment",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("TREATMOD" = "Y"),
      "AE leading to dose modification/interruption",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("AEREL" = "Y"),
      "Related AE",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("TREATWITHDRAWL" = "Y", "AEREL" = "Y"),
      "Related AE leading to withdrawal from treatment",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("TREATMOD" = "Y", "AEREL" = "Y"),
      "Related AE leading to dose modification/interruption",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("GRADE3_5" = "Y"),
      "Grade 3-5 AE",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("SMQ01NAM" = "C.1.1.1.3/B.2.2.3.1 AESI"),
      "C.1.1.1.3/B.2.2.3.1 AESI (SMQ)  (broad)",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("SMQ02NAM" = "C.1.1.1.3/B.2.2.3.1 AESI"),
      "Y.9.9.9.9/Z.9.9.9.9 AESI (SMQ)  (narrow)",
      2L
    ) %>%
    aet01_count_patients_with_event(
      c("CQ01NAM" = "D.2.1.5.3/A.1.1.1.1 AESI"),
      "D.2.1.5.3/A.1.1.1.1 AESI",
      2L
    ) %>%
    count_patients_with_event(
      vars = "AETERM",
      filters = c("AESER" = "Y"),
      denom = "N_col",
      .stats = "count",
      .labels = c(count = "Serious"),
      .indent_mods = c(count = 2L),
      .formats = c(count = "xx")
    ) %>%
    count_patients_with_event(
      vars = "AETERM",
      filters = c("AEREL" = "Y"),
      denom = "N_col",
      .stats = "count",
      .labels = c(count = "Related"),
      .indent_mods = c(count = 2L),
      .formats = c(count = "xx")
    ) %>%
    count_values(
      "AESER",
      values = "Y",
      .stats = "count",
      .labels = c(count = "Serious"),
      .indent_mods = c(count = 2L)
    )

  result <- build_table(l, adae, col_count = table(adsl$ARM))
  result <- insert_rrow(result, rrow("Total number of patients with at least one", "", "", ""), at = 5)
  result <- insert_rrow(result, rrow("Medical concepts: patients with", "", "", ""), at = 16)
  result <- insert_rrow(result, rrow("Total number of unique preferred terms which are", "", "", ""), at = 21)
  result <- insert_rrow(result, rrow("Total number of adverse events which are", "", "", ""), at = 24)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total AEs", "Total number deaths", "Total number of patients withdrawn from study due to an AE",
      "Total number of patients with at least one", "  AE with fatal outcome",
      "  Serious AE", "  Serious AE leading to withdrawal from treatment",
      "  Serious AE leading to dose modification/interruption", "  Related Serious AE",
      "  AE leading to withdrawal from treatment", "  AE leading to dose modification/interruption",
      "  Related AE", "  Related AE leading to withdrawal from treatment",
      "  Related AE leading to dose modification/interruption", "Medical concepts: patients with",
      "  Grade 3-5 AE", "  C.1.1.1.3/B.2.2.3.1 AESI (SMQ)  (broad)",
      "  Y.9.9.9.9/Z.9.9.9.9 AESI (SMQ)  (narrow)", "  D.2.1.5.3/A.1.1.1.1 AESI",
      "Total number of unique preferred terms which are", "  Serious",
      "  Related", "Total number of adverse events which are", "  Serious",
      "A: Drug X", "(N=134)", "122 (91.04%)", "609", "107 (79.85%)",
      "6 (4.48%)", "", "6 (4.48%)", "104 (77.61%)", "6 (4.48%)", "43 (32.09%)",
      "76 (56.72%)", "25 (18.66%)", "79 (58.96%)", "105 (78.36%)",
      "14 (10.45%)", "56 (41.79%)", "", "109 (81.34%)", "72 (53.73%)",
      "0 (0%)", "74 (55.22%)", "", "4", "5", "", "249", "B: Placebo",
      "(N=134)", "123 (91.79%)", "622", "112 (83.58%)", "5 (3.73%)",
      "", "9 (6.72%)", "101 (75.37%)", "16 (11.94%)", "48 (35.82%)",
      "70 (52.24%)", "31 (23.13%)", "89 (66.42%)", "108 (80.6%)", "14 (10.45%)",
      "56 (41.79%)", "", "104 (77.61%)", "79 (58.96%)", "0 (0%)", "80 (59.7%)",
      "", "4", "5", "", "255", "C: Combination", "(N=132)", "120 (90.91%)",
      "703", "111 (84.09%)", "6 (4.55%)", "", "6 (4.55%)", "99 (75%)",
      "11 (8.33%)", "52 (39.39%)", "75 (56.82%)", "31 (23.48%)", "90 (68.18%)",
      "109 (82.58%)", "15 (11.36%)", "60 (45.45%)", "", "109 (82.58%)",
      "75 (56.82%)", "0 (0%)", "87 (65.91%)", "", "4", "5", "", "282"
    ),
    .Dim = c(27L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})
