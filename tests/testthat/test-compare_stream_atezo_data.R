
if (require("atezo.data", quietly = TRUE)) {

  context("compare stream outputs for atezo data in the atezo.data R package")
  
  require("dplyr") || stop("dplyr is needed for the atezo.data tests")
  require("forcats") || stop("forcats is needed for the atezo.data tests")
  
  # for debugging run 
  # library(testthat)
  # library(tern)
  
  
  fct_rr <- function(x, ...) {
    dots <- list(...)
    x1 <- fct_recode(factor(x), ...)
    do.call(fct_relevel, c(list(x1), as.list(names(dots))))
  }
  
  # Pre - Processing (factor level order, factor labels, variable labels)
  ASL_raw <- asl(com.roche.cdt30019.go29436.re) 
  
  ASL <- ASL_raw %>%
    mutate_all(function(x) {
      if (is.factor(x)) {
        x[x == ""] <- NA
        x <- droplevels(x, "")
      } else if(is.character(x)) {
        x[x == ""] <- NA
      }
      x
    }) %>%
    mutate(
      ARM = fct_relevel(ARM, "DUMMY C", "DUMMY B", "DUMMY A"),
      SEX = fct_rr(SEX, Male = "M", Female = "F"),
      MLIVER = fct_rr(MLIVER, Yes = "Y", No = "N"),
      TCLEVEL = fct_relevel(TCLEVEL, "0", "1", "2", "3"),
      TCICLVL2 = fct_relevel(TCICLVL2, "TC3 or IC2/3", "TC0/1/2 and IC0/1"),
      AGE4CAT = fct_relevel(AGE4CAT, "<65", "65 to 74", "75 to 84", ">=85"),
      ETHNIC = fct_rr(ETHNIC, "Hispanic or Latino" ="HISPANIC OR LATINO",
                      "Not Hispanic or Latino" = "NOT HISPANIC OR LATINO",
                      "Not reported" = "NOT REPORTED",
                      "Unknown" = "UNKNOWN"),
      RACE = fct_rr(RACE,
                    "American Indian or Alaska Native" = "AMERICAN INDIAN OR ALASKA NATIVE",
                    "Asian" = "ASIAN",
                    "Black or African American" = "BLACK OR AFRICAN AMERICAN",
                    "White" = "WHITE",
                    "Multiple" = "MULTIPLE",
                    "Unknown" = "UNKNOWN"),
      TOBHX = fct_rr(TOBHX, Never = "NEVER", Current = "CURRENT", Previous = "PREVIOUS"),
      HIST = fct_rr(HIST,
                    "Adenocarcinoma" = "Adenocarcinoma",
                    "Adenocarcinoma With Neuroendocrine Features" = "Adenocarcinoma with neuroendocrine features",
                    "Adenosquamous" = "Adenosquamous" ,
                    "Bronchioloalveolar Carcinoma" = "Bronchioloalveolar carcinoma",
                    "Large Cell" = "Large cell",
                    "Sarcomatoid" = "Sarcomatoid",
                    "Undifferentiated" = "Undifferentiated",
                    "NA" = "NA",
                    "Unknown" = "Unknown"
      ),
      EGFRMUT = fct_rr(EGFRMUT, Positive = "POSITIVE", Negative = "NEGATIVE", Unknown = "UNKNOWN"),
      ALKMUT = fct_rr(ALKMUT, Positive = "POSITIVE", Negative = "NEGATIVE", Unknown = "UNKNOWN"),
      KRASMUT = fct_rr(KRASMUT, Positive = "POSITIVE", Negative = "NEGATIVE", Unknown = "UNKNOWN"),
      BECOG = fct_relevel(BECOG, "0", "1")
    ) %>%  var_relabel(
      SEX = "Sex",
      MLIVER = "Liver Metastasis at Enrollment",
      TCICLVL2 = "TC/IC Strat Factor from IxRS Group 2",
      BAGE = "Age (years)",
      AGE4CAT = "Aqe Group 4 Categories (years)",
      ETHNIC = "Ethnicity",
      RACE = "Race",
      BWT = "Weight (kg) at baseline",
      TOBHX = "Tobacco Use History",
      HIST = "Non−squamous Histology Detail",
      EGFRMUT = "EGFR Mutation Status",
      ALKMUT = "EML4−ALK Rearrangement Status",
      KRASMUT = "KRAS Mutation Status",
      BECOG = "Baseline ECOG"
    )
  
  
  ARS <- ars(com.roche.cdt30019.go29436.re) %>%
    drop_shared_variables(ASL, c("USUBJID", "STUDYID"))
  
  ATE <- ate(com.roche.cdt30019.go29436.re) %>%
    drop_shared_variables(ASL, c("USUBJID", "STUDYID"))
  
  
  test_that("demographic table", {
    
    tbl_stream <- get_demographic_table(com.roche.cdt30019.go29436.re)
    
    ASL_f <- ASL %>%
      filter(ITTWTFL == 'Y') %>%
      select(ARM, SEX, MLIVER, TCICLVL2, BAGE, AGE4CAT, ETHNIC,
             RACE, BWT, TOBHX, HIST, EGFRMUT, ALKMUT,KRASMUT, BECOG)
    # var_labels(ASL_f)
    
    tbl <- t_summarize_variables(
      data = ASL_f[, -1],
      col_by = ASL_f$ARM,
      total = "All Patients",
      drop_levels = TRUE,
      useNA_factors = 'no'
    )
    
    # Viewer(tbl, tbl_stream)
    comp <- compare_rtables(tbl, tbl_stream, tol = .1, comp.attr = FALSE)
    
    expect_true(all(comp == "."), "demographic table does not match")

    
  })
  
  
  test_that("forest response", {
    
    tbl_stream <- get_forest_response_table(com.roche.cdt30019.go29436.re)
  
    ARS_f <- ARS %>% filter(PARAMCD == "OVRSPI") %>% 
                    select(USUBJID, STUDYID, AVAL, AVALC)
  
    ASL_f <- ASL %>% 
      filter(ITTWTFL == "Y") %>% 
      filter(ARMCD %in% c("A", "C")) %>%
      select(USUBJID, STUDYID, ARM, SEX, MLIVER, ICLEVEL, TCLEVEL, TCICLVL2, TC3IC3, AGE4CAT, RACE)
    
    ANL <- left_join(ASL_f, ARS_f, by = c('USUBJID', 'STUDYID'))
    
    # names(group_data)
    
    ANL$TC3IC3[ANL$TC3IC3 == "UNKNOWN"] <- NA
    ANL$TC3IC3 <- relevel(droplevels(factor(ANL$TC3IC3)),
                          "TC3 or IC3", "0/1/2 and IC0/1/2")
    
    arm <- droplevels(fct_relevel(ANL$ARM, "DUMMY C"))
    
    tbl <- t_forest_rsp(
      rsp = ANL$AVALC %in% c("CR","PR"),
      col_by = arm,
      group_data = ANL[, c("TCLEVEL", "ICLEVEL", "TC3IC3")],
      total = "All Patients"
    )
    
    # Viewer(tbl, tbl_stream)
    
    comp <- compare_rtables(tbl, tbl_stream, tol = 0.1, comp.attr = FALSE)
    
    expect_true(all(comp == "."), "forest response table is not correct")
    
  })

  test_that("forest time to event", {
    
    tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
    
    #Viewer(tbl_stream)
    
    ASL_f <- ASL %>%
      filter(ARMCD1 %in% c("C", "A"), ITTWTFL == 'Y')
    
    ATE_f <- ATE %>%
      filter(PARAMCD == "OS")


    ANL <- merge(ASL_f, ATE_f, by = c("USUBJID", "STUDYID"))
  
    ANL$SEX <- fct_relevel(ANL$SEX, "Female")
    ANL$RACE[!(ANL$RACE %in% c("Asian", "Black or African American", "White"))] <- NA
    ANL$RACE <- droplevels(ANL$RACE)
    
    
    tbl <- t_forest_tte(
      tte = ANL$AVAL,
      is_event = !ANL$CNSR,
      col_by = droplevels(ANL$ARM),
      group_data = ANL[, c("SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE")]
    )
  
    # Viewer(tbl)
    # Viewer(tbl, tbl_stream)
    
    comp <- compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
    
    expect_true(all(comp == "."), "forest tte is not the same")
    
  })  
  
  test_that("response table", {
    
    
    
  })
  
}
