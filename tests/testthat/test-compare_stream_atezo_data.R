
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
  
  test_that("demographic table", {
    
    ASL <- asl(com.roche.cdt30019.go29436.re)
    tbl_stream <- get_demographic_table(com.roche.cdt30019.go29436.re)
    
    ASL_f <- ASL %>%
      select(ARM, ITTWTFL, SEX, MLIVER, TCICLVL2, BAGE, AGE4CAT, ETHNIC,
             RACE, BWT, TOBHX, HIST, EGFRMUT, ALKMUT,KRASMUT, BECOG) %>%
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
      ) %>%
      filter(ITTWTFL == 'Y') 
    
    ASL_f <- var_relabel(ASL_f,
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
    # var_labels(ASL_f)
    
    tbl <- t_summarize_variables(
      data = ASL_f[, -c(1:2)],
      col_by = ASL_f$ARM,
      total = "All Patients",
      drop_levels = TRUE,
      useNA_factors = 'no'
    )
    
    # Viewer(tbl, tbl_stream)
    comp <- compare_rtables(tbl, tbl_stream, tol = .1, comp.attr = FALSE)
    
    expect_true(all(comp == "."), "demographic table does not match")

    
  })
  
  
  
  
  test_that("response table", {
    
    
  })
  
}
