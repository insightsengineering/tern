# h_stack_by_baskets returns the correct dataframe

    Code
      res
    Output
      # A tibble: 8 x 6
        STUDYID USUBJID               ASTDTM              AEDECOD       AESEQ SMQ     
        <fct>   <fct>                 <dttm>              <fct>         <int> <fct>   
      1 AB12345 AB12345-BRA-11-id-8   2021-12-04 19:00:00 dcd D.2.1.5.3     2 D.2.1.5~
      2 AB12345 AB12345-BRA-12-id-120 2020-02-04 19:00:00 dcd D.2.1.5.3     2 D.2.1.5~
      3 AB12345 AB12345-BRA-1-id-171  2022-11-28 19:00:00 dcd C.1.1.1.3     2 C.1.1.1~
      4 AB12345 AB12345-BRA-1-id-23   2020-07-09 20:00:00 dcd B.2.2.3.1     3 C.1.1.1~
      5 AB12345 AB12345-BRA-1-id-59   2021-10-10 20:00:00 dcd C.1.1.1.3     4 C.1.1.1~
      6 AB12345 AB12345-BRA-1-id-9    2021-05-31 20:00:00 dcd C.1.1.1.3     1 C.1.1.1~
      7 AB12345 AB12345-BRA-11-id-8   2021-12-20 19:00:00 dcd C.1.1.1.3     3 C.1.1.1~
      8 AB12345 AB12345-BRA-12-id-120 2020-09-30 20:00:00 dcd C.1.1.1.3     3 C.1.1.1~

# h_stack_by_baskets returns an empty dataframe with desired variables and labels when there are no adverse events falling within any of the baskets selected

    Code
      res
    Output
      [1] 0

---

    Code
      res
    Output
      [1] "STUDYID" "USUBJID" "ASTDTM"  "AEDECOD" "AESEQ"   "SMQ"    

---

    Code
      res
    Output
                          STUDYID                     USUBJID 
                               NA                          NA 
                           ASTDTM                     AEDECOD 
                               NA                          NA 
                            AESEQ                         SMQ 
                               NA "Standardized MedDRA Query" 

---

    Code
      res
    Output
      [1] "CQ01NAM"  "SMQ01NAM" "SMQ02NAM"

# The levels of the SMQ column does also include the options from aag_summary not observed in ADAE

    Code
      res
    Output
      [1] "C.1.1.1.3/B.2.2.3.1 AESI(BROAD)" "C.1.1.1.3/B.2.2.3.1 aesi(BROAD)"
      [3] "D.2.1.5.3/A.1.1.1.1 AESI"        "D.2.1.5.3/A.1.1.1.1 aesi"       
      [5] "level1_not_in_adae"              "level2_not_in_adae"             

