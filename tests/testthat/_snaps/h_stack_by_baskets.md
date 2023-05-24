# h_stack_by_baskets returns the correct dataframe

    Code
      res
    Output
        STUDYID               USUBJID              ASTDTM       AEDECOD AESEQ
      1 AB12345   AB12345-BRA-11-id-8 2021-12-05 02:02:07 dcd D.2.1.5.3     2
      2 AB12345 AB12345-BRA-12-id-120 2020-02-05 01:42:29 dcd D.2.1.5.3     2
      3 AB12345  AB12345-BRA-1-id-171 2022-11-29 12:18:31 dcd C.1.1.1.3     2
      4 AB12345   AB12345-BRA-1-id-23 2020-07-10 07:32:49 dcd B.2.2.3.1     3
      5 AB12345   AB12345-BRA-1-id-59 2021-10-10 23:54:46 dcd C.1.1.1.3     4
      6 AB12345    AB12345-BRA-1-id-9 2021-06-01 14:39:09 dcd C.1.1.1.3     1
      7 AB12345   AB12345-BRA-11-id-8 2021-12-21 02:02:07 dcd C.1.1.1.3     3
      8 AB12345 AB12345-BRA-12-id-120 2020-10-01 01:42:29 dcd C.1.1.1.3     3
                                    SMQ
      1        D.2.1.5.3/A.1.1.1.1 aesi
      2        D.2.1.5.3/A.1.1.1.1 aesi
      3 C.1.1.1.3/B.2.2.3.1 aesi(BROAD)
      4 C.1.1.1.3/B.2.2.3.1 aesi(BROAD)
      5 C.1.1.1.3/B.2.2.3.1 aesi(BROAD)
      6 C.1.1.1.3/B.2.2.3.1 aesi(BROAD)
      7 C.1.1.1.3/B.2.2.3.1 aesi(BROAD)
      8 C.1.1.1.3/B.2.2.3.1 aesi(BROAD)

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
                           STUDYID                      USUBJID 
                "Study Identifier"  "Unique Subject Identifier" 
                            ASTDTM                      AEDECOD 
         "Analysis Start Datetime"    "Dictionary-Derived Term" 
                             AESEQ                          SMQ 
      "Sponsor-Defined Identifier"  "Standardized MedDRA Query" 

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

