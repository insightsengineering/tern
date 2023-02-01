# extract_rsp_biomarkers functions as expected with valid input and default arguments

    Code
      res
    Output
         biomarker biomarker_label n_tot n_rsp      prop        or       lcl      ucl
      1        AGE             AGE   200   164 0.8200000 0.9950509 0.9459879 1.046659
      2     BMRKR1          BMRKR1   200   164 0.8200000 0.9765311 0.8819556 1.081248
      3        AGE             AGE   110    91 0.8272727 1.0082471 0.9348405 1.087418
      4     BMRKR1          BMRKR1   110    91 0.8272727 0.9771788 0.8623205 1.107336
      5        AGE             AGE    90    73 0.8111111 0.9849219 0.9210816 1.053187
      6     BMRKR1          BMRKR1    90    73 0.8111111 0.9720575 0.8125165 1.162925
      7        AGE             AGE    70    53 0.7571429 0.9348856 0.8579986 1.018663
      8     BMRKR1          BMRKR1    70    53 0.7571429 1.1595718 0.9578324 1.403802
      9        AGE             AGE    68    58 0.8529412 0.9868318 0.8802767 1.106285
      10    BMRKR1          BMRKR1    68    58 0.8529412 0.8793711 0.7308739 1.058040
      11       AGE             AGE    62    53 0.8548387 1.0700781 0.9671640 1.183943
      12    BMRKR1          BMRKR1    62    53 0.8548387 0.8856737 0.7231318 1.084751
         conf_level      pval     pval_label     subgroup    var    var_label
      1        0.95 0.8474980 p-value (Wald) All Patients    ALL All Patients
      2        0.95 0.6477107 p-value (Wald) All Patients    ALL All Patients
      3        0.95 0.8313629 p-value (Wald)            F    SEX          SEX
      4        0.95 0.7174615 p-value (Wald)            F    SEX          SEX
      5        0.95 0.6567888 p-value (Wald)            M    SEX          SEX
      6        0.95 0.7566895 p-value (Wald)            M    SEX          SEX
      7        0.95 0.1241263 p-value (Wald)          LOW BMRKR2       BMRKR2
      8        0.95 0.1289696 p-value (Wald)          LOW BMRKR2       BMRKR2
      9        0.95 0.8201327 p-value (Wald)       MEDIUM BMRKR2       BMRKR2
      10       0.95 0.1731530 p-value (Wald)       MEDIUM BMRKR2       BMRKR2
      11       0.95 0.1892413 p-value (Wald)         HIGH BMRKR2       BMRKR2
      12       0.95 0.2405613 p-value (Wald)         HIGH BMRKR2       BMRKR2
         row_type
      1   content
      2   content
      3  analysis
      4  analysis
      5  analysis
      6  analysis
      7  analysis
      8  analysis
      9  analysis
      10 analysis
      11 analysis
      12 analysis

# extract_rsp_biomarkers works as expected with other custom options

    Code
      res
    Output
        biomarker        subgroup
      4       AGE             low
      5       AGE      low/medium
      6       AGE low/medium/high

# tabulate_rsp_biomarkers works as expected with valid input

    Code
      res
    Output
                       Total n   Responders   Response (%)   Odds Ratio      95% CI      p-value (Wald)
      —————————————————————————————————————————————————————————————————————————————————————————————————
      AGE                                                                                              
        All Patients     200        164          82.0%          1.00      (0.95, 1.05)       0.8475    
        SEX                                                                                            
          F              110         91          82.7%          1.01      (0.93, 1.09)       0.8314    
          M              90          73          81.1%          0.98      (0.92, 1.05)       0.6568    
        BMRKR2                                                                                         
          LOW            70          53          75.7%          0.93      (0.86, 1.02)       0.1241    
          MEDIUM         68          58          85.3%          0.99      (0.88, 1.11)       0.8201    
          HIGH           62          53          85.5%          1.07      (0.97, 1.18)       0.1892    
      BMRKR1                                                                                           
        All Patients     200        164          82.0%          0.98      (0.88, 1.08)       0.6477    
        SEX                                                                                            
          F              110         91          82.7%          0.98      (0.86, 1.11)       0.7175    
          M              90          73          81.1%          0.97      (0.81, 1.16)       0.7567    
        BMRKR2                                                                                         
          LOW            70          53          75.7%          1.16      (0.96, 1.40)       0.1290    
          MEDIUM         68          58          85.3%          0.88      (0.73, 1.06)       0.1732    
          HIGH           62          53          85.5%          0.89      (0.72, 1.08)       0.2406    

---

    Code
      res
    Output
      $col_x
      [1] 4
      
      $col_ci
      [1] 5
      
      $col_symbol_size
      [1] 1
      
      $forest_header
      [1] "Lower\nBetter"  "Higher\nBetter"
      

# tabulate_rsp_biomarkers functions as expected with NULL subgroups

    Code
      res
    Output
                       Total n   Responders   Response (%)   Odds Ratio      95% CI      p-value (Wald)
      —————————————————————————————————————————————————————————————————————————————————————————————————
      AGE                                                                                              
        All Patients     200        164          82.0%          1.00      (0.95, 1.05)       0.8475    
      BMRKR1                                                                                           
        All Patients     200        164          82.0%          0.98      (0.88, 1.08)       0.6477    

# tabulate_rsp_biomarkers works with only a single biomarker in the data frame

    Code
      res
    Output
                                     Total n   Responders   Response (%)   Odds Ratio      95% CI      p-value (Wald)
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Continuous Level Biomarker 1                                                                                   
        All Patients                   400        282          70.5%          0.98      (0.95, 1.01)       0.3000    

