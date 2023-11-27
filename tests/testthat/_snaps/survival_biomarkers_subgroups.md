# extract_survival_biomarkers functions as expected with valid input and default arguments

    Code
      res
    Output
         biomarker              biomarker_label n_tot n_tot_events   median        hr
      1        AGE                          Age   200          141 753.5176 1.0113037
      2     BMRKR1 Continuous Level Biomarker 1   200          141 753.5176 0.9987473
      3        AGE                          Age   110           79 685.2335 0.9974417
      4     BMRKR1 Continuous Level Biomarker 1   110           79 685.2335 1.0145281
      5        AGE                          Age    90           62 888.4916 1.0213080
      6     BMRKR1 Continuous Level Biomarker 1    90           62 888.4916 0.9829857
      7        AGE                          Age    70           52 735.4722 1.0139949
      8     BMRKR1 Continuous Level Biomarker 1    70           52 735.4722 0.9991674
      9        AGE                          Age    68           42 858.9952 1.0332689
      10    BMRKR1 Continuous Level Biomarker 1    68           42 858.9952 0.9709938
      11       AGE                          Age    62           47 727.8043 0.9964359
      12    BMRKR1 Continuous Level Biomarker 1    62           47 727.8043 1.0611819
               lcl      ucl conf_level      pval     pval_label     subgroup    var
      1  0.9880703 1.035084       0.95 0.3431854 p-value (Wald) All Patients    ALL
      2  0.9521917 1.047579       0.95 0.9589522 p-value (Wald) All Patients    ALL
      3  0.9646002 1.031401       0.95 0.8807989 p-value (Wald)            F    SEX
      4  0.9580753 1.074307       0.95 0.6214667 p-value (Wald)            F    SEX
      5  0.9890882 1.054577       0.95 0.1973532 p-value (Wald)            M    SEX
      6  0.9024823 1.070670       0.95 0.6938508 p-value (Wald)            M    SEX
      7  0.9678771 1.062310       0.95 0.5584219 p-value (Wald)          LOW BMRKR2
      8  0.9243879 1.079996       0.95 0.9832563 p-value (Wald)          LOW BMRKR2
      9  0.9768608 1.092934       0.95 0.2531999 p-value (Wald)       MEDIUM BMRKR2
      10 0.8873583 1.062512       0.95 0.5218385 p-value (Wald)       MEDIUM BMRKR2
      11 0.9646222 1.029299       0.95 0.8292481 p-value (Wald)         HIGH BMRKR2
      12 0.9643359 1.167754       0.95 0.2239082 p-value (Wald)         HIGH BMRKR2
                            var_label row_type
      1                  All Patients  content
      2                  All Patients  content
      3                           Sex analysis
      4                           Sex analysis
      5                           Sex analysis
      6                           Sex analysis
      7  Continuous Level Biomarker 2 analysis
      8  Continuous Level Biomarker 2 analysis
      9  Continuous Level Biomarker 2 analysis
      10 Continuous Level Biomarker 2 analysis
      11 Continuous Level Biomarker 2 analysis
      12 Continuous Level Biomarker 2 analysis

# extract_survival_biomarkers works as expected with groups_lists

    Code
      res
    Output
         biomarker        subgroup
      7        AGE             low
      8     BMRKR1             low
      9        AGE      low/medium
      10    BMRKR1      low/medium
      11       AGE low/medium/high
      12    BMRKR1 low/medium/high

# tabulate_survival_biomarkers works as expected with valid input

    Code
      res
    Output
                                       Total n   Total Events   Median (DAYS)   Hazard Ratio   95% Wald CI    p-value (Wald)
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Age                                                                                                                   
        All Patients                     200         141            753.5           1.01       (0.99, 1.04)       0.3432    
        Sex                                                                                                                 
          F                              110          79            685.2           1.00       (0.96, 1.03)       0.8808    
          M                              90           62            888.5           1.02       (0.99, 1.05)       0.1974    
        Continuous Level Biomarker 2                                                                                        
          LOW                            70           52            735.5           1.01       (0.97, 1.06)       0.5584    
          MEDIUM                         68           42            859.0           1.03       (0.98, 1.09)       0.2532    
          HIGH                           62           47            727.8           1.00       (0.96, 1.03)       0.8292    
      Continuous Level Biomarker 1                                                                                          
        All Patients                     200         141            753.5           1.00       (0.95, 1.05)       0.9590    
        Sex                                                                                                                 
          F                              110          79            685.2           1.01       (0.96, 1.07)       0.6215    
          M                              90           62            888.5           0.98       (0.90, 1.07)       0.6939    
        Continuous Level Biomarker 2                                                                                        
          LOW                            70           52            735.5           1.00       (0.92, 1.08)       0.9833    
          MEDIUM                         68           42            859.0           0.97       (0.89, 1.06)       0.5218    
          HIGH                           62           47            727.8           1.06       (0.96, 1.17)       0.2239    

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
      [1] "Higher\nBetter" "Lower\nBetter" 
      

# tabulate_survival_biomarkers functions as expected with NULL subgroups

    Code
      res
    Output
                                     Total n   Total Events   Median (DAYS)   Hazard Ratio   95% Wald CI    p-value (Wald)
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Age                                                                                                                 
        All Patients                   200         141            753.5           1.01       (0.99, 1.04)       0.3432    
      Continuous Level Biomarker 1                                                                                        
        All Patients                   200         141            753.5           1.00       (0.95, 1.05)       0.9590    

# tabulate_survival_biomarkers works with only a single biomarker in the data frame

    Code
      res
    Output
                                     Total n   Total Events   Median   Hazard Ratio   95% Wald CI    p-value (Wald)
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Continuous Level Biomarker 1                                                                                 
        All Patients                   400         282        680.0        0.98       (0.95, 1.01)       0.3000    

# tabulate_survival_biomarkers na_str argument works as expected

    Code
      res
    Output
                                       Total n   Total Events   Median (DAYS)   Hazard Ratio   95% Wald CI    p-value (Wald)
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Age                                                                                                                   
        All Patients                     200         141            753.5           1.01       (0.99, 1.04)       0.3432    
        Sex                                                                                                                 
          F                              110          79            685.2        <No data>     (0.96, 1.03)       0.8808    
          M                              90           62            888.5        <No data>     (0.99, 1.05)       0.1974    
        Continuous Level Biomarker 2                                                                                        
          LOW                            70           52            735.5           1.01       (0.97, 1.06)       0.5584    
          MEDIUM                         68           42            859.0           1.03       (0.98, 1.09)       0.2532    
          HIGH                           62           47            727.8           1.00       (0.96, 1.03)       0.8292    
      Continuous Level Biomarker 1                                                                                          
        All Patients                     200         141            753.5        <No data>     (0.95, 1.05)       0.9590    
        Sex                                                                                                                 
          F                              110          79            685.2        <No data>     (0.96, 1.07)       0.6215    
          M                              90           62            888.5           0.98       (0.90, 1.07)       0.6939    
        Continuous Level Biomarker 2                                                                                        
          LOW                            70           52            735.5           1.00       (0.92, 1.08)       0.9833    
          MEDIUM                         68           42            859.0           0.97       (0.89, 1.06)       0.5218    
          HIGH                           62           47            727.8           1.06       (0.96, 1.17)       0.2239    

