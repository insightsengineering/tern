# analyze_vars works as expected with ref_group_position last split fun

    Code
      res[3:4, ]
    Output
                    ARM A        ARM B        ARM C   
                    (N=69)       (N=73)       (N=58)  
      ————————————————————————————————————————————————
      Mean (SD)   34.1 (6.8)   35.8 (7.1)   36.1 (7.4)
      Median         32.8         35.4         36.2   

# compare_vars works as expected with ref_group first split fun

    Code
      res[1:2, ]
    Output
                    ARM B        ARM A        ARM C   
                    (N=73)       (N=69)       (N=58)  
      ————————————————————————————————————————————————
      n               73           69           58    
      Mean (SD)   35.8 (7.1)   34.1 (6.8)   36.1 (7.4)

# summarize_ancova works as expected with ref_group position split fun

    Code
      res[1:2, ]
    Output
                              ARM A    ARM B    ARM C 
                              (N=69)   (N=73)   (N=58)
      ————————————————————————————————————————————————
      Unadjusted comparison                           
        n                       69       73       58  

# binary endpoint layouts work as expected with ref_group_position last split fun

    Code
      res
    Output
                                             A: Drug X          C: Combination     B: Placebo
                                               (N=69)               (N=58)           (N=73)  
      ———————————————————————————————————————————————————————————————————————————————————————
      Odds Ratio (95% CI)                2.47 (1.22 - 5.01)   2.29 (1.10 - 4.78)             
      Difference in Response rate (%)           20.5                 19.0                    
        95% CI (Wald, with correction)      (3.6, 37.3)          (1.2, 36.8)                 
        p-value (Chi-Squared Test)             0.0113               0.0263                   

# time to event layouts works as expected with ref_group_position last split fun

    Code
      res
    Output
                                            ARM A             ARM C            ARM B     
                                            (N=69)           (N=58)            (N=73)    
      ———————————————————————————————————————————————————————————————————————————————————
      CoxPH                                                                              
        p-value (log-rank)                  0.0159           0.1820                      
        Hazard Ratio                         0.58             1.31                       
        95% CI                           (0.37, 0.91)     (0.88, 1.95)                   
      6 Months                                                                           
        Patients remaining at risk            49               39                46      
        Event Free Rate (%)                 85.29             71.87            71.55     
        95% CI                          (76.38, 94.19)   (60.15, 83.58)    (60.96, 82.14)
        Difference in Event Free Rate       13.74             0.31                       
          95% CI                        (-0.10, 27.57)   (-15.47, 16.10)                 
          p-value (Z-test)                  0.0517           0.9688                      

# summarize_ancova works as expected with ref_group_position last split fun

    Code
      res
    Output
                                   ARM A    ARM C    ARM B 
                                   (N=69)   (N=58)   (N=73)
      —————————————————————————————————————————————————————
      Unadjusted rate (per year)                           
        Rate                       8.2061   7.8551   9.1554

