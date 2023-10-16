# analyze_vars works as expected with ref_group_last split fun

    Code
      res
    Output
                       ARM A         ARM C         ARM B   
                      (N=69)        (N=58)        (N=73)   
      —————————————————————————————————————————————————————
      AGE                                                  
        n               69            58            73     
        Mean (SD)   34.1 (6.8)    36.1 (7.4)    35.8 (7.1) 
        Median         32.8          36.2          35.4    
        Min - Max   22.4 - 48.0   23.0 - 58.3   23.3 - 57.5
      STRATA2                                              
        n               69            58            73     
        S1          39 (56.5%)    27 (46.6%)    34 (46.6%) 
        S2          30 (43.5%)    31 (53.4%)    39 (53.4%) 

# compare_vars works as expected with ref_group_last split fun

    Code
      res
    Output
                           ARM A        ARM C        ARM B   
                           (N=69)       (N=58)       (N=73)  
      ———————————————————————————————————————————————————————
      n                      69           58           73    
      Mean (SD)          34.1 (6.8)   36.1 (7.4)   35.8 (7.1)
      p-value (t-test)     0.1446       0.8212               

# summarize_ancova works as expected with ref_group_last split fun

    Code
      res
    Output
                                           ARM A           ARM C       ARM B 
                                          (N=69)          (N=58)       (N=73)
      ———————————————————————————————————————————————————————————————————————
      Unadjusted comparison                                                  
        n                                   69              58           73  
        Adjusted Mean                      6.30            6.16         6.75 
        Difference in Adjusted Means       -0.45           -0.59             
          95% CI                       (-1.60, 0.70)   (-1.79, 0.62)         
          p-value                         0.4433          0.3371             

---

    Code
      res
    Output
                                   ARM A    ARM C    ARM B 
                                   (N=69)   (N=58)   (N=73)
      —————————————————————————————————————————————————————
      Unadjusted rate (per year)                           
        Rate                       8.2061   7.8551   9.1554

# binary endpoint layouts work as expected with ref_group_last split fun

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

# time to event layouts works as expected with ref_group_last split fun

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

