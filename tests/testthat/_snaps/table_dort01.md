# DORT01 variant 1 is produced correctly

    Code
      res
    Output
                                                    A: Drug X        B: Placebo     C: Combination
                                                     (N=134)          (N=134)          (N=132)    
      ————————————————————————————————————————————————————————————————————————————————————————————
      Responders                                        68               73               62      
        Responders with subsequent event (%)        33 (48.5%)       51 (69.9%)       53 (85.5%)  
          Earliest contributing event                                                             
            Death                                       14               20               25      
            Disease Progression                         19               31               28      
        Responders without subsequent event (%)     35 (51.5%)       22 (30.1%)       9 (14.5%)   
      Duration of response (Months)                                                               
        Median                                         23.8             11.1             6.6      
        95% CI                                     (17.9, 39.6)     (8.3, 14.6)       (4.4, 9.2)  
        25% and 75%-ile                             9.3, 44.3        6.2, 20.5        3.0, 15.7   
        Range (censored)                           1.6 to 64.5      0.1 to 43.8      0.2 to 39.6  
        Range (event)                              0.3 to 44.3      0.6 to 61.9      0.2 to 53.8  
      12 Months duration                                                                          
        Patients remaining at risk                      28               25               18      
        Event Free Rate (%)                           63.41            43.82            33.72     
        95% CI                                    (50.48, 76.34)   (31.28, 56.36)   (21.28, 46.15)

# DORT01 variant 2 (selecting sectons) is produced correctly

    Code
      res
    Output
                                                   A: Drug X      B: Placebo    C: Combination
                                                    (N=134)        (N=134)         (N=132)    
      ————————————————————————————————————————————————————————————————————————————————————————
      Responders                                       68             73              62      
        Responders with subsequent event (%)       33 (48.5%)     51 (69.9%)      53 (85.5%)  
          Earliest contributing event                                                         
            Death                                      14             20              25      
            Disease Progression                        19             31              28      
        Responders without subsequent event (%)    35 (51.5%)     22 (30.1%)      9 (14.5%)   
      Duration of response (Months)                                                           
        Median                                        23.8           11.1            6.6      
        95% CI                                    (17.9, 39.6)   (8.3, 14.6)      (4.4, 9.2)  
        25% and 75%-ile                            9.3, 44.3      6.2, 20.5       3.0, 15.7   
        Range (censored)                          1.6 to 64.5    0.1 to 43.8     0.2 to 39.6  
        Range (event)                             0.3 to 44.3    0.6 to 61.9     0.2 to 53.8  
      Unstratified Analysis                                                                   
        p-value (log-rank)                                          0.0029         <0.0001    
        Hazard Ratio                                                 1.94            2.99     
        95% CI                                                   (1.24, 3.02)    (1.92, 4.67) 

# DORT01 variant 3 (modifying conftype and alpha level) is produced correctly

    Code
      res
    Output
                                                    A: Drug X        B: Placebo     C: Combination
                                                     (N=134)          (N=134)          (N=132)    
      ————————————————————————————————————————————————————————————————————————————————————————————
      Responders                                        68               73               62      
        Responders with subsequent event (%)        33 (48.5%)       51 (69.9%)       53 (85.5%)  
          Earliest contributing event                                                             
            Death                                       14               20               25      
            Disease Progression                         19               31               28      
        Responders without subsequent event (%)     35 (51.5%)       22 (30.1%)       9 (14.5%)   
      Duration of response (Months)                                                               
        Median                                         23.8             11.1             6.6      
        90% CI                                     (17.9, 30.2)     (9.1, 14.0)       (4.4, 8.0)  
        25% and 75%-ile                             9.3, 44.3        6.2, 20.5        3.0, 15.7   
        Range (censored)                           1.6 to 64.5      0.1 to 43.8      0.2 to 39.6  
        Range (event)                              0.3 to 44.3      0.6 to 61.9      0.2 to 53.8  
      12 Months duration                                                                          
        Patients remaining at risk                      28               25               18      
        Event Free Rate (%)                           63.41            43.82            33.72     
        97.5% CI                                  (48.63, 78.19)   (29.48, 58.16)   (19.49, 47.94)

# DORT01 variant 4 (modifying time point for the “xx duration”) is produced correctly

    Code
      res
    Output
                                                    A: Drug X        B: Placebo     C: Combination
                                                     (N=134)          (N=134)          (N=132)    
      ————————————————————————————————————————————————————————————————————————————————————————————
      Responders                                        68               73               62      
        Responders with subsequent event (%)        33 (48.5%)       51 (69.9%)       53 (85.5%)  
          Earliest contributing event                                                             
            Death                                       14               20               25      
            Disease Progression                         19               31               28      
        Responders without subsequent event (%)     35 (51.5%)       22 (30.1%)       9 (14.5%)   
      Duration of response (Months)                                                               
        Median                                         23.8             11.1             6.6      
        95% CI                                     (17.9, 39.6)     (8.3, 14.6)       (4.4, 9.2)  
        25% and 75%-ile                             9.3, 44.3        6.2, 20.5        3.0, 15.7   
        Range (censored)                           1.6 to 64.5      0.1 to 43.8      0.2 to 39.6  
        Range (event)                              0.3 to 44.3      0.6 to 61.9      0.2 to 53.8  
      6 Months duration                                                                           
        Patients remaining at risk                      51               48               28      
        Event Free Rate (%)                           83.63            76.14            50.57     
        95% CI                                    (74.78, 92.48)   (65.92, 86.35)   (37.68, 63.46)

