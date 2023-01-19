# TTET01 default variant is produced correctly

    Code
      res
    Output
                                        A: Drug X        B: Placebo      C: Combination 
                                         (N=134)          (N=134)           (N=132)     
      ——————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)            79 (59%)        87 (64.9%)       116 (87.9%)   
        Earliest contributing event                                                     
          Death                             79               87               116       
      Patients without event (%)         55 (41%)        47 (35.1%)        16 (12.1%)   
      Time to Event (Months)                                                            
        Median                             41.4             27.5              11.1      
        95% CI                         (27.7, 54.7)     (17.3, 30.2)      (9.6, 15.9)   
        25% and 75%-ile                 15.4, 75.2       9.5, 54.9         5.3, 25.2    
        Range (censored)               0.4 to 154.7     0.9 to 91.0       0.3 to 49.4   
        Range (event)                  0.3 to 116.4     0.0 to 122.4      0.1 to 101.6  
      Unstratified Analysis                                                             
        p-value (log-rank)                                 0.0334           <0.0001     
        Hazard Ratio                                        1.39              2.75      
        95% CI                                          (1.03, 1.90)      (2.05, 3.70)  
      6 Months                                                                          
        Patients remaining at risk         106              112                92       
        Event Free Rate (%)               83.83            89.16             73.40      
        95% CI                        (77.49, 90.17)   (83.80, 94.53)    (65.72, 81.07) 
      Difference in Event Free Rate                         5.33             -10.43     
      95% CI                                           (-2.97, 13.64)   (-20.38, -0.48) 
      p-value (Z-test)                                     0.2080            0.0399     
      12 Months                                                                         
        Patients remaining at risk          92               83                56       
        Event Free Rate (%)               78.03            70.32             46.39      
        95% CI                        (70.82, 85.24)   (62.27, 78.37)    (37.59, 55.18) 
      Difference in Event Free Rate                        -7.71             -31.64     
      95% CI                                           (-18.51, 3.10)   (-43.01, -20.26)
      p-value (Z-test)                                     0.1622           <0.0001     

# TTET01 variant 2: selecting sections to display

    Code
      res
    Output
                                       A: Drug X        B: Placebo     C: Combination
                                        (N=134)          (N=134)          (N=132)    
      ———————————————————————————————————————————————————————————————————————————————
      Patients with event (%)           79 (59%)        87 (64.9%)      116 (87.9%)  
      Patients without event (%)        55 (41%)        47 (35.1%)       16 (12.1%)  
      Time to Event (Months)                                                         
        Median                            41.4             27.5             11.1     
        95% CI                        (27.7, 54.7)     (17.3, 30.2)     (9.6, 15.9)  
        25% and 75%-ile                15.4, 75.2       9.5, 54.9        5.3, 25.2   
        Range (censored)              0.4 to 154.7     0.9 to 91.0      0.3 to 49.4  
        Range (event)                 0.3 to 116.4     0.0 to 122.4     0.1 to 101.6 
      Unstratified Analysis                                                          
        p-value (log-rank)                                0.0334          <0.0001    
        Hazard Ratio                                       1.39             2.75     
        95% CI                                         (1.03, 1.90)     (2.05, 3.70) 
      12 Months                                                                      
        Patients remaining at risk         92               83               56      
        Event Free Rate (%)              78.03            70.32            46.39     
        95% CI                       (70.82, 85.24)   (62.27, 78.37)   (37.59, 55.18)

# TTET01 variant 3: modifying analysis details like conftype, ties, alpha level

    Code
      res
    Output
                                        A: Drug X        B: Placebo      C: Combination 
                                         (N=134)          (N=134)           (N=132)     
      ——————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)            79 (59%)        87 (64.9%)       116 (87.9%)   
        Earliest contributing event                                                     
          Death                             79               87               116       
      Patients without event (%)         55 (41%)        47 (35.1%)        16 (12.1%)   
      Time to Event (Months)                                                            
        Median                             41.4             27.5              11.1      
        90% CI                         (27.9, 54.1)     (18.8, 29.9)      (9.7, 15.1)   
        25% and 75%-ile                 15.4, 75.2       9.5, 54.9         5.3, 25.2    
        Range (censored)               0.4 to 154.7     0.9 to 91.0       0.3 to 49.4   
        Range (event)                  0.3 to 116.4     0.0 to 122.4      0.1 to 101.6  
      Unstratified Analysis                                                             
        p-value (log-rank)                                 0.0334           <0.0001     
        Hazard Ratio                                        1.39              2.75      
        95% CI                                          (1.03, 1.90)      (2.05, 3.70)  
      12 Months                                                                         
        Patients remaining at risk          92               83                56       
        Event Free Rate (%)               78.03            70.32             46.39      
        90% CI                        (71.24, 83.40)   (62.97, 76.49)    (38.87, 53.56) 
      Difference in Event Free Rate                        -7.71             -31.64     
      97.5% CI                                         (-20.07, 4.65)   (-44.64, -18.63)
      p-value (Z-test)                                     0.1622           <0.0001     

# TTET01 variant 4: with stratified analysis

    Code
      res
    Output
                                        A: Drug X        B: Placebo      C: Combination 
                                         (N=134)          (N=134)           (N=132)     
      ——————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)            79 (59%)        87 (64.9%)       116 (87.9%)   
        Earliest contributing event                                                     
          Death                             79               87               116       
      Patients without event (%)         55 (41%)        47 (35.1%)        16 (12.1%)   
      Time to Event (Months)                                                            
        Median                             41.4             27.5              11.1      
        95% CI                         (27.7, 54.7)     (17.3, 30.2)      (9.6, 15.9)   
        25% and 75%-ile                 15.4, 75.2       9.5, 54.9         5.3, 25.2    
        Range (censored)               0.4 to 154.7     0.9 to 91.0       0.3 to 49.4   
        Range (event)                  0.3 to 116.4     0.0 to 122.4      0.1 to 101.6  
      Unstratified Analysis                                                             
        p-value (log-rank)                                 0.0334           <0.0001     
        Hazard Ratio                                        1.39              2.75      
        95% CI                                          (1.03, 1.90)      (2.05, 3.70)  
      Stratified Analysis                                                               
        p-value (log-rank)                                 0.0478           <0.0001     
        Hazard Ratio                                        1.36              2.73      
        95% CI                                          (1.00, 1.86)      (2.02, 3.69)  
      12 Months                                                                         
        Patients remaining at risk          92               83                56       
        Event Free Rate (%)               78.03            70.32             46.39      
        95% CI                        (70.82, 85.24)   (62.27, 78.37)    (37.59, 55.18) 
      Difference in Event Free Rate                        -7.71             -31.64     
      95% CI                                           (-18.51, 3.10)   (-43.01, -20.26)
      p-value (Z-test)                                     0.1622           <0.0001     

# TTET01 variant 5: modifying time point

    Code
      res
    Output
                                        A: Drug X        B: Placebo     C: Combination 
                                         (N=134)          (N=134)           (N=132)    
      —————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)            79 (59%)        87 (64.9%)       116 (87.9%)  
        Earliest contributing event                                                    
          Death                             79               87               116      
      Patients without event (%)         55 (41%)        47 (35.1%)       16 (12.1%)   
      Time to Event (Months)                                                           
        Median                             41.4             27.5             11.1      
        95% CI                         (27.7, 54.7)     (17.3, 30.2)      (9.6, 15.9)  
        25% and 75%-ile                 15.4, 75.2       9.5, 54.9         5.3, 25.2   
        Range (censored)               0.4 to 154.7     0.9 to 91.0       0.3 to 49.4  
        Range (event)                  0.3 to 116.4     0.0 to 122.4     0.1 to 101.6  
      Unstratified Analysis                                                            
        p-value (log-rank)                                 0.0334           <0.0001    
        Hazard Ratio                                        1.39             2.75      
        95% CI                                          (1.03, 1.90)     (2.05, 3.70)  
      6 Months                                                                         
        Patients remaining at risk         106              112               92       
        Event Free Rate (%)               83.83            89.16             73.40     
        95% CI                        (77.49, 90.17)   (83.80, 94.53)   (65.72, 81.07) 
      Difference in Event Free Rate                         5.33            -10.43     
      95% CI                                           (-2.97, 13.64)   (-20.38, -0.48)
      p-value (Z-test)                                     0.2080           0.0399     

# TTET01 variant 6: requesting more than one p-value

    Code
      res
    Output
                                        A: Drug X        B: Placebo      C: Combination 
                                         (N=134)          (N=134)           (N=132)     
      ——————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)            79 (59%)        87 (64.9%)       116 (87.9%)   
        Earliest contributing event                                                     
          Death                             79               87               116       
      Patients without event (%)         55 (41%)        47 (35.1%)        16 (12.1%)   
      Time to Event (Months)                                                            
        Median                             41.4             27.5              11.1      
        95% CI                         (27.7, 54.7)     (17.3, 30.2)      (9.6, 15.9)   
        25% and 75%-ile                 15.4, 75.2       9.5, 54.9         5.3, 25.2    
        Range (censored)               0.4 to 154.7     0.9 to 91.0       0.3 to 49.4   
        Range (event)                  0.3 to 116.4     0.0 to 122.4      0.1 to 101.6  
      Unstratified Analysis                                                             
        p-value (log-rank)                                 0.0334           <0.0001     
          p-value (wald)                                   0.0342           <0.0001     
          p-value (likelihood)                             0.0341           <0.0001     
          Hazard Ratio                                      1.39              2.75      
              95% CI                                    (1.03, 1.90)      (2.05, 3.70)  
      12 Months                                                                         
        Patients remaining at risk          92               83                56       
        Event Free Rate (%)               78.03            70.32             46.39      
        95% CI                        (70.82, 85.24)   (62.27, 78.37)    (37.59, 55.18) 
      Difference in Event Free Rate                        -7.71             -31.64     
      95% CI                                           (-18.51, 3.10)   (-43.01, -20.26)
      p-value (Z-test)                                     0.1622           <0.0001     

