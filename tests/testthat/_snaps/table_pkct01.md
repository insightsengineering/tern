# PKCT01 is produced correctly

    Code
      res
    Output
                         n     Mean    SD    SE    CV (%)   CV % Geometric Mean
      —————————————————————————————————————————————————————————————————————————
      A: Drug X                                                                
        Plasma Drug X                                                          
                        1474   6.5    6.7    0.2   102.4            NA         
        Plasma Drug Y                                                          
                         0      NA     NA    NA      NA             NA         
        Urine Drug X                                                           
                        804    0.9    1.8    0.1   210.7            NA         
        Urine Drug Y                                                           
                        804    0.9    1.8    0.1   210.7            NA         
      C: Combination                                                           
        Plasma Drug X                                                          
                        1452   6.5    6.7    0.2   103.3            NA         
        Plasma Drug Y                                                          
                        1452   13.1   13.5   0.4   103.3            NA         
        Urine Drug X                                                           
                        792    0.8    1.8    0.1   212.4            NA         
        Urine Drug Y                                                           
                        792    0.8    1.8    0.1   212.4            NA         

---

    Code
      res
    Output
                        n_blq
      ———————————————————————
      A: Drug X              
        Plasma Drug X        
                         402 
        Plasma Drug Y        
                          0  
        Urine Drug X         
                         402 
        Urine Drug Y         
                         402 
      C: Combination         
        Plasma Drug X        
                         396 
        Plasma Drug Y        
                         396 
        Urine Drug X         
                         396 
        Urine Drug Y         
                         396 

# Specific PKCT01 features are present

    Code
      res
    Output
      Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable
       Protocol: xxxxx
      Analyte:  Plasma Drug X Treatment: A: Drug X
      Analyte:  Urine Drug X Treatment: C: Combination
      Analyte:  Urine Drug Y Treatment: A: Drug X
      Analyte:  Plasma Drug Y Treatment: C: Combination
      
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Cohort/Treatment                                                                                                                                        
        Visit                                                                                                                                                 
          Norminal Time from First Dose           Number                                                                                                      
                                                    of                                                                                                        
                                           n    <LTR/BLQ>s    Mean      SD     CV (%) Mean   Geometric Mean   CV % Geometric Mean   Median   Minimum   Maximum
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      A: Drug X                                                                                                                                               
        Day 1                                                                                                                                                 
          0                                                                                                                                                   
                                          402      402         0        0          NE              NE                 NE              0         0         0   
          0.5                                                                                                                                                 
                                          134       0         12.6     1.51       12.0            12.5               12.2            12.6     9.72      15.6  
          1                                                                                                                                                   
                                          134       0         16.2     1.63       10.0            16.1               10.1            16.2     12.6      19.9  
          1.5                                                                                                                                                 
                                          134       0         15.6     1.46        9.3            15.6                9.3            15.5     12.3       19   
          2                                                                                                                                                   
                                          134       0         13.4     1.35       10.1            13.4               10.0            13.3     10.8      16.5  
          3                                                                                                                                                   
                                          134       0         8.47     1.25       14.7            8.38               15.0            8.4      5.88      10.9  
          4                                                                                                                                                   
                                          402       0         4.79     1.01       21.2            4.69               21.9            4.79      2.7      7.09  
          8                                                                                                                                                   
                                          402       0        0.348    0.179       51.6           0.303               58.2           0.318     0.076     0.866 
          12                                                                                                                                                  
                                          402       0        0.0224   0.0189      84.4           0.0156              111.2          0.017     0.002     0.083 
          24                                                                                                                                                  
                                          402      402         0        0          NE              NE                 NE              0         0         0   
        Day 2                                                                                                                                                 
          48                                                                                                                                                  
                                          402      402         0        0          NE              NE                 NE              0         0         0   
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      NE: Not Estimable

