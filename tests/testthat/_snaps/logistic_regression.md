# tidy.glm works as expected for simple case

    Code
      res
    Output
        variable   variable_label  term              term_label interaction
      1    ARMCD Planned Arm Code ARM A Reference ARM A, n = 64            
      2    ARMCD Planned Arm Code ARM B           ARM B, n = 68            
      3    ARMCD Planned Arm Code ARM C           ARM C, n = 52            
      4      AGE              Age   AGE                     Age            
      5      SEX              Sex     F    Reference F, n = 100            
      6      SEX              Sex     M               M, n = 84            
        interaction_label reference reference_label  estimate  std_error df
      1                                                                   2
      2                                             -1.973924    1.14659  1
      3                                              16.01132     2300.5  1
      4                                             0.1674111 0.08943489  1
      5                                                                    
      6                                             0.6291583  0.9193781  1
            pvalue is_variable_summary is_term_summary odds_ratio        lcl      ucl
      1  0.2272022                TRUE           FALSE                               
      2  0.0851491               FALSE            TRUE  0.1389107 0.00724572 2.663113
      3  0.9944468               FALSE            TRUE    8987294          0      Inf
      4 0.06122358               FALSE            TRUE    1.18224   0.938983 1.488517
      5                           TRUE           FALSE                               
      6  0.4937666               FALSE            TRUE   1.876031  0.1756955 20.03177
                            ci
      1                       
      2 0.00724572, 2.66311277
      3                 0, Inf
      4     0.938983, 1.488517
      5                       
      6  0.1756955, 20.0317685

# tidy.glm works as expected for interaction case

    Code
      res
    Output
          variable  term interaction reference   estimate std_error
      1        SEX     F                                           
      2        SEX     M                        0.7437057 0.9406595
      3      ARMCD ARM A                                           
      4      ARMCD ARM B                        -11.53157  6.443318
      5      ARMCD ARM B         AGE        35         NA        NA
      6      ARMCD ARM C                         14.72268  13103.52
      7      ARMCD ARM C         AGE        35         NA        NA
      8        AGE   AGE                       -0.0511774 0.1462201
      9        AGE   AGE       ARMCD     ARM A         NA        NA
      10       AGE   AGE       ARMCD     ARM B         NA        NA
      11       AGE   AGE       ARMCD     ARM C         NA        NA
      12 ARMCD:AGE ARM A                                           
      13 ARMCD:AGE ARM B                        0.3040042 0.1882037
      14 ARMCD:AGE ARM C                       0.04825293  356.2659

# logistic_regression_cols works as expected

    Code
      res
    Output
         Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio   Wald 75% CI   p-value
      ————————————————————————————————————————————————————————————————————————————————————————————————
                 df                estimate              se             or           ci           p   

# summarize_logistic works as expected for interaction model with continuous variable

    Code
      res
    Output
                                              Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 99% CI     p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Sex                                                                                                                                      
        Reference F, n = 100                                                                                                                   
        M, n = 84                                     1                  0.744              0.941           2.10       (0.19, 23.73)    0.4292 
      Planned Arm Code                                2                                                                                 0.2016 
        Reference ARM A, n = 64                                                                                                                
        ARM B, n = 68                                 1                 -11.532             6.443                                       0.0735 
          Age                                                                                                                                  
            35                                                                                              0.41       (0.01, 11.60)           
        ARM C, n = 52                                 1                  14.723           13103.521                                     0.9991 
          Age                                                                                                                                  
            35                                                                                            >999.99     (0.00, >999.99)          
      Age                                                                                                                                      
        Age                                           1                  -0.051             0.146                                       0.7263 
          Planned Arm Code                                                                                                                     
            ARM A                                                                                           0.95       (0.65, 1.38)            
            ARM B                                                                                           1.29       (0.95, 1.75)            
            ARM C                                                                                           1.00      (0.00, >999.99)          
      Interaction of Planned Arm Code * Age           2                                                                                 0.2713 
        Reference ARM A, n = 64                                                                                                                
        ARM B, n = 68                                 1                  0.304              0.188                                       0.1062 
        ARM C, n = 52                                 1                  0.048             356.266                                      0.9999 

# summarize_logistic works as expected for interaction model with categorical variable

    Code
      res
    Output
                                              Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 99% CI     p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Age                                                                                                                                      
        Age                                           1                  0.186              0.093           1.21       (0.95, 1.53)     0.0461 
      Planned Arm Code                                2                                                                                 1.0000 
        Reference ARM A, n = 64                                                                                                                
        ARM B, n = 68                                 1                 -19.927            4655.091                                     0.9966 
          Sex                                                                                                                                  
            F                                                                                               0.00      (0.00, >999.99)          
            M                                                                                               1.05       (0.02, 45.12)           
        ARM C, n = 52                                 1                  -0.485            6977.551                                     0.9999 
          Sex                                                                                                                                  
            F                                                                                               0.62      (0.00, >999.99)          
            M                                                                                             >999.99     (0.00, >999.99)          
      Sex                                                                                                                                      
        Reference F, n = 100                                                                                                                   
        M, n = 84                                     1                 -18.467            4655.091                                     0.9968 
          Planned Arm Code                                                                                                                     
            ARM A                                                                                           0.00      (0.00, >999.99)          
            ARM B                                                                                           4.52       (0.22, 94.89)           
            ARM C                                                                                           1.20      (0.00, >999.99)          
      Interaction of Planned Arm Code * Sex           2                                                                                 1.0000 
        Reference ARM A or F, n = 129                                                                                                          
        ARM B * M, n = 31                             1                  19.975            4655.091                                     0.9966 
        ARM C * M, n = 24                             1                  18.649            8840.154                                     0.9983 

# summarize_logistic works as expected for simple model without interactions

    Code
      res
    Output
                                  Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 99% CI     p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Planned Arm Code                    2                                                                                 0.2435 
        Reference ARM A, n = 64                                                                                                    
        ARM B, n = 68                     1                  -1.905             1.134           0.15       (<0.01, 2.76)    0.0928 
        ARM C, n = 52                     1                  16.089            2306.294       >999.99     (0.00, >999.99)   0.9944 
      Age                                                                                                                          
        Age                               1                  0.165              0.090           1.18       (0.94, 1.49)     0.0665 

