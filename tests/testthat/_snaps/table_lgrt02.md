# LGRT02 without interaction term is produced correctly

    Code
      res
    Output
                                                             Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 95% CI     p-value
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Planned Arm Code                                               2                                                                                 0.0346 
        Reference ARM A, n = 134                                                                                                                              
        ARM B, n = 134                                               1                  -2.162             1.084           0.12       (0.01, 0.96)     0.0461 
        ARM C, n = 132                                               1                  -0.090             1.426           0.91       (0.06, 14.97)    0.9499 
      Sex                                                                                                                                                     
        Reference M, n = 169                                                                                                                                  
        F, n = 231                                                   1                  0.364              0.701           1.44       (0.36, 5.69)     0.6032 
      Race                                                           5                                                                                 0.9685 
        Reference AMERICAN INDIAN OR ALASKA NATIVE, n = 25                                                                                                    
        ASIAN, n = 208                                               1                 -16.246            2017.122         0.00      (0.00, >999.99)   0.9936 
        BLACK OR AFRICAN AMERICAN, n = 91                            1                 -15.205            2017.122         0.00      (0.00, >999.99)   0.9940 
        WHITE, n = 74                                                1                 -15.955            2017.122         0.00      (0.00, >999.99)   0.9937 
        MULTIPLE, n = 1                                              1                  -0.363           10941.553         0.70      (0.00, >999.99)   1.0000 
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, n = 1             1                  1.036            10941.553         2.82      (0.00, >999.99)   0.9999 
      Age                                                                                                                                                     
        Age                                                          1                  0.071              0.053           1.07       (0.97, 1.19)     0.1866 

# LGRT02 with categorical interaction is produced correctly

    Code
      res
    Output
                                              Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 95% CI     p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Age                                                                                                                                      
        Age                                           1                  0.067              0.054           1.07       (0.96, 1.19)     0.2084 
      Planned Arm Code                                2                                                                                 0.4882 
        Reference ARM A, n = 134                                                                                                               
        ARM B, n = 134                                1                 -17.850            2362.767                                     0.9940 
          Sex                                                                                                                                  
            F                                                                                               0.23       (0.02, 2.11)            
            M                                                                                               0.00      (0.00, >999.99)          
        ARM C, n = 132                                1                 -16.442            2362.767                                     0.9944 
          Sex                                                                                                                                  
            F                                                                                             >999.99     (0.00, >999.99)          
            M                                                                                               0.00      (0.00, >999.99)          
      Sex                                                                                                                                      
        Reference M, n = 169                                                                                                                   
        F, n = 231                                    1                 -16.044            2362.767                                     0.9946 
          Planned Arm Code                                                                                                                     
            ARM A                                                                                           0.00      (0.00, >999.99)          
            ARM B                                                                                           1.39       (0.29, 6.59)            
            ARM C                                                                                         >999.99     (0.00, >999.99)          
      Interaction of Planned Arm Code * Sex           2                                                                                 0.9999 
        Reference ARM A or M, n = 248                                                                                                          
        ARM B * F, n = 82                             1                  16.373            2362.767                                     0.9945 
        ARM C * F, n = 70                             1                  32.492            3156.732                                     0.9918 

# LGRT02 with continuous interaction is produced correctly

    Code
      res
    Output
                                              Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 95% CI      p-value
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Sex                                                                                                                                       
        Reference M, n = 169                                                                                                                    
        F, n = 231                                    1                  0.381              0.710           1.46        (0.36, 5.88)     0.5915 
      Planned Arm Code                                2                                                                                  0.2768 
        Reference ARM A, n = 134                                                                                                                
        ARM B, n = 134                                1                  20.020             13.714                                       0.1443 
          Age                                                                                                                                   
            18                                                                                             113.59     (0.14, >999.99)           
            65                                                                                              0.00      (<0.01, >999.99)          
        ARM C, n = 132                                1                  15.622             14.810                                       0.2915 
          Age                                                                                                                                   
            18                                                                                             64.74      (0.03, >999.99)           
            65                                                                                              0.00      (<0.01, >999.99)          
      Age                                                                                                                                       
        Age                                           1                  0.877              0.581                                        0.1309 
          Planned Arm Code                                                                                                                      
            ARM A                                                                                           2.40        (0.77, 7.50)            
            ARM B                                                                                           1.03        (0.93, 1.14)            
            ARM C                                                                                           1.27        (0.84, 1.93)            
      Interaction of Planned Arm Code * Age           2                                                                                  0.2213 
        Reference ARM A, n = 134                                                                                                                
        ARM B, n = 134                                1                  -0.849             0.583                                        0.1449 
        ARM C, n = 132                                1                  -0.636             0.618                                        0.3034 

# LGRT02 with setting values indicating an event and custom alpha level is produced correctly

    Code
      res
    Output
                                                             Degrees of Freedom   Parameter Estimate   Standard Error   Odds Ratio     Wald 90% CI     p-value
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Planned Arm Code                                               2                                                                                 0.0346 
        Reference ARM A, n = 134                                                                                                                              
        ARM B, n = 134                                               1                  2.162              1.084           8.69       (1.46, 51.66)    0.0461 
        ARM C, n = 132                                               1                  0.090              1.426           1.09       (0.10, 11.43)    0.9499 
      Sex                                                                                                                                                     
        Reference M, n = 169                                                                                                                                  
        F, n = 231                                                   1                  -0.364             0.701           0.69       (0.22, 2.20)     0.6032 
      Age                                                                                                                                                     
        Age                                                          1                  -0.071             0.053           0.93       (0.85, 1.02)     0.1866 
      Race                                                           5                                                                                 0.9685 
        Reference AMERICAN INDIAN OR ALASKA NATIVE, n = 25                                                                                                    
        ASIAN, n = 208                                               1                  16.246            2017.122       >999.99     (0.00, >999.99)   0.9936 
        BLACK OR AFRICAN AMERICAN, n = 91                            1                  15.205            2017.122       >999.99     (0.00, >999.99)   0.9940 
        WHITE, n = 74                                                1                  15.955            2017.122       >999.99     (0.00, >999.99)   0.9937 
        MULTIPLE, n = 1                                              1                  0.363            10941.553         1.44      (0.00, >999.99)   1.0000 
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, n = 1             1                  -1.036           10941.553         0.35      (0.00, >999.99)   0.9999 

