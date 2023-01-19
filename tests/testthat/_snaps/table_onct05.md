# ONCT05 variant 1 (Objective Response Rate by Subgroup) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                   B: Placebo           A: Drug X                                  
                                Total n    n    Response (%)    n    Response (%)   Odds Ratio      95% CI    
      ————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                200     100      71.0%       100      90.0%          3.68      (1.68, 8.04) 
      Sex                                                                                                     
        F                         120     62       64.5%       58       91.4%          5.83      (2.03, 16.73)
        M                         80      38       81.6%       42       88.1%          1.67      (0.48, 5.79) 
      Stratification Factor 2                                                                                 
        S1                        105     48       70.8%       57       89.5%          3.50      (1.22, 10.00)
        S2                        95      52       71.2%       43       90.7%          3.95      (1.20, 13.01)

# ONCT05 variant 2 (Specifying class variables) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                   B: Placebo           A: Drug X                                    
                                Total n    n    Response (%)    n    Response (%)   Odds Ratio       95% CI     
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                200     100      71.0%       100      90.0%          3.68       (1.68, 8.04)  
      Sex                                                                                                       
        M                         80      38       81.6%       42       88.1%          1.67       (0.48, 5.79)  
        F                         120     62       64.5%       58       91.4%          5.83       (2.03, 16.73) 
      Stratification Factor 1                                                                                   
        C                         72      36       72.2%       36       94.4%          6.54       (1.32, 32.44) 
        B                         71      35       74.3%       36       77.8%          1.21       (0.41, 3.61)  
        A                         57      29       65.5%       28       100.0%       >999.99     (0.00, >999.99)

# ONCT05 variant 3 (selecting columns and changing the alpha level) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                                                                      
                                Total n   Odds Ratio      90% CI       p-value (Chi-Squared Test)
      ———————————————————————————————————————————————————————————————————————————————————————————
      All Patients                200        3.68      (1.91, 7.09)              0.0007          
      Sex                                                                                        
        F                         120        5.83      (2.41, 14.12)             0.0004          
        M                         80         1.67      (0.59, 4.74)              0.4150          
      Stratification Factor 2                                                                    
        S1                        105        3.50      (1.45, 8.45)              0.0154          
        S2                        95         3.95      (1.45, 10.74)             0.0178          

# ONCT05 variant 4 (setting values indicating response) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                   B: Placebo           A: Drug X                                    
                                Total n    n    Response (%)    n    Response (%)   Odds Ratio       95% CI     
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                200     100      95.0%       100      99.0%          5.21       (0.60, 45.43) 
      Sex                                                                                                       
        F                         120     62       96.8%       58       98.3%          1.90       (0.17, 21.53) 
        M                         80      38       92.1%       42       100.0%       >999.99     (0.00, >999.99)
      Stratification Factor 2                                                                                   
        S1                        105     48       97.9%       57       100.0%       >999.99     (0.00, >999.99)
        S2                        95      52       92.3%       43       97.7%          3.50       (0.38, 32.55) 

