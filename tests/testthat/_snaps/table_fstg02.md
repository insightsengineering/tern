# FSTG02 table variant 1 (Subgroup Analysis of Survival Duration) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                          B: Placebo               A: Drug X                                    
                                      Total n    n    Median (Months)    n    Median (Months)   Hazard Ratio   95% Wald CI 
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                      268     134        27.5         134        41.4             0.72       (0.53, 0.98)
      Sex                                                                                                                  
        F                               161     82         28.0         79         41.9             0.70       (0.46, 1.05)
        M                               107     52         17.3         55         27.9             0.78       (0.49, 1.26)
      Categorical Level Biomarker 2                                                                                        
        LOW                             95      45         24.7         50         38.1             0.71       (0.42, 1.17)
        MEDIUM                          93      56         23.7         37         41.7             0.57       (0.32, 1.01)
        HIGH                            80      33         27.9         47         35.2             0.98       (0.56, 1.72)

# FSTG02 table variant 2 (specifying class variables and options for the treatment variable)

    Code
      res
    Output
      Baseline Risk Factors                      Placebo                 Drug X                                      
                                Total n    n    Median (Months)    n    Median (Months)   Hazard Ratio   95% Wald CI 
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                268     134        27.5         134        41.4             0.72       (0.53, 0.98)
      Sex                                                                                                            
        M                         107     52         17.3         55         27.9             0.78       (0.49, 1.26)
        F                         161     82         28.0         79         41.9             0.70       (0.46, 1.05)
      Stratification Factor 1                                                                                        
        C                         94      45         16.3         49         54.7             0.53       (0.31, 0.90)
        B                         92      45         27.9         47         32.4             0.87       (0.53, 1.45)
        A                         82      44         35.7         38         35.2             0.86       (0.48, 1.53)

# FSTG02 table variant 3 (selecting columns and changing the alpha level)

    Code
      res
    Output
      Baseline Risk Factors                                                
                                      Total n   Hazard Ratio   90% Wald CI 
      —————————————————————————————————————————————————————————————————————
      All Patients                      268         0.72       (0.55, 0.93)
      Sex                                                                  
        F                               161         0.70       (0.50, 0.98)
        M                               107         0.78       (0.53, 1.17)
      Categorical Level Biomarker 2                                        
        LOW                             95          0.71       (0.46, 1.08)
        MEDIUM                          93          0.57       (0.36, 0.92)
        HIGH                            80          0.98       (0.61, 1.57)

# FSTG02 table variant 4 (fixed symbol size) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                          B: Placebo               A: Drug X                                    
                                      Total n    n    Median (Months)    n    Median (Months)   Hazard Ratio   95% Wald CI 
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                      268     134        27.5         134        41.4             0.72       (0.53, 0.98)
      Sex                                                                                                                  
        F                               161     82         28.0         79         41.9             0.70       (0.46, 1.05)
        M                               107     52         17.3         55         27.9             0.78       (0.49, 1.26)
      Categorical Level Biomarker 2                                                                                        
        LOW                             95      45         24.7         50         38.1             0.71       (0.42, 1.17)
        MEDIUM                          93      56         23.7         37         41.7             0.57       (0.32, 1.01)
        HIGH                            80      33         27.9         47         35.2             0.98       (0.56, 1.72)

