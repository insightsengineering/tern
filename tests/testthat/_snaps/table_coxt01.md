# 1. Cox Regression

    Code
      res
    Output
                                    n    Hazard Ratio      95% CI      p-value
      ————————————————————————————————————————————————————————————————————————
      Treatment:                                                              
        ARM A vs control (ARM B)   247       0.70       (0.51, 0.96)   0.0293 
      Covariate:                                                              
        Sex                        247       0.71       (0.52, 0.98)   0.0370 
        Race                       247       0.70       (0.51, 0.97)   0.0318 
        Age                        247       0.70       (0.51, 0.97)   0.0321 

# 2. Cox Regression (with Interaction Term)

    Code
      res
    Output
                                       n    Hazard Ratio      95% CI      p-value   Interaction p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                       
        ARM A vs control (ARM B)      247       0.70       (0.51, 0.96)   0.0293                       
      Covariate:                                                                                       
        Sex                           247                                                 0.4635       
          F                                     0.64       (0.42, 0.98)                                
          M                                     0.82       (0.50, 1.35)                                
        Race                          247                                                 0.9197       
          ASIAN                                 0.75       (0.48, 1.16)                                
          BLACK OR AFRICAN AMERICAN             0.66       (0.34, 1.28)                                
          WHITE                                 0.65       (0.33, 1.27)                                
        Age                           247                                                 0.8626       
          34                                    0.70       (0.51, 0.97)                                

# 3. Cox Regression (specifying covariates)

    Code
      res
    Output
                                       n    Hazard Ratio      95% CI      p-value   Interaction p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                       
        ARM A vs control (ARM B)      247       0.70       (0.51, 0.96)   0.0293                       
      Covariate:                                                                                       
        Sex                           247                                                 0.4635       
          F                                     0.64       (0.42, 0.98)                                
          M                                     0.82       (0.50, 1.35)                                
        Race                          247                                                 0.9197       
          ASIAN                                 0.75       (0.48, 1.16)                                
          BLACK OR AFRICAN AMERICAN             0.66       (0.34, 1.28)                                
          WHITE                                 0.65       (0.33, 1.27)                                
        Age                           247                                                 0.8626       
          30                                    0.69       (0.48, 1.00)                                
          40                                    0.72       (0.48, 1.08)                                
          50                                    0.75       (0.35, 1.61)                                

# 4. Cox Regression (setting strata, ties, and alpha level)

    Code
      res
    Output
                                       n    Hazard Ratio      90% CI      p-value   Interaction p-value
      —————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                       
        ARM A vs control (ARM B)      247       0.70       (0.53, 0.92)   0.0293                       
      Covariate:                                                                                       
        Sex                           247                                                 0.4635       
          F                                     0.64       (0.42, 0.98)                                
          M                                     0.82       (0.50, 1.35)                                
        Race                          247                                                 0.9197       
          ASIAN                                 0.75       (0.48, 1.16)                                
          BLACK OR AFRICAN AMERICAN             0.66       (0.34, 1.28)                                
          WHITE                                 0.65       (0.33, 1.27)                                
        Age                           247                                                 0.8626       
          30                                    0.69       (0.51, 0.94)                                
          40                                    0.72       (0.51, 1.02)                                
          50                                    0.75       (0.39, 1.42)                                

