# analyze_vars_in_cols works correctly

    Code
      res
    Output
      ARM                              
        SEX             n    Mean   SE 
      —————————————————————————————————
      A: Drug X                        
        F                              
                        0     NA    NA 
        M                              
                        0     NA    NA 
      B: Placebo                       
        F                              
                        0     NA    NA 
        M                              
                        0     NA    NA 
      C: Combination                   
        F                              
                       288   36.0   0.4
        M                              
                       234   36.3   0.6

# custom labels can be set with labelstr

    Code
      res
    Output
                           n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      —————————————————————————————————————————————————————————————————————————
      some custom label   288   36.0   6.3   0.4    17.6           18.0        
      some custom label   234   36.3   8.5   0.6    23.4           23.5        

