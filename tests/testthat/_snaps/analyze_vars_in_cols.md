# analyze_vars_in_cols works correctly

    Code
      res
    Output
      ARM                              
        SEX             n    Mean   SE 
      —————————————————————————————————
      A: Drug X                        
        F               0     NA    NA 
        M               0     NA    NA 
      B: Placebo                       
        F               0     NA    NA 
        M               0     NA    NA 
      C: Combination                   
        F              288   36.0   0.4
        M              234   36.3   0.6

# custom labels can be set with labelstr

    Code
      res
    Output
                             n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      ———————————————————————————————————————————————————————————————————————————
      F                                                                          
        some custom label   288   36.0   6.3   0.4    17.6           18.0        
      M                                                                          
        some custom label   234   36.3   8.5   0.6    23.4           23.5        

# custom labels can be set with labelstr and summarize

    Code
      res
    Output
           n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      —————————————————————————————————————————————————————————
      F   288   36.0   6.3   0.4    17.6           18.0        
      M   234   36.3   8.5   0.6    23.4           23.5        

# summarize works with nested analyze

    Code
      sort_at_path(tbl, c("SEX", "*", "RACE"), scorefun(1))
    Output
                                            n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      ——————————————————————————————————————————————————————————————————————————————————————————
      F                                    288   36.0   6.3   0.4    17.6           18.0        
        ASIAN                              135   35.4   4.4   0.4    12.5           13.0        
        BLACK OR AFRICAN AMERICAN          81    35.3   6.9   0.8    19.6           21.0        
        WHITE                              36    38.3   7.7   1.3    20.2           20.2        
        AMERICAN INDIAN OR ALASKA NATIVE   36    37.8   8.5   1.4    22.5           23.0        
      M                                    234   36.3   8.5   0.6    23.4           23.5        
        ASIAN                              126   34.8   6.7   0.6    19.3           19.2        
        WHITE                              63    36.5   9.5   1.2    26.1           28.6        
        BLACK OR AFRICAN AMERICAN          27    45.2   9.6   1.8    21.1           20.3        
        AMERICAN INDIAN OR ALASKA NATIVE   18    32.8   4.9   1.2    15.1           15.3        

