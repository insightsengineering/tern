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

# custom labels can be set with row_labels for analyze_colvars

    Code
      res
    Output
                             n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      ———————————————————————————————————————————————————————————————————————————
      F                                                                          
        some custom label   288   36.0   6.3   0.4    17.6           18.0        
      M                                                                          
        some custom label   234   36.3   8.5   0.6    23.4           23.5        

---

    Code
      res
    Output
                            n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      ——————————————————————————————————————————————————————————————————————————
      F                                                                         
        Female Statistic   288   36.0   6.3   0.4    17.6           18.0        
      M                                                                         
        Male Statistic     234   36.3   8.5   0.6    23.4           23.5        

# custom labels can be set with row_labels and summarize

    Code
      res
    Output
           n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      —————————————————————————————————————————————————————————
      F   288   36.0   6.3   0.4    17.6           18.0        
      M   234   36.3   8.5   0.6    23.4           23.5        

---

    Code
      res
    Output
                          n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      ————————————————————————————————————————————————————————————————————————
      Female Statistic   288   36.0   6.3   0.4    17.6           18.0        
      Male Statistic     234   36.3   8.5   0.6    23.4           23.5        

# summarize works with nested analyze

    Code
      sort_at_path(tbl, c("SEX", "*", "RACE"), scorefun(1))
    Output
        Sex                                                                                             
                Ethnicity                           n    Mean   SD    SE    CV (%)   CV % Geometric Mean
      ——————————————————————————————————————————————————————————————————————————————————————————————————
        Female                                     288   36.0   6.3   0.4    17.6           18.0        
                asian                              135   35.4   4.4   0.4    12.5           13.0        
                black or african american          81    35.3   6.9   0.8    19.6           21.0        
                white                              36    38.3   7.7   1.3    20.2           20.2        
                american indian or alaska native   36    37.8   8.5   1.4    22.5           23.0        
        Male                                       234   36.3   8.5   0.6    23.4           23.5        
                asian                              126   34.8   6.7   0.6    19.3           19.2        
                white                              63    36.5   9.5   1.2    26.1           28.6        
                black or african american          27    45.2   9.6   1.8    21.1           20.3        
                american indian or alaska native   18    32.8   4.9   1.2    15.1           15.3        

---

    Code
      tbl_sorted
    Output
                                            n    Mean    SD    SE    CV (%)   CV % Geometric Mean
      ———————————————————————————————————————————————————————————————————————————————————————————
      F                                    288   36.0   6.3    0.4    17.6           18.0        
        ASIAN                              135   35.4   4.4    0.4    12.5           13.0        
          C: Combination                   135   35.4   4.4    0.4    12.5           13.0        
            B                              63    35.4   3.9    0.5    10.9           11.2        
            C                              54    34.7   5.5    0.7    15.7           16.1        
            A                              18    37.6   0.4    0.1    1.1             1.1        
        BLACK OR AFRICAN AMERICAN          81    35.3   6.9    0.8    19.6           21.0        
          C: Combination                   81    35.3   6.9    0.8    19.6           21.0        
            C                              36    32.8   6.8    1.1    20.9           20.7        
            A                              27    41.1   2.4    0.5    5.9             5.8        
            B                              18    31.5   6.1    1.4    19.5           19.9        
        WHITE                              36    38.3   7.7    1.3    20.2           20.2        
          C: Combination                   36    38.3   7.7    1.3    20.2           20.2        
            A                              18    37.2   3.5    0.8    9.3             9.4        
            B                               9    49.6   0.0    0.0    0.0             0.0        
            C                               9    29.4   0.0    0.0    0.0             0.0        
        AMERICAN INDIAN OR ALASKA NATIVE   36    37.8   8.5    1.4    22.5           23.0        
          C: Combination                   36    37.8   8.5    1.4    22.5           23.0        
            A                              18    38.4   10.0   2.4    26.2           27.2        
            C                              18    37.2   6.8    1.6    18.4           18.8        
      M                                    234   36.3   8.5    0.6    23.4           23.5        
        ASIAN                              126   34.8   6.7    0.6    19.3           19.2        
          C: Combination                   126   34.8   6.7    0.6    19.3           19.2        
            A                              45    35.7   3.5    0.5    9.8             9.5        
            B                              45    37.2   7.6    1.1    20.6           20.3        
            C                              36    30.6   6.7    1.1    22.0           20.8        
        WHITE                              63    36.5   9.5    1.2    26.1           28.6        
          C: Combination                   63    36.5   9.5    1.2    26.1           28.6        
            A                              36    43.0   5.7    0.9    13.2           14.1        
            C                              18    23.6   0.6    0.2    2.7             2.7        
            B                               9    36.1   0.0    0.0    0.0             0.0        
        BLACK OR AFRICAN AMERICAN          27    45.2   9.6    1.8    21.1           20.3        
          C: Combination                   27    45.2   9.6    1.8    21.1           20.3        
            B                              18    38.7   1.7    0.4    4.5             4.5        
            C                               9    58.3   0.0    0.0    0.0             0.0        
        AMERICAN INDIAN OR ALASKA NATIVE   18    32.8   4.9    1.2    15.1           15.3        
          C: Combination                   18    32.8   4.9    1.2    15.1           15.3        
            B                               9    28.0   0.0    0.0    0.0             0.0        
            C                               9    37.6   0.0    0.0    0.0             0.0        

# analyze_vars_in_cols works well with categorical data

    Code
      build_table(lyt = lyt, df = adpp %>% mutate(counter = factor("n")))
    Output
      STRATA1   A: Drug X   B: Placebo   C: Combination
        SEX                                            
      —————————————————————————————————————————————————
      A                                                
        F           0           0          81 (100%)   
        M           0           0          81 (100%)   
      B                                                
        F           0           0          90 (100%)   
        M           0           0          81 (100%)   
      C                                                
        F           0           0          117 (100%)  
        M           0           0          72 (100%)   

---

    Code
      basic_table(show_colcounts = TRUE) %>% split_rows_by(var = "STRATA1",
        label_pos = "topleft") %>% split_cols_by("ARM") %>% analyze(vars = "SEX",
        afun = count_fraction) %>% append_topleft("  SEX") %>% build_table(adpp)
    Output
      STRATA1   A: Drug X   B: Placebo   C: Combination
        SEX       (N=0)       (N=0)         (N=522)    
      —————————————————————————————————————————————————
      A                                                
        F           0           0           81 (16%)   
        M           0           0           81 (16%)   
      B                                                
        F           0           0           90 (17%)   
        M           0           0           81 (16%)   
      C                                                
        F           0           0          117 (22%)   
        M           0           0           72 (14%)   

