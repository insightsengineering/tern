# s_compare works for numeric

    Code
      res
    Output
       [1] "n"            "sum"          "mean"         "sd"           "se"          
       [6] "mean_sd"      "mean_se"      "mean_ci"      "mean_sei"     "mean_sdi"    
      [11] "mean_pval"    "median"       "mad"          "median_ci"    "quantiles"   
      [16] "iqr"          "range"        "min"          "max"          "median_range"
      [21] "cv"           "geom_mean"    "geom_mean_ci" "geom_cv"      "pval"        

# s_compare for numeric does not give p-value when not at least 2 values in each group

    Code
      res
    Output
      character(0)

# s_compare for factor works in usual case

    Code
      res
    Output
      [1] "n"              "count"          "count_fraction" "n_blq"         
      [5] "pval_counts"   

---

    Code
      res
    Output
      [1] 0.7659283

# s_compare for factor handles explicit NAs as expected

    Code
      res
    Output
      [1] 0.7659283

---

    Code
      res
    Output
      [1] 0.9063036

# s_compare for character works as expected

    Code
      res
    Output
      $n
      [1] 5
      
      $count
      $count$a
      [1] 3
      
      $count$b
      [1] 1
      
      $count$c
      [1] 1
      
      
      $count_fraction
      $count_fraction$a
      [1] 3.0 0.6
      
      $count_fraction$b
      [1] 1.0 0.2
      
      $count_fraction$c
      [1] 1.0 0.2
      
      
      $n_blq
      [1] 0
      
      $pval_counts
      [1] 0.7659283
      

# s_compare for logical works as expected

    Code
      res
    Output
      [1] 0.2702894

# compare_vars works with default settings in rtables layout pipeline

    Code
      res
    Output
                                       ARM B        ARM A        ARM C   
      ———————————————————————————————————————————————————————————————————
      AGE                                                                
        n                                73           69           58    
        Mean (SD)                    35.8 (7.1)   34.1 (6.8)   36.1 (7.4)
        p-value (t-test)                            0.1446       0.8212  
      SEX                                                                
        n                                73           69           58    
        F                            40 (54.8%)   38 (55.1%)   32 (55.2%)
        M                            33 (45.2%)   31 (44.9%)   26 (44.8%)
        p-value (chi-squared test)                  1.0000       1.0000  

# compare_vars works with custom settings

    Code
      res
    Output
                                         ARM C            ARM A            ARM B     
      ———————————————————————————————————————————————————————————————————————————————
      AGE                                                                            
        Mean, SD                       36.1, 7.4        34.1, 6.8        35.8, 7.1   
        p-value (t-test)                                  0.1176           0.8212    
      SEX                                                                            
        F                            32.00 (55.17%)   38.00 (55.07%)   40.00 (54.79%)
        M                            26.00 (44.83%)   31.00 (44.93%)   33.00 (45.21%)
        p-value (chi-squared test)                        1.0000           1.0000    

# compare_vars 'na_str' argument works as expected

    Code
      res
    Output
                                     ARM B       ARM A       ARM C  
      ——————————————————————————————————————————————————————————————
      n                               73          69          58    
      A: Drug X                        0       69 (100%)       0    
      B: Placebo                   73 (100%)       0           0    
      C: Combination                   0           0       58 (100%)
      p-value (chi-squared test)                   -           -    

