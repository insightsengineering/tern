# s_compare works for numeric

    Code
      res
    Output
       [1] "n"               "sum"             "mean"            "sd"             
       [5] "se"              "mean_sd"         "mean_se"         "mean_ci"        
       [9] "mean_sei"        "mean_sdi"        "mean_ci_3d"      "mean_pval"      
      [13] "median"          "mad"             "median_ci"       "median_ci_3d"   
      [17] "quantiles"       "iqr"             "range"           "min"            
      [21] "max"             "median_range"    "cv"              "geom_mean"      
      [25] "geom_sd"         "geom_mean_sd"    "geom_mean_ci"    "geom_cv"        
      [29] "geom_mean_ci_3d" "pval"           

# s_compare for numeric does not give p-value when not at least 2 values in each group

    Code
      res
    Output
      numeric(0)

# s_compare for factor works in usual case

    Code
      res
    Output
      [1] "n"                       "count"                  
      [3] "count_fraction"          "count_fraction_fixed_dp"
      [5] "fraction"                "n_blq"                  
      [7] "pval_counts"            

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
      $n$n
      n 
      5 
      
      
      $count
      $count$a
      count 
          3 
      
      $count$b
      count 
          1 
      
      $count$c
      count 
          1 
      
      
      $count_fraction
      $count_fraction$a
      count     p 
        3.0   0.6 
      
      $count_fraction$b
      count     p 
        1.0   0.2 
      
      $count_fraction$c
      count     p 
        1.0   0.2 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$a
      count     p 
        3.0   0.6 
      
      $count_fraction_fixed_dp$b
      count     p 
        1.0   0.2 
      
      $count_fraction_fixed_dp$c
      count     p 
        1.0   0.2 
      
      
      $fraction
      $fraction$a
        num denom 
          3     5 
      
      $fraction$b
        num denom 
          1     5 
      
      $fraction$c
        num denom 
          1     5 
      
      
      $n_blq
      $n_blq$n_blq
      n_blq 
          0 
      
      
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

