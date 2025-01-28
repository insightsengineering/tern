# get_stats works as expected for defaults

    Code
      res
    Output
      [1] "count"                   "count_fraction"         
      [3] "count_fraction_fixed_dp" "fraction"               

---

    Code
      res
    Output
      [1] "unique"       "nonunique"    "unique_count"

---

    Code
      res
    Output
      [1] "n"                       "count"                  
      [3] "count_fraction"          "count_fraction_fixed_dp"
      [5] "fraction"                "n_blq"                  

---

    Code
      res
    Output
       [1] "n"               "sum"             "mean"            "sd"             
       [5] "se"              "mean_sd"         "mean_se"         "mean_ci"        
       [9] "mean_sei"        "mean_sdi"        "mean_pval"       "median"         
      [13] "mad"             "median_ci"       "quantiles"       "iqr"            
      [17] "range"           "min"             "max"             "median_range"   
      [21] "cv"              "geom_mean"       "geom_mean_ci"    "geom_cv"        
      [25] "median_ci_3d"    "mean_ci_3d"      "geom_mean_ci_3d"

# get_labels_from_stats works as expected

    Code
      res
    Output
      $count
      [1] "count"
      
      $count_fraction
      [1] "count_fraction"
      
      $count_fraction_fixed_dp
      [1] "count_fraction_fixed_dp"
      
      $fraction
      [1] "fraction"
      

# get_indents_from_stats works as expected

    Code
      res
    Output
      $count
      [1] 0
      
      $count_fraction
      [1] 0
      
      $count_fraction_fixed_dp
      [1] 0
      
      $fraction
      [1] 0
      

# labels_use_control works as expected

    Code
      res
    Output
                               mean_ci                        mean_pval 
                         "Mean 34% CI" "Mean p-value (H0: mean = 0.47)" 
                             median_ci                        quantiles 
                       "Median 34% CI"                "24% and 86%-ile" 
                          geom_mean_ci 
               "Geometric Mean 34% CI" 

---

    Code
      res
    Output
                               mean_ci                        mean_pval 
                             "mean ci" "Mean p-value (H0: mean = 0.47)" 
                             median_ci                        quantiles 
                       "Median 34% CI"                   "my quantiles" 
                          geom_mean_ci 
               "Geometric Mean 34% CI" 

# summary_formats works as expected

    Code
      res
    Output
                            n                     sum                    mean 
                        "xx."                  "xx.x"                  "xx.x" 
                           sd                      se                 mean_sd 
                       "xx.x"                  "xx.x"           "xx.x (xx.x)" 
                      mean_se                 mean_ci                mean_sei 
                "xx.x (xx.x)"        "(xx.xx, xx.xx)"        "(xx.xx, xx.xx)" 
                     mean_sdi               mean_pval                  median 
             "(xx.xx, xx.xx)"    "x.xxxx | (<0.0001)"                  "xx.x" 
                          mad               median_ci               quantiles 
                       "xx.x"        "(xx.xx, xx.xx)"           "xx.x - xx.x" 
                          iqr                   range                     min 
                       "xx.x"           "xx.x - xx.x"                  "xx.x" 
                          max            median_range                      cv 
                       "xx.x"    "xx.x (xx.x - xx.x)"                  "xx.x" 
                    geom_mean            geom_mean_ci                 geom_cv 
                       "xx.x"        "(xx.xx, xx.xx)"                  "xx.x" 
                 median_ci_3d              mean_ci_3d         geom_mean_ci_3d 
      "xx.xx (xx.xx - xx.xx)" "xx.xx (xx.xx - xx.xx)" "xx.xx (xx.xx - xx.xx)" 

# summary_labels works as expected

    Code
      res
    Output
      $n
      [1] "n"
      
      $sum
      [1] "Sum"
      
      $mean
      [1] "Mean"
      
      $sd
      [1] "SD"
      
      $se
      [1] "SE"
      
      $mean_sd
      [1] "Mean (SD)"
      
      $mean_se
      [1] "Mean (SE)"
      
      $mean_ci
      [1] "Mean 95% CI"
      
      $mean_sei
      [1] "Mean -/+ 1xSE"
      
      $mean_sdi
      [1] "Mean -/+ 1xSD"
      
      $mean_pval
      [1] "Mean p-value (H0: mean = 0)"
      
      $median
      [1] "Median"
      
      $mad
      [1] "Median Absolute Deviation"
      
      $median_ci
      [1] "Median 95% CI"
      
      $quantiles
      [1] "25% and 75%-ile"
      
      $iqr
      [1] "IQR"
      
      $range
      [1] "Min - Max"
      
      $min
      [1] "Minimum"
      
      $max
      [1] "Maximum"
      
      $median_range
      [1] "Median (Min - Max)"
      
      $cv
      [1] "CV (%)"
      
      $geom_mean
      [1] "Geometric Mean"
      
      $geom_mean_ci
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
      [1] "CV % Geometric Mean"
      
      $median_ci_3d
      [1] "Median (95% CI)"
      
      $mean_ci_3d
      [1] "Mean (95% CI)"
      
      $geom_mean_ci_3d
      [1] "Geometric Mean (95% CI)"
      

---

    Code
      res
    Output
      $n
      [1] "n"
      
      $count
      [1] "count"
      
      $count_fraction
      [1] "count_fraction"
      
      $count_fraction_fixed_dp
      [1] "count_fraction_fixed_dp"
      
      $fraction
      [1] "fraction"
      
      $n_blq
      [1] "n_blq"
      
      $pval_counts
      [1] "p-value (chi-squared test)"
      

