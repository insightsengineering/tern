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
      [5] "n_blq"                  

---

    Code
      res
    Output
       [1] "n"            "sum"          "mean"         "sd"           "se"          
       [6] "mean_sd"      "mean_se"      "mean_ci"      "mean_sei"     "mean_sdi"    
      [11] "mean_pval"    "median"       "mad"          "median_ci"    "quantiles"   
      [16] "iqr"          "range"        "min"          "max"          "median_range"
      [21] "cv"           "geom_mean"    "geom_mean_ci" "geom_cv"     

# get_labels_from_stats works as expected

    Code
      res
    Output
                        count          count_fraction count_fraction_fixed_dp 
                      "count"        "count_fraction"        "count_fraction" 
                     fraction 
                   "fraction" 

# get_indents_from_stats works as expected

    Code
      res
    Output
                        count          count_fraction count_fraction_fixed_dp 
                            0                       0                       0 
                     fraction 
                            0 

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
                         n                  sum                 mean 
                     "xx."               "xx.x"               "xx.x" 
                        sd                   se              mean_sd 
                    "xx.x"               "xx.x"        "xx.x (xx.x)" 
                   mean_se              mean_ci             mean_sei 
             "xx.x (xx.x)"     "(xx.xx, xx.xx)"     "(xx.xx, xx.xx)" 
                  mean_sdi            mean_pval               median 
          "(xx.xx, xx.xx)"              "xx.xx"               "xx.x" 
                       mad            median_ci            quantiles 
                    "xx.x"     "(xx.xx, xx.xx)"        "xx.x - xx.x" 
                       iqr                range                  min 
                    "xx.x"        "xx.x - xx.x"               "xx.x" 
                       max         median_range                   cv 
                    "xx.x" "xx.x (xx.x - xx.x)"               "xx.x" 
                 geom_mean         geom_mean_ci              geom_cv 
                    "xx.x"     "(xx.xx, xx.xx)"               "xx.x" 

# summary_labels works as expected

    Code
      res
    Output
                                  n                           sum 
                                "n"                         "Sum" 
                               mean                            sd 
                             "Mean"                          "SD" 
                                 se                       mean_sd 
                               "SE"                   "Mean (SD)" 
                            mean_se                       mean_ci 
                        "Mean (SE)"                 "Mean 95% CI" 
                           mean_sei                      mean_sdi 
                    "Mean -/+ 1xSE"               "Mean -/+ 1xSD" 
                          mean_pval                        median 
      "Mean p-value (H0: mean = 0)"                      "Median" 
                                mad                     median_ci 
        "Median Absolute Deviation"               "Median 95% CI" 
                          quantiles                           iqr 
                  "25% and 75%-ile"                         "IQR" 
                              range                           min 
                        "Min - Max"                     "Minimum" 
                                max                  median_range 
                          "Maximum"          "Median (Min - Max)" 
                                 cv                     geom_mean 
                           "CV (%)"              "Geometric Mean" 
                       geom_mean_ci                       geom_cv 
            "Geometric Mean 95% CI"         "CV % Geometric Mean" 

---

    Code
      res
    Output
                                 n                        count 
                               "n"                      "count" 
                    count_fraction      count_fraction_fixed_dp 
                  "count_fraction"             "count_fraction" 
                             n_blq                  pval_counts 
                           "n_blq" "p-value (chi-squared test)" 

# summary_custom works as expected

    Code
      res
    Output
      $stats
       [1] "n"            "sum"          "mean"         "sd"           "se"          
       [6] "mean_sd"      "mean_se"      "mean_ci"      "mean_sei"     "mean_sdi"    
      [11] "mean_pval"    "median"       "mad"          "median_ci"    "quantiles"   
      [16] "iqr"          "range"        "min"          "max"          "median_range"
      [21] "cv"           "geom_mean"    "geom_mean_ci" "geom_cv"     
      
      $formats
                         n                  sum                 mean 
                     "xx."               "xx.x"               "xx.x" 
                        sd                   se              mean_sd 
                    "xx.x"               "xx.x"        "xx.x (xx.x)" 
                   mean_se              mean_ci             mean_sei 
             "xx.x (xx.x)"     "(xx.xx, xx.xx)"     "(xx.xx, xx.xx)" 
                  mean_sdi            mean_pval               median 
          "(xx.xx, xx.xx)"              "xx.xx"               "xx.x" 
                       mad            median_ci            quantiles 
                    "xx.x"     "(xx.xx, xx.xx)"        "xx.x - xx.x" 
                       iqr                range                  min 
                    "xx.x"        "xx.x - xx.x"               "xx.x" 
                       max         median_range                   cv 
                    "xx.x" "xx.x (xx.x - xx.x)"               "xx.x" 
                 geom_mean         geom_mean_ci              geom_cv 
                    "xx.x"     "(xx.xx, xx.xx)"               "xx.x" 
      
      $labels
                                  n                           sum 
                                "n"                         "Sum" 
                               mean                            sd 
                             "Mean"                          "SD" 
                                 se                       mean_sd 
                               "SE"                   "Mean (SD)" 
                            mean_se                       mean_ci 
                        "Mean (SE)"                 "Mean 95% CI" 
                           mean_sei                      mean_sdi 
                    "Mean -/+ 1xSE"               "Mean -/+ 1xSD" 
                          mean_pval                        median 
      "Mean p-value (H0: mean = 0)"                      "Median" 
                                mad                     median_ci 
        "Median Absolute Deviation"               "Median 95% CI" 
                          quantiles                           iqr 
                  "25% and 75%-ile"                         "IQR" 
                              range                           min 
                        "Min - Max"                     "Minimum" 
                                max                  median_range 
                          "Maximum"          "Median (Min - Max)" 
                                 cv                     geom_mean 
                           "CV (%)"              "Geometric Mean" 
                       geom_mean_ci                       geom_cv 
            "Geometric Mean 95% CI"         "CV % Geometric Mean" 
      
      $indent_mods
                 n          sum         mean           sd           se      mean_sd 
                 0            0            0            0            0            0 
           mean_se      mean_ci     mean_sei     mean_sdi    mean_pval       median 
                 0            0            0            0            0            0 
               mad    median_ci    quantiles          iqr        range          min 
                 0            0            0            0            0            0 
               max median_range           cv    geom_mean geom_mean_ci      geom_cv 
                 0            0            0            0            0            0 
      

---

    Code
      res
    Output
      $stats
      [1] "n"     "count"
      
      $formats
      $formats$n
      [1] "xx.xx"
      
      $formats$count
      [1] "xx."
      
      
      $labels
          n count 
        "n"   "#" 
      
      $indent_mods
          n count 
          2     2 
      

