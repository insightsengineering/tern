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

---

    Code
      res
    Output
      $n
      [1] "xx."
      
      $count
      [1] "xx."
      
      $count_fraction
      function(x, ...) {
        attr(x, "label") <- NULL
      
        if (any(is.na(x))) {
          return("NA")
        }
      
        checkmate::assert_vector(x)
        checkmate::assert_integerish(x[1])
        assert_proportion_value(x[2], include_boundaries = TRUE)
      
        result <- if (x[1] == 0) {
          "0"
        } else {
          paste0(x[1], " (", round(x[2] * 100, 1), "%)")
        }
      
        return(result)
      }
      <environment: namespace:tern>
      
      $n_blq
      [1] "xx."
      
      $pval
      [1] "x.xxxx | (<0.0001)"
      

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
                    count_fraction                        n_blq 
                  "count_fraction"                      "n_blq" 
                              pval 
      "p-value (chi-squared test)" 

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
      

# control_analyze_vars works with customized parameters

    Code
      res
    Output
      $conf_level
      [1] 0.9
      
      $quantiles
      [1] 0.1 0.9
      
      $quantile_type
      [1] 2
      
      $test_mean
      [1] 0
      

