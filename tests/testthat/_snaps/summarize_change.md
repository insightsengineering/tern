# s_change_from_baseline handles empty data (complete missing for a visit)

    Code
      res
    Output
      $n
      n 
      0 
      
      $sum
      sum 
       NA 
      
      $mean
      mean 
        NA 
      
      $sd
      sd 
      NA 
      
      $se
      se 
      NA 
      
      $mean_sd
      mean   sd 
        NA   NA 
      
      $mean_se
      mean   se 
        NA   NA 
      
      $mean_ci
      mean_ci_lwr mean_ci_upr 
               NA          NA 
      attr(,"label")
      [1] "Mean 95% CI"
      
      $mean_sei
      mean_sei_lwr mean_sei_upr 
                NA           NA 
      attr(,"label")
      [1] "Mean -/+ 1xSE"
      
      $mean_sdi
      mean_sdi_lwr mean_sdi_upr 
                NA           NA 
      attr(,"label")
      [1] "Mean -/+ 1xSD"
      
      $mean_pval
      p_value 
           NA 
      attr(,"label")
      [1] "Mean p-value (H0: mean = 0)"
      
      $median
      median 
          NA 
      
      $mad
      mad 
       NA 
      
      $median_ci
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA
      attr(,"label")
      [1] "Median 95% CI"
      
      $quantiles
      quantile_0.25 quantile_0.75 
                 NA            NA 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $iqr
      iqr 
       NA 
      
      $range
      min max 
       NA  NA 
      
      $min
      min 
       NA 
      
      $max
      max 
       NA 
      
      $median_range
      median    min    max 
          NA     NA     NA 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $cv
      cv 
      NA 
      
      $geom_mean
      geom_mean 
            NaN 
      
      $geom_mean_ci
      mean_ci_lwr mean_ci_upr 
               NA          NA 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
      geom_cv 
           NA 
      

# s_change_from_baseline handles NA in baseline values

    Code
      res
    Output
      $n
      n 
      3 
      
      $sum
      sum 
        9 
      
      $mean
      mean 
         3 
      
      $sd
      sd 
       3 
      
      $se
            se 
      1.732051 
      
      $mean_sd
      mean   sd 
         3    3 
      
      $mean_se
          mean       se 
      3.000000 1.732051 
      
      $mean_ci
      mean_ci_lwr mean_ci_upr 
        -4.452413   10.452413 
      attr(,"label")
      [1] "Mean 95% CI"
      
      $mean_sei
      mean_sei_lwr mean_sei_upr 
          1.267949     4.732051 
      attr(,"label")
      [1] "Mean -/+ 1xSE"
      
      $mean_sdi
      mean_sdi_lwr mean_sdi_upr 
                 0            6 
      attr(,"label")
      [1] "Mean -/+ 1xSD"
      
      $mean_pval
        p_value 
      0.2254033 
      attr(,"label")
      [1] "Mean p-value (H0: mean = 0)"
      
      $median
      median 
           3 
      
      $mad
      mad 
        0 
      
      $median_ci
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA
      attr(,"label")
      [1] "Median 95% CI"
      
      $quantiles
      quantile_0.25 quantile_0.75 
                  0             6 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $iqr
      iqr 
        6 
      
      $range
      min max 
        0   6 
      
      $min
      min 
        0 
      
      $max
      max 
        6 
      
      $median_range
      median    min    max 
           3      0      6 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $cv
       cv 
      100 
      
      $geom_mean
      geom_mean 
             NA 
      
      $geom_mean_ci
      mean_ci_lwr mean_ci_upr 
               NA          NA 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
      geom_cv 
           NA 
      

# s_change_from_baseline handles baseline substitution

    Code
      res
    Output
      $`FALSE`
      $`FALSE`$n
      n 
      2 
      
      $`FALSE`$sum
      sum 
        3 
      
      $`FALSE`$mean
      mean 
       1.5 
      
      $`FALSE`$sd
             sd 
      0.7071068 
      
      $`FALSE`$se
       se 
      0.5 
      
      $`FALSE`$mean_sd
           mean        sd 
      1.5000000 0.7071068 
      
      $`FALSE`$mean_se
      mean   se 
       1.5  0.5 
      
      $`FALSE`$mean_ci
      mean_ci_lwr mean_ci_upr 
        -4.853102    7.853102 
      attr(,"label")
      [1] "Mean 95% CI"
      
      $`FALSE`$mean_sei
      mean_sei_lwr mean_sei_upr 
                 1            2 
      attr(,"label")
      [1] "Mean -/+ 1xSE"
      
      $`FALSE`$mean_sdi
      mean_sdi_lwr mean_sdi_upr 
         0.7928932    2.2071068 
      attr(,"label")
      [1] "Mean -/+ 1xSD"
      
      $`FALSE`$mean_pval
        p_value 
      0.2048328 
      attr(,"label")
      [1] "Mean p-value (H0: mean = 0)"
      
      $`FALSE`$median
      median 
         1.5 
      
      $`FALSE`$mad
      mad 
        0 
      
      $`FALSE`$median_ci
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA
      attr(,"label")
      [1] "Median 95% CI"
      
      $`FALSE`$quantiles
      quantile_0.25 quantile_0.75 
                  1             2 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $`FALSE`$iqr
      iqr 
        1 
      
      $`FALSE`$range
      min max 
        1   2 
      
      $`FALSE`$min
      min 
        1 
      
      $`FALSE`$max
      max 
        2 
      
      $`FALSE`$median_range
      median    min    max 
         1.5    1.0    2.0 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $`FALSE`$cv
            cv 
      47.14045 
      
      $`FALSE`$geom_mean
      geom_mean 
       1.414214 
      
      $`FALSE`$geom_mean_ci
       mean_ci_lwr  mean_ci_upr 
        0.01729978 115.60839614 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $`FALSE`$geom_cv
       geom_cv 
      52.10922 
      
      
      $`TRUE`
      $`TRUE`$n
      n 
      2 
      
      $`TRUE`$sum
      sum 
        5 
      
      $`TRUE`$mean
      mean 
       2.5 
      
      $`TRUE`$sd
           sd 
      2.12132 
      
      $`TRUE`$se
       se 
      1.5 
      
      $`TRUE`$mean_sd
         mean      sd 
      2.50000 2.12132 
      
      $`TRUE`$mean_se
      mean   se 
       2.5  1.5 
      
      $`TRUE`$mean_ci
      mean_ci_lwr mean_ci_upr 
        -16.55931    21.55931 
      attr(,"label")
      [1] "Mean 95% CI"
      
      $`TRUE`$mean_sei
      mean_sei_lwr mean_sei_upr 
                 1            4 
      attr(,"label")
      [1] "Mean -/+ 1xSE"
      
      $`TRUE`$mean_sdi
      mean_sdi_lwr mean_sdi_upr 
         0.3786797    4.6213203 
      attr(,"label")
      [1] "Mean -/+ 1xSD"
      
      $`TRUE`$mean_pval
        p_value 
      0.3440417 
      attr(,"label")
      [1] "Mean p-value (H0: mean = 0)"
      
      $`TRUE`$median
      median 
         2.5 
      
      $`TRUE`$mad
      mad 
        0 
      
      $`TRUE`$median_ci
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA
      attr(,"label")
      [1] "Median 95% CI"
      
      $`TRUE`$quantiles
      quantile_0.25 quantile_0.75 
                  1             4 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $`TRUE`$iqr
      iqr 
        3 
      
      $`TRUE`$range
      min max 
        1   4 
      
      $`TRUE`$min
      min 
        1 
      
      $`TRUE`$max
      max 
        4 
      
      $`TRUE`$median_range
      median    min    max 
         2.5    1.0    4.0 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $`TRUE`$cv
            cv 
      84.85281 
      
      $`TRUE`$geom_mean
      geom_mean 
              2 
      
      $`TRUE`$geom_mean_ci
       mean_ci_lwr  mean_ci_upr 
      2.992824e-04 1.336530e+04 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $`TRUE`$geom_cv
       geom_cv 
      127.0458 
      
      

# summarize_change works as expected

    Code
      res
    Output
                       all obs   
      ———————————————————————————
      V1                         
        n                 3      
        Mean (SD)    6.00 (3.00) 
        Median          6.00     
        Min - Max    3.00 - 9.00 
      V2                         
        n                 3      
        Mean (SD)   -1.00 (0.00) 
        Median          -1.00    
        Min - Max   -1.00 - -1.00
      V3                         
        n                 3      
        Mean (SD)   -2.00 (0.00) 
        Median          -2.00    
        Min - Max   -2.00 - -2.00

