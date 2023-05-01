# control_summarize_vars works with customized parameters

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
      

# s_summary return NA for x length 0L

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
      

# s_summary handles NA

    Code
      res
    Output
      $n
      n 
      1 
      
      $sum
      sum 
        1 
      
      $mean
      mean 
         1 
      
      $sd
      sd 
      NA 
      
      $se
      se 
      NA 
      
      $mean_sd
      mean   sd 
         1   NA 
      
      $mean_se
      mean   se 
         1   NA 
      
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
           1 
      
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
                  1             1 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $iqr
      iqr 
        0 
      
      $range
      min max 
        1   1 
      
      $min
      min 
        1 
      
      $max
      max 
        1 
      
      $median_range
      median    min    max 
           1      1      1 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $cv
      cv 
      NA 
      
      $geom_mean
      geom_mean 
              1 
      
      $geom_mean_ci
      mean_ci_lwr mean_ci_upr 
               NA          NA 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
      geom_cv 
           NA 
      

---

    Code
      res
    Output
      $n
      n 
      2 
      
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
             NA 
      
      $geom_mean_ci
      mean_ci_lwr mean_ci_upr 
               NA          NA 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
      geom_cv 
           NA 
      

# s_summary returns right results for n = 2

    Code
      res
    Output
      $n
      n 
      2 
      
      $sum
      sum 
        3 
      
      $mean
      mean 
       1.5 
      
      $sd
             sd 
      0.7071068 
      
      $se
       se 
      0.5 
      
      $mean_sd
           mean        sd 
      1.5000000 0.7071068 
      
      $mean_se
      mean   se 
       1.5  0.5 
      
      $mean_ci
      mean_ci_lwr mean_ci_upr 
        -4.853102    7.853102 
      attr(,"label")
      [1] "Mean 95% CI"
      
      $mean_sei
      mean_sei_lwr mean_sei_upr 
                 1            2 
      attr(,"label")
      [1] "Mean -/+ 1xSE"
      
      $mean_sdi
      mean_sdi_lwr mean_sdi_upr 
         0.7928932    2.2071068 
      attr(,"label")
      [1] "Mean -/+ 1xSD"
      
      $mean_pval
        p_value 
      0.2048328 
      attr(,"label")
      [1] "Mean p-value (H0: mean = 0)"
      
      $median
      median 
         1.5 
      
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
                  1             2 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $iqr
      iqr 
        1 
      
      $range
      min max 
        1   2 
      
      $min
      min 
        1 
      
      $max
      max 
        2 
      
      $median_range
      median    min    max 
         1.5    1.0    2.0 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $cv
            cv 
      47.14045 
      
      $geom_mean
      geom_mean 
       1.414214 
      
      $geom_mean_ci
       mean_ci_lwr  mean_ci_upr 
        0.01729978 115.60839614 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
       geom_cv 
      52.10922 
      

# s_summary returns right results for n = 8

    Code
      res
    Output
      $n
      n 
      8 
      
      $sum
      sum 
       48 
      
      $mean
      mean 
         6 
      
      $sd
            sd 
      3.207135 
      
      $se
            se 
      1.133893 
      
      $mean_sd
          mean       sd 
      6.000000 3.207135 
      
      $mean_se
          mean       se 
      6.000000 1.133893 
      
      $mean_ci
      mean_ci_lwr mean_ci_upr 
         3.318768    8.681232 
      attr(,"label")
      [1] "Mean 95% CI"
      
      $mean_sei
      mean_sei_lwr mean_sei_upr 
          4.866107     7.133893 
      attr(,"label")
      [1] "Mean -/+ 1xSE"
      
      $mean_sdi
      mean_sdi_lwr mean_sdi_upr 
          2.792865     9.207135 
      attr(,"label")
      [1] "Mean -/+ 1xSD"
      
      $mean_pval
          p_value 
      0.001133783 
      attr(,"label")
      [1] "Mean p-value (H0: mean = 0)"
      
      $median
      median 
         6.5 
      
      $mad
      mad 
        0 
      
      $median_ci
      median_ci_lwr median_ci_upr 
                  1            10 
      attr(,"conf_level")
      [1] 0.9921875
      attr(,"label")
      [1] "Median 95% CI"
      
      $quantiles
      quantile_0.25 quantile_0.75 
                3.5           8.5 
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $iqr
      iqr 
        5 
      
      $range
      min max 
        1  10 
      
      $min
      min 
        1 
      
      $max
      max 
       10 
      
      $median_range
      median    min    max 
         6.5    1.0   10.0 
      attr(,"label")
      [1] "Median (Min - Max)"
      
      $cv
            cv 
      53.45225 
      
      $geom_mean
      geom_mean 
       4.842534 
      
      $geom_mean_ci
      mean_ci_lwr mean_ci_upr 
         2.456211    9.547283 
      attr(,"label")
      [1] "Geometric Mean 95% CI"
      
      $geom_cv
       geom_cv 
      96.61307 
      

# s_summary works with factors

    Code
      res
    Output
      $n
      [1] 9
      
      $count
      $count$Female
      [1] 2
      
      $count$Male
      [1] 3
      
      $count$Unknown
      [1] 4
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.0000000 0.2222222
      
      $count_fraction$Male
      [1] 3.0000000 0.3333333
      
      $count_fraction$Unknown
      [1] 4.0000000 0.4444444
      
      
      $n_blq
      [1] 0
      

# s_summary works with factors with NA values handled and correctly removes them by default

    Code
      res
    Output
      $n
      [1] 9
      
      $count
      $count$Female
      [1] 2
      
      $count$Male
      [1] 3
      
      $count$Unknown
      [1] 4
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.0000000 0.2222222
      
      $count_fraction$Male
      [1] 3.0000000 0.3333333
      
      $count_fraction$Unknown
      [1] 4.0000000 0.4444444
      
      
      $n_blq
      [1] 0
      

# s_summary works with length 0 factors that have levels

    Code
      res
    Output
      $n
      [1] 0
      
      $count
      $count$a
      [1] 0
      
      $count$b
      [1] 0
      
      $count$c
      [1] 0
      
      
      $count_fraction
      $count_fraction$a
      [1] 0 0
      
      $count_fraction$b
      [1] 0 0
      
      $count_fraction$c
      [1] 0 0
      
      
      $n_blq
      [1] 0
      

# s_summary works with factors and different denominator choices

    Code
      res
    Output
      $n
      [1] 9
      
      $count
      $count$Female
      [1] 2
      
      $count$Male
      [1] 3
      
      $count$Unknown
      [1] 4
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.0 0.1
      
      $count_fraction$Male
      [1] 3.00 0.15
      
      $count_fraction$Unknown
      [1] 4.0 0.2
      
      
      $n_blq
      [1] 0
      

---

    Code
      res
    Output
      $n
      [1] 9
      
      $count
      $count$Female
      [1] 2
      
      $count$Male
      [1] 3
      
      $count$Unknown
      [1] 4
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.00000000 0.06666667
      
      $count_fraction$Male
      [1] 3.0 0.1
      
      $count_fraction$Unknown
      [1] 4.0000000 0.1333333
      
      
      $n_blq
      [1] 0
      

# s_summary works with characters by converting to character and handling empty strings

    Code
      res
    Output
      $n
      [1] 10
      
      $count
      $count$Female
      [1] 2
      
      $count$Male
      [1] 3
      
      $count$Unknown
      [1] 4
      
      $count$`<Missing>`
      [1] 1
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.0 0.2
      
      $count_fraction$Male
      [1] 3.0 0.3
      
      $count_fraction$Unknown
      [1] 4.0 0.4
      
      $count_fraction$`<Missing>`
      [1] 1.0 0.1
      
      
      $n_blq
      [1] 0
      

# s_summary works with logical vectors

    Code
      res
    Output
      $n
      [1] 6
      
      $count
      [1] 4
      
      $count_fraction
      [1] 4.0000000 0.6666667
      
      $n_blq
      [1] 0
      

# s_summary works with logical vectors and by default removes NA

    Code
      res
    Output
      $n
      [1] 6
      
      $count
      [1] 4
      
      $count_fraction
      [1] 4.0000000 0.6666667
      
      $n_blq
      [1] 0
      

# s_summary works with logical vectors and by if requested does not remove NA from n

    Code
      res
    Output
      $n
      [1] 8
      
      $count
      [1] 4
      
      $count_fraction
      [1] 4.0 0.5
      
      $n_blq
      [1] 0
      

# create_afun_summary creates an `afun` that works

    Code
      res
    Output
                               A              B          C    
      ————————————————————————————————————————————————————————
      V1                                                      
        AVAL                                                  
          n                    2              1          0    
            My median          8              3          NA   
          Min - Max        6.0 - 9.0      3.0 - 3.0      NA   
          Mean 95% CI   (-11.56, 26.56)      NA          NA   
        ARM                                                   
          n                    2              2          2    
          A                2 (100%)           0          0    
          B                    0          2 (100%)       0    
          C                    0              0       2 (100%)
      V2                                                      
        AVAL                                                  
          n                    2              1          0    
            My median          6              2          NA   
          Min - Max        5.0 - 8.0      2.0 - 2.0      NA   
          Mean 95% CI   (-12.56, 25.56)      NA          NA   
        ARM                                                   
          n                    2              2          2    
          A                2 (100%)           0          0    
          B                    0          2 (100%)       0    
          C                    0              0       2 (100%)
      V3                                                      
        AVAL                                                  
          n                    2              1          0    
            My median          6              1          NA   
          Min - Max        4.0 - 7.0      1.0 - 1.0      NA   
          Mean 95% CI   (-13.56, 24.56)      NA          NA   
        ARM                                                   
          n                    2              2          2    
          A                2 (100%)           0          0    
          B                    0          2 (100%)       0    
          C                    0              0       2 (100%)

# `summarize_vars` works with healthy input, default `na.rm = TRUE`.

    Code
      res
    Output
                   all obs 
      —————————————————————
      n               4    
      Mean (SD)   2.5 (1.3)
      Median         2.5   
      Min - Max   1.0 - 4.0

# `summarize_vars` works with healthy input, and control function.

    Code
      res
    Output
                          all obs   
      ——————————————————————————————
      n                      9      
      Mean (SD)          5.0 (2.7)  
      Mean (SE)          5.0 (0.9)  
      Mean 90% CI       (3.30, 6.70)
      10% and 90%-ile    1.0 - 9.0  

# `summarize_vars` works with healthy input, alternative `na.rm = FALSE`

    Code
      res
    Output
                  all obs
      ———————————————————
      n              6   
      Mean (SD)     NA   
      Median        NA   
      Min - Max     NA   

# `summarize_vars` works with healthy factor input

    Code
      res
    Output
           all obs 
      —————————————
      n       3    
      a   2 (66.7%)
      b   1 (33.3%)

# `summarize_vars` works with healthy factor input, alternative `na.rm = FALSE`

    Code
      res
    Output
                  all obs
      ———————————————————
      n              5   
      a           2 (40%)
      b           1 (20%)
      <Missing>   2 (40%)

# `summarize_vars` works with factors and different denominators

    Code
      res
    Output
                                                    A: Drug X    B: Placebo   C: Combination
                                                     (N=121)      (N=106)        (N=129)    
      ——————————————————————————————————————————————————————————————————————————————————————
      F (N=187)                                                                             
        n                                               70           56             61      
        ASIAN                                       44 (23.5%)   37 (19.8%)     40 (21.4%)  
        BLACK OR AFRICAN AMERICAN                   18 (9.6%)    12 (6.4%)       13 (7%)    
        WHITE                                        8 (4.3%)     7 (3.7%)       8 (4.3%)   
        AMERICAN INDIAN OR ALASKA NATIVE                0            0              0       
        MULTIPLE                                        0            0              0       
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER       0            0              0       
        OTHER                                           0            0              0       
        UNKNOWN                                         0            0              0       
      M (N=169)                                                                             
        n                                               51           50             68      
        ASIAN                                       35 (20.7%)   31 (18.3%)      44 (26%)   
        BLACK OR AFRICAN AMERICAN                   10 (5.9%)    12 (7.1%)      14 (8.3%)   
        WHITE                                        6 (3.6%)     7 (4.1%)      10 (5.9%)   
        AMERICAN INDIAN OR ALASKA NATIVE                0            0              0       
        MULTIPLE                                        0            0              0       
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER       0            0              0       
        OTHER                                           0            0              0       
        UNKNOWN                                         0            0              0       

# summarize_vars works in demographic table example

    Code
      res
    Output
                                                  A: Drug X    B: Placebo   C: Combination
      ————————————————————————————————————————————————————————————————————————————————————
      ASIAN                                                                               
        CHN                                       39 (49.4%)   32 (47.1%)     39 (46.4%)  
        USA                                        7 (8.9%)    9 (13.2%)      13 (15.5%)  
        BRA                                        6 (7.6%)    8 (11.8%)       6 (7.1%)   
        PAK                                        6 (7.6%)     5 (7.4%)      9 (10.7%)   
        NGA                                       9 (11.4%)     3 (4.4%)       8 (9.5%)   
        RUS                                        7 (8.9%)     4 (5.9%)       4 (4.8%)   
        JPN                                        2 (2.5%)     6 (8.8%)       3 (3.6%)   
        GBR                                        1 (1.3%)     1 (1.5%)       1 (1.2%)   
        CAN                                        2 (2.5%)        0           1 (1.2%)   
        CHE                                           0            0              0       
      BLACK OR AFRICAN AMERICAN                                                           
        CHN                                        14 (50%)    10 (41.7%)     18 (66.7%)  
        USA                                       4 (14.3%)    3 (12.5%)      3 (11.1%)   
        BRA                                       3 (10.7%)    4 (16.7%)       1 (3.7%)   
        PAK                                        1 (3.6%)     2 (8.3%)       2 (7.4%)   
        NGA                                           0         1 (4.2%)       1 (3.7%)   
        RUS                                        1 (3.6%)     1 (4.2%)          0       
        JPN                                       3 (10.7%)        0           1 (3.7%)   
        GBR                                        1 (3.6%)     2 (8.3%)       1 (3.7%)   
        CAN                                        1 (3.6%)     1 (4.2%)          0       
        CHE                                           0            0              0       
      WHITE                                                                               
        CHN                                       9 (64.3%)    6 (42.9%)      12 (66.7%)  
        USA                                       2 (14.3%)    2 (14.3%)       1 (5.6%)   
        BRA                                           0         1 (7.1%)          0       
        PAK                                        1 (7.1%)     1 (7.1%)       1 (5.6%)   
        NGA                                        1 (7.1%)     1 (7.1%)          0       
        RUS                                        1 (7.1%)        0          2 (11.1%)   
        JPN                                           0        2 (14.3%)       1 (5.6%)   
        GBR                                           0            0              0       
        CAN                                           0         1 (7.1%)       1 (5.6%)   
        CHE                                           0            0              0       
      AMERICAN INDIAN OR ALASKA NATIVE                                                    
        CHN                                           0            0              0       
        USA                                           0            0              0       
        BRA                                           0            0              0       
        PAK                                           0            0              0       
        NGA                                           0            0              0       
        RUS                                           0            0              0       
        JPN                                           0            0              0       
        GBR                                           0            0              0       
        CAN                                           0            0              0       
        CHE                                           0            0              0       
      MULTIPLE                                                                            
        CHN                                           0            0              0       
        USA                                           0            0              0       
        BRA                                           0            0              0       
        PAK                                           0            0              0       
        NGA                                           0            0              0       
        RUS                                           0            0              0       
        JPN                                           0            0              0       
        GBR                                           0            0              0       
        CAN                                           0            0              0       
        CHE                                           0            0              0       
      NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER                                           
        CHN                                           0            0              0       
        USA                                           0            0              0       
        BRA                                           0            0              0       
        PAK                                           0            0              0       
        NGA                                           0            0              0       
        RUS                                           0            0              0       
        JPN                                           0            0              0       
        GBR                                           0            0              0       
        CAN                                           0            0              0       
        CHE                                           0            0              0       
      OTHER                                                                               
        CHN                                           0            0              0       
        USA                                           0            0              0       
        BRA                                           0            0              0       
        PAK                                           0            0              0       
        NGA                                           0            0              0       
        RUS                                           0            0              0       
        JPN                                           0            0              0       
        GBR                                           0            0              0       
        CAN                                           0            0              0       
        CHE                                           0            0              0       
      UNKNOWN                                                                             
        CHN                                           0            0              0       
        USA                                           0            0              0       
        BRA                                           0            0              0       
        PAK                                           0            0              0       
        NGA                                           0            0              0       
        RUS                                           0            0              0       
        JPN                                           0            0              0       
        GBR                                           0            0              0       
        CAN                                           0            0              0       
        CHE                                           0            0              0       

# `summarize_vars` works with logical input

    Code
      res
    Output
                       all obs
      ————————————————————————
      n                   5   
      count_fraction   3 (60%)

# `summarize_vars` works with empty named numeric variables

    Code
      res
    Output
                  a        b           c    
      ——————————————————————————————————————
      n           0        2           2    
      Mean (SD)   NA   3.5 (0.7)   5.5 (0.7)
      Median      NA      3.5         5.5   
      Min - Max   NA   3.0 - 4.0   5.0 - 6.0

