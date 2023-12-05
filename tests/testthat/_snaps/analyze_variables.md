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
      

# s_summary works when factors have NA levels

    Code
      res
    Output
      $n
      [1] 7
      
      $count
      $count$Female
      [1] 2
      
      $count$Male
      [1] 2
      
      $count$Unknown
      [1] 2
      
      $count$`NA`
      [1] 1
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.0000000 0.2857143
      
      $count_fraction$Male
      [1] 2.0000000 0.2857143
      
      $count_fraction$Unknown
      [1] 2.0000000 0.2857143
      
      $count_fraction$`NA`
      [1] 1.0000000 0.1428571
      
      
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
      
      $count$`NA`
      [1] 1
      
      
      $count_fraction
      $count_fraction$Female
      [1] 2.0 0.2
      
      $count_fraction$Male
      [1] 3.0 0.3
      
      $count_fraction$Unknown
      [1] 4.0 0.4
      
      $count_fraction$`NA`
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
      

# s_summary works with length 0 logical vectors

    Code
      res
    Output
      $n
      [1] 0
      
      $count
      [1] 0
      
      $count_fraction
      [1] 0 0
      
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
      

# a_summary work with healthy input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                            row_name   formatted_cell indent_mod                   row_label
      1                            n               10          0                           n
      2                          Sum              1.3          0                         Sum
      3                         Mean              0.1          0                        Mean
      4                           SD              0.8          0                          SD
      5                           SE              0.2          0                          SE
      6                    Mean (SD)        0.1 (0.8)          0                   Mean (SD)
      7                    Mean (SE)        0.1 (0.2)          0                   Mean (SE)
      8                  Mean 95% CI    (-0.43, 0.69)          0                 Mean 95% CI
      9                Mean -/+ 1xSE    (-0.11, 0.38)          0               Mean -/+ 1xSE
      10               Mean -/+ 1xSD    (-0.65, 0.91)          0               Mean -/+ 1xSD
      11 Mean p-value (H0: mean = 0)             0.61          0 Mean p-value (H0: mean = 0)
      12                      Median              0.3          0                      Median
      13   Median Absolute Deviation             -0.0          0   Median Absolute Deviation
      14               Median 95% CI    (-0.82, 0.74)          0               Median 95% CI
      15             25% and 75%-ile       -0.6 - 0.6          0             25% and 75%-ile
      16                         IQR              1.2          0                         IQR
      17                   Min - Max       -0.8 - 1.6          0                   Min - Max
      18                     Minimum             -0.8          0                     Minimum
      19                     Maximum              1.6          0                     Maximum
      20          Median (Min - Max) 0.3 (-0.8 - 1.6)          0          Median (Min - Max)
      21                      CV (%)            590.4          0                      CV (%)
      22              Geometric Mean               NA          0              Geometric Mean
      23       Geometric Mean 95% CI               NA          0       Geometric Mean 95% CI
      24         CV % Geometric Mean               NA          0         CV % Geometric Mean

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
         row_name formatted_cell indent_mod row_label
      1         n              5          0         n
      2         a              3          0         a
      3         b              1          0         b
      4         c              1          0         c
      5         a        3 (60%)          0         a
      6         b        1 (20%)          0         b
      7         c        1 (20%)          0         c
      8         a      3 (60.0%)          0         a
      9         b      1 (20.0%)          0         b
      10        c      1 (20.0%)          0         c
      11    n_blq              0          0     n_blq

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
         row_name formatted_cell indent_mod row_label
      1         n              4          0         n
      2         A              2          0         A
      3         B              1          0         B
      4         C              1          0         C
      5         A        2 (50%)          0         A
      6         B        1 (25%)          0         B
      7         C        1 (25%)          0         C
      8         A      2 (50.0%)          0         A
      9         B      1 (25.0%)          0         B
      10        C      1 (25.0%)          0         C
      11    n_blq              0          0     n_blq

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
              row_name formatted_cell indent_mod      row_label
      1              n              5          0              n
      2          count              3          0          count
      3 count_fraction        3 (60%)          0 count_fraction
      4 count_fraction      3 (60.0%)          0 count_fraction
      5          n_blq              0          0          n_blq

# a_summary works with custom input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
             row_name formatted_cell indent_mod     row_label
      1      std. dev              1          3      std. dev
      2 Median 95% CI   -0.62 - 1.12          3 Median 95% CI

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                  row_name formatted_cell indent_mod         row_label
      1  number of records           5.00         -1 number of records
      2                  a              2          5                 a
      3                  b              1          5                 b
      4                  c              1          5                 c
      5                 NA              1          5                NA
      6                  a        2 (40%)          0                 a
      7                  b        1 (20%)          0                 b
      8                  c        1 (20%)          0                 c
      9                 NA        1 (20%)          0                NA
      10                 a      2 (40.0%)          0                 a
      11                 b      1 (20.0%)          0                 b
      12                 c      1 (20.0%)          0                 c
      13                NA      1 (20.0%)          0                NA
      14             n_blq              0          0             n_blq

# a_summary works with healthy input when compare = TRUE.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                            row_name  formatted_cell indent_mod                   row_label
      1                            n              10          0                           n
      2                          Sum            51.3          0                         Sum
      3                         Mean             5.1          0                        Mean
      4                           SD             0.8          0                          SD
      5                           SE             0.2          0                          SE
      6                    Mean (SD)       5.1 (0.8)          0                   Mean (SD)
      7                    Mean (SE)       5.1 (0.2)          0                   Mean (SE)
      8                  Mean 95% CI    (4.57, 5.69)          0                 Mean 95% CI
      9                Mean -/+ 1xSE    (4.89, 5.38)          0               Mean -/+ 1xSE
      10               Mean -/+ 1xSD    (4.35, 5.91)          0               Mean -/+ 1xSD
      11 Mean p-value (H0: mean = 0)            0.00          0 Mean p-value (H0: mean = 0)
      12                      Median             5.3          0                      Median
      13   Median Absolute Deviation            -0.0          0   Median Absolute Deviation
      14               Median 95% CI    (4.18, 5.74)          0               Median 95% CI
      15             25% and 75%-ile       4.4 - 5.6          0             25% and 75%-ile
      16                         IQR             1.2          0                         IQR
      17                   Min - Max       4.2 - 6.6          0                   Min - Max
      18                     Minimum             4.2          0                     Minimum
      19                     Maximum             6.6          0                     Maximum
      20          Median (Min - Max) 5.3 (4.2 - 6.6)          0          Median (Min - Max)
      21                      CV (%)            15.2          0                      CV (%)
      22              Geometric Mean             5.1          0              Geometric Mean
      23       Geometric Mean 95% CI    (4.56, 5.66)          0       Geometric Mean 95% CI
      24         CV % Geometric Mean            15.2          0         CV % Geometric Mean
      25            p-value (t-test)         <0.0001          0            p-value (t-test)

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                           row_name formatted_cell indent_mod                  row_label
      1                           n              5          0                          n
      2                           a              3          0                          a
      3                           b              1          0                          b
      4                           c              1          0                          c
      5                           a        3 (60%)          0                          a
      6                           b        1 (20%)          0                          b
      7                           c        1 (20%)          0                          c
      8                           a      3 (60.0%)          0                          a
      9                           b      1 (20.0%)          0                          b
      10                          c      1 (20.0%)          0                          c
      11                      n_blq              0          0                      n_blq
      12 p-value (chi-squared test)         0.9560          0 p-value (chi-squared test)

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                           row_name formatted_cell indent_mod                  row_label
      1                           n              4          0                          n
      2                           A              2          0                          A
      3                           B              1          0                          B
      4                           C              1          0                          C
      5                           A        2 (50%)          0                          A
      6                           B        1 (25%)          0                          B
      7                           C        1 (25%)          0                          C
      8                           A      2 (50.0%)          0                          A
      9                           B      1 (25.0%)          0                          B
      10                          C      1 (25.0%)          0                          C
      11                      n_blq              0          0                      n_blq
      12 p-value (chi-squared test)         0.9074          0 p-value (chi-squared test)

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                          row_name formatted_cell indent_mod                  row_label
      1                          n              5          0                          n
      2                      count              3          0                      count
      3             count_fraction        3 (60%)          0             count_fraction
      4             count_fraction      3 (60.0%)          0             count_fraction
      5                      n_blq              0          0                      n_blq
      6 p-value (chi-squared test)         0.8091          0 p-value (chi-squared test)

# a_summary works with custom input when compare = TRUE.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
             row_name formatted_cell indent_mod     row_label
      1        pvalue        <0.0001          3        pvalue
      2 Median 95% CI   -0.41 - 1.10          3 Median 95% CI

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                           row_name formatted_cell indent_mod                  row_label
      1           number of records           5.00         -1          number of records
      2                           a              2          5                          a
      3                           b              1          5                          b
      4                           c              1          5                          c
      5                          NA              1          5                         NA
      6                           a        2 (40%)          0                          a
      7                           b        1 (20%)          0                          b
      8                           c        1 (20%)          0                          c
      9                          NA        1 (20%)          0                         NA
      10                          a      2 (40.0%)          0                          a
      11                          b      1 (20.0%)          0                          b
      12                          c      1 (20.0%)          0                          c
      13                         NA      1 (20.0%)          0                         NA
      14                      n_blq              0          0                      n_blq
      15 p-value (chi-squared test)         0.8254          0 p-value (chi-squared test)

# `analyze_vars` works with healthy input, default `na.rm = TRUE`.

    Code
      res
    Output
                   all obs 
      —————————————————————
      n               4    
      Mean (SD)   2.5 (1.3)
      Median         2.5   
      Min - Max   1.0 - 4.0

# `analyze_vars` works with healthy input, and control function.

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

# `analyze_vars` works with healthy input, alternative `na.rm = FALSE`

    Code
      res
    Output
                  all obs
      ———————————————————
      n              6   
      Mean (SD)     NA   
      Median        NA   
      Min - Max     NA   

# `analyze_vars` works with healthy factor input

    Code
      res
    Output
           all obs 
      —————————————
      n       3    
      a   2 (66.7%)
      b   1 (33.3%)

# `analyze_vars` works with healthy factor input, alternative `na.rm = FALSE`

    Code
      res
    Output
           all obs
      ————————————
      n       5   
      a    2 (40%)
      b    1 (20%)
      NA   2 (40%)

---

    Code
      res
    Output
                  all obs
      ———————————————————
      n              5   
      a           2 (40%)
      b           1 (20%)
      <Missing>   2 (40%)

# `analyze_vars` works with factors and different denominators

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

# analyze_vars works in demographic table example

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

# `analyze_vars` works with logical input

    Code
      res
    Output
                       all obs
      ————————————————————————
      n                   5   
      count_fraction   3 (60%)

# `analyze_vars` works with healthy logical input, alternative `na.rm = FALSE`

    Code
      res
    Output
              all obs
      ———————————————
      n          5   
      FALSE   1 (20%)
      TRUE    2 (40%)
      NA      2 (40%)

---

    Code
      res
    Output
                  all obs
      ———————————————————
      n              5   
      FALSE       1 (20%)
      TRUE        2 (40%)
      <Missing>   2 (40%)

# `analyze_vars` works with empty named numeric variables

    Code
      res
    Output
                  a        b           c    
      ——————————————————————————————————————
      n           0        2           2    
      Mean (SD)   NA   3.5 (0.7)   5.5 (0.7)
      Median      NA      3.5         5.5   
      Min - Max   NA   3.0 - 4.0   5.0 - 6.0

# analyze_vars 'na_str' argument works as expected

    Code
      res
    Output
                        A           B       C
      ———————————————————————————————————————
      V1                                     
        n               2           1       0
        Mean (SD)   7.5 (2.1)    3.0 (-)    -
        Median         7.5         3.0      -
        Min - Max   6.0 - 9.0   3.0 - 3.0   -
      V2                                     
        n               2           1       0
        Mean (SD)   6.5 (2.1)    2.0 (-)    -
        Median         6.5         2.0      -
        Min - Max   5.0 - 8.0   2.0 - 2.0   -
      V3                                     
        n               2           1       0
        Mean (SD)   5.5 (2.1)    1.0 (-)    -
        Median         5.5         1.0      -
        Min - Max   4.0 - 7.0   1.0 - 1.0   -

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
      

# analyze_vars works correctly with auto formats

    Code
      res
    Output
                       all obs     
      —————————————————————————————
      n                   5        
      Mean               1.4       
      Mean (SD)   1.44042 (1.91481)
      Min - Max    0.0010 - 4.0000 

