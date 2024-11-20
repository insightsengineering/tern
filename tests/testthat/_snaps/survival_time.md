# s_surv_time works with default arguments

    Code
      res
    Output
      $median
      [1] 23.91143
      attr(,"label")
      [1] "Median"
      
      $median_ci
      [1] 18.25878 32.85945
      attr(,"label")
      [1] "95% CI"
      
      $quantiles
      [1]  9.822926 41.981812
      attr(,"label")
      [1] "25% and 75%-ile"
      
      $range_censor
      [1]  6.157775 78.913544
      attr(,"label")
      [1] "Range (censored)"
      
      $range_event
      [1]   0.07143141 154.08901021
      attr(,"label")
      [1] "Range (event)"
      
      $range
      [1]   0.07143141 154.08901021
      attr(,"label")
      [1] "Range"
      
      $median_ci_3d
      [1] 23.91143 18.25878 32.85945
      attr(,"label")
      [1] "Median (95% CI)"
      
      $quantiles_lower
      [1]  9.822926  5.628823 16.690121
      attr(,"label")
      [1] "25%-ile (95% CI)"
      
      $quantiles_upper
      [1] 41.98181 32.85945 53.41445
      attr(,"label")
      [1] "75%-ile (95% CI)"
      

# s_surv_time works with customized arguments

    Code
      res
    Output
      $median
      [1] 23.91143
      attr(,"label")
      [1] "Median"
      
      $median_ci
      [1] 13.59124 37.97055
      attr(,"label")
      [1] "99% CI"
      
      $quantiles
      [1]  6.649204 51.094870
      attr(,"label")
      [1] "20% and 80%-ile"
      
      $range_censor
      [1]  6.157775 78.913544
      attr(,"label")
      [1] "Range (censored)"
      
      $range_event
      [1]   0.07143141 154.08901021
      attr(,"label")
      [1] "Range (event)"
      
      $range
      [1]   0.07143141 154.08901021
      attr(,"label")
      [1] "Range"
      
      $median_ci_3d
      [1] 23.91143 13.59124 37.97055
      attr(,"label")
      [1] "Median (99% CI)"
      
      $quantiles_lower
      [1]  6.649204  1.887860 12.771697
      attr(,"label")
      [1] "20%-ile (99% CI)"
      
      $quantiles_upper
      [1] 51.09487 37.97055       NA
      attr(,"label")
      [1] "80%-ile (99% CI)"
      

# a_surv_time works with default arguments

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                row_name     formatted_cell indent_mod        row_label
      1           Median               24.8          0           Median
      2           95% CI       (21.1, 31.3)          0           95% CI
      3  Median (95% CI) 24.8 (21.1 - 31.3)          0  Median (95% CI)
      4  25% and 75%-ile         10.8, 47.6          0  25% and 75%-ile
      5 25%-ile (95% CI)  10.8 (6.6 - 13.4)          0 25%-ile (95% CI)
      6 75%-ile (95% CI) 47.6 (39.3 - 57.8)          0 75%-ile (95% CI)
      7 Range (censored)        0.8 to 78.9          0 Range (censored)
      8    Range (event)       0.1 to 155.5          0    Range (event)
      9            Range       0.1 to 155.5          0            Range

# a_surv_time works with customized arguments

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
               row_name               formatted_cell indent_mod       row_label
      1 median conf int (13.591239860, 37.970548966)          3 median conf int
      2 20% and 80%-ile                 6.65 / 51.09          0 20% and 80%-ile
      3           Range                 0.1 to 154.1          0           Range

# surv_time works with default arguments

    Code
      res
    Output
                                  ARM A          ARM B          ARM C    
      ———————————————————————————————————————————————————————————————————
      Survival Time (Months)                                             
        Median                     32.0           23.9           20.8    
          95% CI               (22.5, 49.3)   (18.3, 32.9)   (12.9, 26.0)
        25% and 75%-ile         17.4, 65.3     9.8, 42.0      7.3, 37.1  
        Range                  0.3 to 155.5   0.1 to 154.1   0.6 to 80.7 

# surv_time works with customized arguments

    Code
      res
    Output
                                     ARM A                ARM B                ARM C       
      —————————————————————————————————————————————————————————————————————————————————————
      Survival Time (Months)                                                               
        Median                        32.0                 23.9                 20.8       
          90% CI                  (25.6, 49.3)         (18.9, 32.1)         (13.0, 26.0)   
        Median (90% CI)        32.0 (25.6 - 49.3)   23.9 (18.9 - 32.1)   20.8 (13.0 - 26.0)
        40% and 60%-ile            25.6, 46.5           18.3, 29.2           13.0, 25.7    
        40%-ile (90% CI)       25.6 (20.7 - 33.4)   18.3 (12.8 - 23.9)   13.0 (10.1 - 24.8)
        60%-ile (90% CI)       46.5 (32.0 - 57.8)   29.2 (23.9 - 41.3)   25.7 (20.8 - 37.1)
        Range (censored)          0.8 to 63.5          6.2 to 78.9          3.4 to 52.4    
        Range (event)             0.3 to 155.5         0.1 to 154.1         0.6 to 80.7    
        Range                     0.3 to 155.5         0.1 to 154.1         0.6 to 80.7    

# surv_time works with referential footnotes

    Code
      res
    Output
                               ARM A              ARM B              ARM C              All       
      ————————————————————————————————————————————————————————————————————————————————————————————
      Time to Event                                                                               
        Median                  32.0               23.9              20.8               24.9      
          95% CI            (22.6, 53.4)       (18.3, 32.9)      (12.9, 26.0)       (21.5, 31.7)  
        25% and 75%-ile      17.4, 65.3         9.8, 42.7          7.3, 37.1         11.5, 49.3   
        Range             0.3 to 155.5 {1}   0.1 to 154.1 {2}   0.6 to 80.7 {3}   0.1 to 155.5 {1}
      ————————————————————————————————————————————————————————————————————————————————————————————
      
      {1} - Censored observation: range minimum
      {2} - Censored observations: range minimum & maximum
      {3} - Censored observation: range maximum
      ————————————————————————————————————————————————————————————————————————————————————————————
      

