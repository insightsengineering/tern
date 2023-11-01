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
                                  ARM A          ARM B          ARM C    
      ———————————————————————————————————————————————————————————————————
      Survival Time (Months)                                             
        Median                     32.0           23.9           20.8    
          90% CI               (25.6, 49.3)   (18.9, 32.1)   (13.0, 26.0)
        40% and 60%-ile         25.6, 46.5     18.3, 29.2     13.0, 25.7 
        Range (censored)       0.8 to 63.5    6.2 to 78.9    3.4 to 52.4 
        Range (event)          0.3 to 155.5   0.1 to 154.1   0.6 to 80.7 
        Range                  0.3 to 155.5   0.1 to 154.1   0.6 to 80.7 

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
        Range             0.3 to 155.5 {1}   0.1 to 154.1 {2}   0.6 to 80.7 {3}   0.1 to 155.5 {4}
      ————————————————————————————————————————————————————————————————————————————————————————————
      
      {1} - Censored observation: range minimum
      {2} - Censored observations: range minimum & maximum
      {3} - Censored observation: range maximum
      {4} - Censored observation: range minimum
      ————————————————————————————————————————————————————————————————————————————————————————————
      

