# s_coxph_pairwise works with default arguments and no stratification factors

    Code
      res
    Output
      $pvalue
      [1] 0.09049511
      attr(,"label")
      [1] "p-value (log-rank)"
      
      $hr
      [1] 0.7108557
      attr(,"label")
      [1] "Hazard Ratio"
      
      $hr_ci
      [1] 0.4779138 1.0573368
      attr(,"label")
      [1] "95% CI"
      
      $hr_ci_3d
      [1] 0.7108557 0.4779138 1.0573368
      attr(,"label")
      [1] "Hazard Ratio (95% CI)"
      
      $n_tot
      [1] 142
      attr(,"label")
      [1] "Total n"
      
      $n_tot_events
      [1] 101
      attr(,"label")
      [1] "Total events"
      

# s_coxph_pairwise works with customized arguments and no stratification factors

    Code
      res
    Output
      $pvalue
      [1] 0.09203863
      attr(,"label")
      [1] "p-value (wald)"
      
      $hr
      [1] 0.7108557
      attr(,"label")
      [1] "Hazard Ratio"
      
      $hr_ci
      [1] 0.5094153 0.9919525
      attr(,"label")
      [1] "90% CI"
      
      $hr_ci_3d
      [1] 0.7108557 0.5094153 0.9919525
      attr(,"label")
      [1] "Hazard Ratio (90% CI)"
      
      $n_tot
      [1] 142
      attr(,"label")
      [1] "Total n"
      
      $n_tot_events
      [1] 101
      attr(,"label")
      [1] "Total events"
      

# s_coxph_pairwise works with default arguments and stratification factors

    Code
      res
    Output
      $pvalue
      [1] 0.03613543
      attr(,"label")
      [1] "p-value (log-rank)"
      
      $hr
      [1] 0.6251817
      attr(,"label")
      [1] "Hazard Ratio"
      
      $hr_ci
      [1] 0.4014842 0.9735181
      attr(,"label")
      [1] "95% CI"
      
      $hr_ci_3d
      [1] 0.6251817 0.4014842 0.9735181
      attr(,"label")
      [1] "Hazard Ratio (95% CI)"
      
      $n_tot
      [1] 142
      attr(,"label")
      [1] "Total n"
      
      $n_tot_events
      [1] 101
      attr(,"label")
      [1] "Total events"
      

# s_coxph_pairwise works with customized arguments and stratification factors

    Code
      res
    Output
      $pvalue
      [1] 0.03764119
      attr(,"label")
      [1] "p-value (wald)"
      
      $hr
      [1] 0.6251817
      attr(,"label")
      [1] "Hazard Ratio"
      
      $hr_ci
      [1] 0.4311132 0.9066115
      attr(,"label")
      [1] "90% CI"
      
      $hr_ci_3d
      [1] 0.6251817 0.4311132 0.9066115
      attr(,"label")
      [1] "Hazard Ratio (90% CI)"
      
      $n_tot
      [1] 142
      attr(,"label")
      [1] "Total n"
      
      $n_tot_events
      [1] 101
      attr(,"label")
      [1] "Total events"
      

# coxph_pairwise works with default arguments and no stratification factors

    Code
      res
    Output
                              ARM A      ARM B          ARM C    
      ———————————————————————————————————————————————————————————
      Unstratified Analysis                                      
        p-value (log-rank)               0.0905         0.0086   
        Hazard Ratio                      1.41           1.81    
        95% CI                        (0.95, 2.09)   (1.16, 2.84)

# coxph_pairwise works with customized arguments and no stratification factors

    Code
      res
    Output
                               ARM A      ARM B          ARM C    
      ————————————————————————————————————————————————————————————
      Unstratified Analysis                                       
        p-value (likelihood)              0.0903         0.0099   
        Hazard Ratio                       1.41           1.81    
        99% CI                         (0.83, 2.37)   (1.00, 3.27)

# coxph_pairwise works with default arguments and stratification factors

    Code
      res
    Output
                             ARM A      ARM B          ARM C    
      ——————————————————————————————————————————————————————————
      Stratified Analysis                                       
        p-value (log-rank)              0.0769         0.0058   
        Hazard Ratio                     1.44           1.89    
        95% CI                       (0.96, 2.15)   (1.19, 2.98)

# coxph_pairwise works with customized arguments and stratification factors

    Code
      res
    Output
                            ARM A       ARM B            ARM C     
      —————————————————————————————————————————————————————————————
      Stratified Analysis                                          
        Hazard Ratio                    1.600            2.049     
        99% CI                      (0.894, 2.863)   (1.092, 3.844)

# coxph_pairwise works with NA values

    Code
      result
    Output
                              ARM A   ARM B    ARM C 
      ———————————————————————————————————————————————
      Unstratified Analysis                          
        p-value (log-rank)            1.0000   1.0000
        Hazard Ratio                  empty    empty 
        95% CI                        empty    empty 

