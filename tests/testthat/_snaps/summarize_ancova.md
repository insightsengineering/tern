# h_ancova works with healthy input

    Code
      res
    Output
       Species    emmean     SE  df lower.CL upper.CL
       setosa       6.15 0.3371 143     5.49     6.82
       versicolor   5.72 0.0668 143     5.59     5.85
       virginica    5.41 0.1488 143     5.11     5.70
      
      Confidence level used: 0.95 

# s_ancova works with healthy input

    Code
      res
    Output
      $n
      [1] 50
      
      $lsmean
      [1] 5.717409
      attr(,"label")
      [1] "Adjusted Mean"
      
      $lsmean_diff
      [1] -0.4374138
      attr(,"label")
      [1] "Difference in Adjusted Means"
      
      $lsmean_diff_ci
      [1] -1.4268150  0.5519873
      attr(,"label")
      [1] "99% CI"
      
      $pval
      [1] 0.2503574
      attr(,"label")
      [1] "p-value"
      

# s_ancova works with interaction and .in_ref_col = TRUE

    Code
      res
    Output
      $n
      [1] 20
      
      $lsmean
      [1] 4.362642
      attr(,"label")
      [1] "Adjusted Mean"
      
      $lsmean_diff
      character(0)
      attr(,"label")
      [1] "Difference in Adjusted Means"
      
      $lsmean_diff_ci
      character(0)
      attr(,"label")
      [1] "95% CI"
      
      $pval
      character(0)
      attr(,"label")
      [1] "p-value"
      

# summarize_ancova works with healthy inputs

    Code
      res
    Output
                                                      setosa     versicolor       virginica   
                                                      (N=50)       (N=50)           (N=50)    
      ————————————————————————————————————————————————————————————————————————————————————————
      Unadjusted comparison                                                                   
        n                                               50           50               50      
        Mean                                           5.01         5.94             6.59     
        Difference in Means                                         0.93             1.58     
          95% CI                                                (0.73, 1.13)     (1.38, 1.79) 
          p-value                                                 <0.0001          <0.0001    
      Adjusted comparison (covariates Petal.Length)                                           
        n                                               50           50               50      
        Adjusted Mean                                  7.08         5.48             4.97     
        Difference in Adjusted Means                               -1.60            -2.12     
          95% CI                                               (-1.98, -1.22)   (-2.66, -1.58)
          p-value                                                 <0.0001          <0.0001    

