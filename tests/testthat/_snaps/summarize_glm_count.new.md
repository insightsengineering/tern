# h_glm_poisson glm-fit works with healthy input

    Code
      res
    Output
           Estimate         SE    z_value            Pr             coefs
      1  2.10487843 0.07035975 29.9159442 1.220818e-196       (Intercept)
      2  0.10946932 0.09613709  1.1386793  2.548369e-01     ARMB: Placebo
      3 -0.04371457 0.10265030 -0.4258591  6.702105e-01 ARMC: Combination

# h_glm_poisson emmeans-fit works with healthy input

    Code
      res
    Output
        ARMCD     rate std.error  df null statistic       p.value
      1 ARM A 8.206105 0.5773795 Inf    1  29.91594 1.220818e-196
      2 ARM B 9.155436 0.5997925 Inf    1  33.80055 1.935734e-250
      3 ARM C 7.855107 0.5871181 Inf    1  27.57650 2.129731e-167

# h_glm_quasipoisson glm-fit works with healthy input

    Code
      res
    Output
           Estimate        SE    z_value         Pr                coefs
      1  2.01065582 0.7781805  2.5837912 0.01051488          (Intercept)
      2  0.07631174 0.7510804  0.1016026 0.91917812          REGION1Asia
      3  0.64425750 0.9396557  0.6856314 0.49377257       REGION1Eurasia
      4  2.13096720 1.5327784  1.3902643 0.16605816        REGION1Europe
      5 -0.07449500 0.8525865 -0.0873753 0.93046426 REGION1North America
      6  0.38101695 0.9046241  0.4211881 0.67408881 REGION1South America
      7  0.11047866 0.4143377  0.2666392 0.79003312        ARMB: Placebo
      8 -0.17694419 0.4563327 -0.3877526 0.69862863    ARMC: Combination

# h_glm_quasipoisson emmeans-fit works with healthy input

    Code
      res
    Output
                   ARM     rate std.error  df null statistic      p.value
      1      A: Drug X 12.64167  5.195162 Inf    1  6.173420 6.682841e-10
      2     B: Placebo 14.11838  5.392442 Inf    1  6.931571 4.161914e-12
      3 C: Combination 10.59153  4.074355 Inf    1  6.135104 8.510352e-10

# h_glm_count glm-fit works with healthy input

    Code
      res
    Output
           Estimate         SE    z_value            Pr       coefs
      1  2.10487843 0.07035975 29.9159442 1.220818e-196 (Intercept)
      2  0.10946932 0.09613709  1.1386793  2.548369e-01  ARMCDARM B
      3 -0.04371457 0.10265030 -0.4258591  6.702105e-01  ARMCDARM C

# h_glm_count emmeans-fit works with healthy input

    Code
      res
    Output
        ARMCD     rate std.error  df null statistic       p.value
      1 ARM A 8.206105 0.5773795 Inf    1  29.91594 1.220818e-196
      2 ARM B 9.155436 0.5997925 Inf    1  33.80055 1.935734e-250
      3 ARM C 7.855107 0.5871181 Inf    1  27.57650 2.129731e-167

# h_ppmeans works with healthy input

    Code
      res
    Output
                     rate asymp.LCL asymp.UCL            ARM
      A: Drug X      3.07  2.202774  4.278651      A: Drug X
      B: Placebo     3.07  2.202774  4.278651     B: Placebo
      C: Combination 3.07  2.202774  4.278651 C: Combination

# s_glm_count works with healthy input

    Code
      res
    Output
      $n
      [1] 73
      
      $rate
      [1] 3.486005
      attr(,"label")
      [1] "Adjusted Rate"
      
      $rate_ci
      [1] 1.983340 6.127155
      attr(,"label")
      [1] "95% CI"
      
      $rate_ratio
      character(0)
      attr(,"label")
      [1] "Adjusted Rate Ratio"
      
      $rate_ratio_ci
      character(0)
      attr(,"label")
      [1] "95% CI"
      
      $pval
      character(0)
      attr(,"label")
      [1] "p-value"
      

# s_glm_count works with no reference group selected.

    Code
      res
    Output
      $n
      [1] 73
      
      $rate
      [1] 3.486005
      attr(,"label")
      [1] "Adjusted Rate"
      
      $rate_ci
      [1] 1.983340 6.127155
      attr(,"label")
      [1] "95% CI"
      
      $rate_ratio
      [1] 0.8954054 0.7501944
      attr(,"label")
      [1] "Adjusted Rate Ratio"
      
      $rate_ratio_ci
      [1] 0.3974979 0.3067371 2.0169939 1.8347687
      attr(,"label")
      [1] "95% CI"
      
      $pval
      [1] 0.7897470 0.5287652
      attr(,"label")
      [1] "p-value"
      

# summarize_glm_count works with healthy inputs

    Code
      res
    Output
                                                B: Placebo     A: Drug X    C: Combination
                                                  (N=73)        (N=69)          (N=58)    
      ————————————————————————————————————————————————————————————————————————————————————
      Number of exacerbations per patient                                                 
        0                                       8 (10.96%)     3 (4.35%)      6 (10.34%)  
        1                                       9 (12.33%)    11 (15.94%)     6 (10.34%)  
        2                                       15 (20.55%)   18 (26.09%)     9 (15.52%)  
        3                                       11 (15.07%)   14 (20.29%)    15 (25.86%)  
        4                                       9 (12.33%)    10 (14.49%)     9 (15.52%)  
        5                                       9 (12.33%)    7 (10.14%)      8 (13.79%)  
        6                                        4 (5.48%)     4 (5.80%)      4 (6.90%)   
        7                                       8 (10.96%)     2 (2.90%)      0 (0.00%)   
        10                                       0 (0.00%)     0 (0.00%)      1 (1.72%)   
      Unadjusted exacerbation rate (per year)                                             
        Rate                                      9.1554        8.2061          7.8551    

