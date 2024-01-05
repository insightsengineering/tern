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

# h_glm_poisson glm-fit works with healthy input with covariates

    Code
      res
    Output
           Estimate         SE    z_value           Pr                coefs
      1  2.01065582 0.18541942 10.8438255 2.133586e-27          (Intercept)
      2  0.07631174 0.17896220  0.4264126 6.698072e-01          REGION1Asia
      3  0.64425750 0.22389462  2.8775033 4.008358e-03       REGION1Eurasia
      4  2.13096720 0.36521976  5.8347533 5.387022e-09        REGION1Europe
      5 -0.07449500 0.20314837 -0.3667024 7.138410e-01 REGION1North America
      6  0.38101695 0.21554753  1.7676703 7.711605e-02 REGION1South America
      7  0.11047866 0.09872549  1.1190490 2.631192e-01        ARMB: Placebo
      8 -0.17694419 0.10873176 -1.6273459 1.036637e-01    ARMC: Combination

# h_glm_poisson emmeans-fit works with healthy input with covariates

    Code
      res
    Output
        ARMCD     rate std.error  df null statistic       p.value
      1 ARM A 12.64167 1.2378669 Inf    1  25.90902 5.270655e-148
      2 ARM B 14.11838 1.2848735 Inf    1  29.09088 4.682722e-186
      3 ARM C 10.59153 0.9708089 Inf    1  25.74821 3.375733e-146

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

# h_glm_negbin glm-fit works with healthy input

    Code
      res
    Output
           Estimate        SE    z_value           Pr                coefs
      1 1.005041594 0.1992268 5.04471149 4.542062e-07          (Intercept)
      2 0.007741431 0.1919877 0.04032253 9.678360e-01          REGION1Asia
      3 0.317703043 0.2360653 1.34582686 1.783584e-01       REGION1Eurasia
      4 0.591541717 0.4058327 1.45759983 1.449509e-01        REGION1Europe
      5 0.117240049 0.2196300 0.53380718 5.934749e-01 REGION1North America
      6 0.139971334 0.2348685 0.59595610 5.512046e-01 REGION1South America
      7 0.113082781 0.1056295 1.07056107 2.843668e-01        ARMB: Placebo
      8 0.026817451 0.1131811 0.23694292 8.127011e-01    ARMC: Combination

# h_glm_negbin emmeans-fit works with healthy input

    Code
      res
    Output
                   ARM response std.error  df null statistic      p.value
      1      A: Drug X 3.322579 0.3367532 Inf    1  11.84712 2.227054e-32
      2     B: Placebo 3.720373 0.3782682 Inf    1  12.92183 3.390023e-38
      3 C: Combination 3.412887 0.3424577 Inf    1  12.23369 2.054037e-34

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
      fits
    Output
      $glm_fit
      
      Call:  stats::glm(formula = formula, family = stats::poisson(link = "log"), 
          data = .df_row, offset = offset)
      
      Coefficients:
               (Intercept)           REGION1Asia        REGION1Eurasia  
                   2.01066               0.07631               0.64426  
             REGION1Europe  REGION1North America  REGION1South America  
                   2.13097              -0.07450               0.38102  
                ARMCDARM B            ARMCDARM C  
                   0.11048              -0.17694  
      
      Degrees of Freedom: 199 Total (i.e. Null);  192 Residual
      Null Deviance:	    983.8 
      Residual Deviance: 939 	AIC: 1498
      
      $emmeans_fit
       ARMCD rate    SE  df asymp.LCL asymp.UCL
       ARM A 12.6 1.238 Inf     10.43      15.3
       ARM B 14.1 1.285 Inf     11.81      16.9
       ARM C 10.6 0.971 Inf      8.85      12.7
      
      Results are averaged over the levels of: REGION1 
      Confidence level used: 0.95 
      Intervals are back-transformed from the log scale 
      

---

    Code
      fits2
    Output
      $glm_fit
      
      Call:  stats::glm(formula = formula, family = stats::quasipoisson(link = "log"), 
          data = .df_row, offset = offset)
      
      Coefficients:
               (Intercept)           REGION1Asia        REGION1Eurasia  
                   2.01066               0.07631               0.64426  
             REGION1Europe  REGION1North America  REGION1South America  
                   2.13097              -0.07450               0.38102  
                ARMCDARM B            ARMCDARM C  
                   0.11048              -0.17694  
      
      Degrees of Freedom: 199 Total (i.e. Null);  192 Residual
      Null Deviance:	    983.8 
      Residual Deviance: 939 	AIC: NA
      
      $emmeans_fit
       ARMCD rate   SE  df asymp.LCL asymp.UCL
       ARM A 12.6 5.20 Inf      5.65      28.3
       ARM B 14.1 5.39 Inf      6.68      29.8
       ARM C 10.6 4.07 Inf      4.98      22.5
      
      Results are averaged over the levels of: REGION1 
      Confidence level used: 0.95 
      Intervals are back-transformed from the log scale 
      

# s_glm_count works with healthy input

    Code
      res
    Output
      $n
      [1] 73
      
      $rate
      [1] 14.11838
      attr(,"label")
      [1] "Adjusted Rate"
      
      $rate_ci
      [1] 11.81189 16.87525
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
      

# s_glm_count (negative binomial) works with healthy input

    Code
      res
    Output
      $n
      [1] 73
      
      $rate
      [1] 3.720373
      attr(,"label")
      [1] "Adjusted Rate"
      
      $rate_ci
      [1] 3.048181 4.540799
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
      [1] 14.11838
      attr(,"label")
      [1] "Adjusted Rate"
      
      $rate_ci
      [1] 11.81189 16.87525
      attr(,"label")
      [1] "95% CI"
      
      $rate_ratio
      [1] 0.8954054 0.7501944
      attr(,"label")
      [1] "Adjusted Rate Ratio"
      
      $rate_ratio_ci
      [1] 0.7378778 0.6062152 1.0865633 0.9283695
      attr(,"label")
      [1] "95% CI"
      
      $pval
      [1] 0.263119218 0.008203621
      attr(,"label")
      [1] "p-value"
      

# s_glm_count (negative binomial) works with no reference group selected.

    Code
      res
    Output
      $n
      [1] 73
      
      $rate
      [1] 3.720373
      attr(,"label")
      [1] "Adjusted Rate"
      
      $rate_ci
      [1] 3.048181 4.540799
      attr(,"label")
      [1] "95% CI"
      
      $rate_ratio
      [1] 0.8930767 0.9173508
      attr(,"label")
      [1] "Adjusted Rate Ratio"
      
      $rate_ratio_ci
      [1] 0.7260672 0.7381034 1.0985017 1.1401282
      attr(,"label")
      [1] "95% CI"
      
      $pval
      [1] 0.2843668 0.4367453
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

# summarize_glm_count (negative binomial) works with healthy inputs

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
        Rate                                      3.1918        2.9275          3.0862    

