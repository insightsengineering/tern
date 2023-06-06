# s_surv_timepoint works with default arguments

    Code
      res
    Output
      $pt_at_risk
      [1] 60
      attr(,"label")
      [1] "Patients remaining at risk"
      
      $event_free_rate
      [1] 82.19178
      attr(,"label")
      [1] "Event Free Rate (%)"
      
      $rate_se
      [1] 4.477783
      attr(,"label")
      [1] "Standard Error of Event Free Rate"
      
      $rate_ci
      [1] 73.41549 90.96807
      attr(,"label")
      [1] "95% CI"
      

# s_surv_timepoint works with customized arguments

    Code
      res
    Output
      $pt_at_risk
      [1] 42
      attr(,"label")
      [1] "Patients remaining at risk"
      
      $event_free_rate
      [1] 75.49797
      attr(,"label")
      [1] "Event Free Rate (%)"
      
      $rate_se
      [1] 5.698695
      attr(,"label")
      [1] "Standard Error of Event Free Rate"
      
      $rate_ci
      [1] 62.15793 91.70097
      attr(,"label")
      [1] "99% CI"
      

# s_surv_timepoint also works when there are 0 patients at risk

    Code
      res
    Output
      $pt_at_risk
      [1] NA
      attr(,"label")
      [1] "Patients remaining at risk"
      
      $event_free_rate
      [1] NA
      attr(,"label")
      [1] "Event Free Rate (%)"
      
      $rate_se
      [1] NA
      attr(,"label")
      [1] "Standard Error of Event Free Rate"
      
      $rate_ci
      [1] NA NA
      attr(,"label")
      [1] "95% CI"
      

# surv_timepoint works with default arguments

    Code
      res
    Output
                                         ARM A            ARM B            ARM C     
      ———————————————————————————————————————————————————————————————————————————————
      6 Months                                                                       
        Patients remaining at risk         56               60               43      
        Event Free Rate (%)              86.46            82.19            77.30     
        95% CI                       (78.22, 94.70)   (73.42, 90.97)   (66.43, 88.16)

# surv_timepoint works with customized arguments

    Code
      res
    Output
                                         ARM A            ARM B            ARM C     
      ———————————————————————————————————————————————————————————————————————————————
      8 Months                                                                       
        Patients remaining at risk         54               54               41      
        Event Free Rate (%)              84.89            75.25            73.70     
        90% CI                       (75.88, 90.73)   (65.73, 82.47)   (62.64, 81.95)

# s_surv_timepoint_diff works with default arguments for comparison group

    Code
      res
    Output
      $rate_diff
      [1] 4.2691
      attr(,"label")
      [1] "Difference in Event Free Rate"
      
      $rate_diff_ci
      [1] -7.767371 16.305570
      attr(,"label")
      [1] "95% CI"
      
      $ztest_pval
      [1] 0.4869546
      attr(,"label")
      [1] "p-value (Z-test)"
      

# s_surv_timepoint_diff works with customized arguments for comparison arm

    Code
      res
    Output
      $rate_diff
      [1] 9.640872
      attr(,"label")
      [1] "Difference in Event Free Rate"
      
      $rate_diff_ci
      [1] -1.404177 20.685921
      attr(,"label")
      [1] "90% CI"
      
      $ztest_pval
      [1] 0.1510762
      attr(,"label")
      [1] "p-value (Z-test)"
      

# surv_timepoint for survival diff works with default arguments

    Code
      res
    Output
                                        ARM A       ARM B            ARM C     
      —————————————————————————————————————————————————————————————————————————
      9 Months                                                                 
        Difference in Event Free Rate               -9.64            -13.03    
          95% CI                                (-22.80, 3.52)   (-27.59, 1.53)
          p-value (Z-test)                          0.1511           0.0794    

# surv_timepoint for survival diff works with customized arguments

    Code
      res
    Output
                                        ARM A       ARM B            ARM C     
      —————————————————————————————————————————————————————————————————————————
      9 Months                                                                 
        Difference in Event Free Rate               -9.64            -13.03    
          99% CI                                (-26.94, 7.66)   (-32.17, 6.10)
          p-value (Z-test)                          0.1511           0.0794    

# surv_timepoint works with method = both

    Code
      result
    Output
                                            ARM A            ARM B            ARM C     
                                            (N=69)           (N=73)           (N=58)    
      ——————————————————————————————————————————————————————————————————————————————————
      9 Months                                                                          
        Patients remaining at risk            53               53               39      
        Event Free Rate (%)                 84.89            75.25            71.86     
        95% CI                          (76.24, 93.53)   (65.32, 85.17)   (60.14, 83.57)
        Difference in Event Free Rate                        -9.64            -13.03    
          95% CI                                         (-22.80, 3.52)   (-27.59, 1.53)
          p-value (Z-test)                                   0.1511           0.0794    

