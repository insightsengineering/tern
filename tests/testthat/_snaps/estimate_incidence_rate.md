# control_incidence_rate works with customized parameters

    Code
      res
    Output
      $conf_level
      [1] 0.9
      
      $conf_type
      [1] "exact"
      
      $input_time_unit
      [1] "month"
      
      $num_pt_year
      [1] 100
      

# h_incidence_rate_normal works as expected with healthy input

    Code
      res
    Output
      $rate
      [1] 0.01
      
      $rate_ci
      [1] -0.001630872  0.021630872
      

# h_incidence_rate_normal_log works as expected with healthy input

    Code
      res
    Output
      $rate
      [1] 0.01
      
      $rate_ci
      [1] 0.003125199 0.031997963
      

# h_incidence_rate_exact works as expected with healthy input

    Code
      res
    Output
      $rate
      [1] 0.01
      
      $rate_ci
      [1] 0.001776808 0.031478968
      

# h_incidence_rate_byar works as expected with healthy input

    Code
      res
    Output
      $rate
      [1] 0.01
      
      $rate_ci
      [1] 0.002820411 0.027609866
      

# h_incidence_rate works as expected with healthy input

    Code
      res
    Output
      $rate
      [1] 1
      
      $rate_ci
      [1] 0.3125199 3.1997963
      

# s_incidence_rate works as expected with healthy input

    Code
      res
    Output
      $person_years
      [1] 9.058333
      attr(,"label")
      [1] "Total patient-years at risk"
      
      $n_events
      [1] 4
      attr(,"label")
      [1] "Number of adverse events observed"
      
      $rate
      [1] 44.15823
      attr(,"label")
      [1] "AE rate per 100 patient-years"
      
      $rate_ci
      [1]  19.40154 100.50487
      attr(,"label")
      [1] "90% CI"
      

# estimate_incidence_rate works as expected with healthy input

    Code
      res
    Output
                                                A                 B       
                                              (N=3)             (N=3)     
      ————————————————————————————————————————————————————————————————————
      Total patient-years at risk              3.8               5.2      
      Number of adverse events observed         1                 3       
      AE rate per 100 patient-years           26.20             57.23     
      90% CI                              (5.06, 135.73)   (22.14, 147.94)

