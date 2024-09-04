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
      
      $n_unique
      [1] 4
      attr(,"label")
      [1] "Total number of patients with at least one adverse event"
      
      $n_rate
      [1]  4.00000 44.15823
      attr(,"label")
      [1] "Number of adverse events observed (AE rate per 100 patient-years)"
      

# estimate_incidence_rate works as expected with healthy input

    Code
      res
    Output
                                                                                A                 B       
                                                                              (N=3)             (N=3)     
      ————————————————————————————————————————————————————————————————————————————————————————————————————
      Total patient-years at risk                                              3.8               5.2      
      Number of adverse events observed                                         1                 3       
      AE rate per 100 patient-years                                           26.20             57.23     
      90% CI                                                              (5.06, 135.73)   (22.14, 147.94)
      Total number of patients with at least one adverse event                  1                 3       
      Number of adverse events observed (AE rate per 100 patient-years)      1 (26.2)         3 (57.2)    

