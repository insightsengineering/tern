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
      

# a_incidence_rate works with default arguments

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
            row_name   formatted_cell indent_mod
      1 person_years            108.7          0
      2     n_events                4          0
      3         rate           3.6799          0
      4      rate_ci (0.0737, 7.2860)          0
      5     n_unique                4          0
      6       n_rate          4 (3.7)          0
                                                                row_label
      1                                       Total patient-years at risk
      2                                 Number of adverse events observed
      3                                     AE rate per 100 patient-years
      4                                                            95% CI
      5          Total number of patients with at least one adverse event
      6 Number of adverse events observed (AE rate per 100 patient-years)

# a_incidence_rate works with customized arguments

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod
      1   n_rate   4.00 (44.16)          3
      2 n_unique           4.00          0
                                                       row_label
      1         Total number of applicable adverse events (rate)
      2 Total number of patients with at least one adverse event

# estimate_incidence_rate works as expected with default input

    Code
      res
    Output
                                                A               B       
                                              (N=3)           (N=3)     
      ——————————————————————————————————————————————————————————————————
      Total patient-years at risk             45.8             62.9     
      Number of adverse events observed         1               3       
      AE rate per 100 patient-years           2.18             4.77     
      95% CI                              (-2.10, 6.46)   (-0.63, 10.17)

# estimate_incidence_rate works as expected with custom input

    Code
      res
    Output
                                                                      A              B      
                                                                    (N=3)          (N=3)    
      ——————————————————————————————————————————————————————————————————————————————————————
            Total number of applicable adverse events (rate)     1.00 (26.20)   3.00 (57.23)
      Total number of patients with at least one adverse event       1.00           3.00    

# estimate_incidence_rate works with default arguments with summarize = TRUE

    Code
      res
    Output
                                                    A                B       
                                                  (N=3)            (N=3)     
      ———————————————————————————————————————————————————————————————————————
      X - Total patient-years at risk              10.1             39.5     
      X - Number of adverse events observed         1                2       
      X - AE rate per 100 patient-years            9.90             5.06     
      X - 95% CI                              (-9.50, 29.31)   (-1.95, 12.08)
      Y - Total patient-years at risk              35.7             23.4     
      Y - Number of adverse events observed         0                1       
      Y - AE rate per 100 patient-years            0.00             4.27     
      Y - 95% CI                               (0.00, 0.00)    (-4.10, 12.65)

# estimate_incidence_rate works with custom arguments with summarize = TRUE

    Code
      res
    Output
                                                                               A         B   
                                                                             (N=3)     (N=3) 
      ———————————————————————————————————————————————————————————————————————————————————————
      Total patient-years at risk                                            45.8      62.9  
      Number of adverse events observed                                        1         3   
      AE rate per 100 patient-years                                          2.18      4.77  
      X                                                                                      
        Total number of patients with at least one adverse event               1         2   
        Number of adverse events observed (AE rate per 100 patient-years)   1 (9.9)   2 (5.1)
      Y                                                                                      
        Total number of patients with at least one adverse event               0         1   
        Number of adverse events observed (AE rate per 100 patient-years)   0 (0.0)   1 (4.3)

