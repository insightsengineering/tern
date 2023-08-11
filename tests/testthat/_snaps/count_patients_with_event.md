# s_count_patients_with_event handles NA

    Code
      res
    Output
      $n
      [1] 2
      
      $count
      [1] 1
      
      $count_fraction
      [1] 1.0 0.5
      
      $n_blq
      [1] 0
      

# s_count_patients_with_event handles multiple columns

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 1
      
      $count_fraction
      [1] 1.0000000 0.3333333
      
      $n_blq
      [1] 0
      

# count_patients_with_event works as expected

    Code
      res
    Output
                                                                     A          B    
                                                                   (N=2)      (N=1)  
      ———————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event   1 (50.0%)   1 (100%)
      Total number of patients with fatal AEs                        0       1 (100%)

# count_patients_with_event works as expected for different column count

    Code
      res
    Output
                                                                     A           B    
                                                                   (N=6)       (N=4)  
      ————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event   1 (16.7%)   1 (25.0%)
      Total number of patients with fatal AEs                        0       1 (25.0%)

# s_count_patients_with_event works with factor filters

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 1
      
      $count_fraction
      [1] 1.0000000 0.3333333
      
      $n_blq
      [1] 0
      

# count_patients_with_flags works as expected with risk difference column

    Code
      res
    Output
                                                                 A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                                                                   (N=69)       (N=73)         (N=58)                 (N=142)           
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event   59 (85.5%)   57 (78.1%)     48 (82.8%)          7.4 (-5.2 - 20.0)      

---

    Code
      res
    Output
                       A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                        (N=202)      (N=177)        (N=162)                 (N=379)           
      ————————————————————————————————————————————————————————————————————————————————————————
      count                59           57             48              -3.0 (-12.3 - 6.3)     
      count_fraction   59 (29.2%)   57 (32.2%)     48 (29.6%)          -3.0 (-12.3 - 6.3)     

