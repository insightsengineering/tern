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
      

