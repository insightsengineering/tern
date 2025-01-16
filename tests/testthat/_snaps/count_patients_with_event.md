# s_count_patients_with_event handles NA

    Code
      res
    Output
      $n
      n 
      2 
      
      $count
      count 
          1 
      
      $count_fraction
         count fraction 
           1.0      0.5 
      
      $count_fraction_fixed_dp
         count fraction 
           1.0      0.5 
      
      $fraction
        num denom 
          1     2 
      
      $n_blq
      n_blq 
          0 
      

# s_count_patients_with_event handles multiple columns

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          1 
      
      $count_fraction
          count  fraction 
      1.0000000 0.3333333 
      
      $count_fraction_fixed_dp
          count  fraction 
      1.0000000 0.3333333 
      
      $fraction
        num denom 
          1     3 
      
      $n_blq
      n_blq 
          0 
      

# a_count_patients_with_event works with healthy input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                       row_name formatted_cell indent_mod               row_label
      1                       n              3          0                       n
      2                   count              1          0                   count
      3          count_fraction      1 (33.3%)          0          count_fraction
      4 count_fraction_fixed_dp      1 (33.3%)          0 count_fraction_fixed_dp
      5                   n_blq              0          0                   n_blq

# a_count_patients_with_event works with custom input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
              row_name formatted_cell indent_mod row_label
      1 count_fraction     1 (33.33%)          1 New label
      2              n              3          3         n

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
      n 
      3 
      
      $count
      count 
          1 
      
      $count_fraction
          count  fraction 
      1.0000000 0.3333333 
      
      $count_fraction_fixed_dp
          count  fraction 
      1.0000000 0.3333333 
      
      $fraction
        num denom 
          1     3 
      
      $n_blq
      n_blq 
          0 
      

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

