# s_count_patients_with_flags handles NA

    Code
      res
    Output
      $n
      $n$TRTEMFL
      [1] 2
      
      
      $count
      $count$TRTEMFL
      [1] 2
      
      
      $count_fraction
      $count_fraction$TRTEMFL
      [1] 2 1
      
      
      $n_blq
      $n_blq[[1]]
      [1] 0
      
      

# s_count_patients_with_flags handles multiple columns

    Code
      res
    Output
      $n
      $n$TRTEMFL
      [1] 3
      
      $n$AEOUTFL
      [1] 3
      
      
      $count
      $count$TRTEMFL
      [1] 3
      
      $count$AEOUTFL
      [1] 1
      
      
      $count_fraction
      $count_fraction$TRTEMFL
      [1] 3 1
      
      $count_fraction$AEOUTFL
      [1] 1.0000000 0.3333333
      
      
      $n_blq
      $n_blq$TRTEMFL
      [1] 0
      
      $n_blq$AEOUTFL
      [1] 0
      
      

# count_patients_with_flags works as expected

    Code
      res
    Output
                                                                     A           B    
                                                                   (N=6)       (N=4)  
      ————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event   1 (16.7%)   1 (25.0%)
      Total number of patients with fatal AEs                        0       1 (25.0%)

# count_patients_with_flags works as expected when specifying table_names

    Code
      res
    Output
                                                                     A           B    
                                                                   (N=6)       (N=4)  
      ————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event   1 (16.7%)   1 (25.0%)
      Total number of patients with fatal AEs                        0       1 (25.0%)
      Total number of patients with at least one adverse event   1 (16.7%)   1 (25.0%)
      Total number of patients with fatal AEs                        0       1 (25.0%)

# count_patients_with_flags works with label row specified

    Code
      res
    Output
                                                                 A: Drug X    B: Placebo   C: Combination
                                                                   (N=69)       (N=73)         (N=58)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event   59 (85.5%)   57 (78.1%)     48 (82.8%)  
      Total number of patients with at least one                                                         
        Serious AE                                               45 (65.2%)   46 (63.0%)     37 (63.8%)  
        Related AE                                               49 (71.0%)   48 (65.8%)     40 (69.0%)  
        Grade 3-5 AE                                             47 (68.1%)   46 (63.0%)     41 (70.7%)  
        Grade 4/5 AE                                             34 (49.3%)   38 (52.1%)     32 (55.2%)  

# Custom variable label behaviour works

    Code
      res
    Output
                                                   A: Drug X    B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one                                           
        SER                                        45 (65.2%)   46 (63.0%)     37 (63.8%)  
        REL                                        49 (71.0%)   48 (65.8%)     40 (69.0%)  
        CTC35                                      47 (68.1%)   46 (63.0%)     41 (70.7%)  
        CTC45                                      34 (49.3%)   38 (52.1%)     32 (55.2%)  

---

    Code
      res
    Output
                                                   A: Drug X    B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one                                           
        Serious AE                                 45 (65.2%)   46 (63.0%)     37 (63.8%)  
        Related AE                                 49 (71.0%)   48 (65.8%)     40 (69.0%)  
        Grade 3-5 AE                               47 (68.1%)   46 (63.0%)     41 (70.7%)  
        Grade 4/5 AE                               34 (49.3%)   38 (52.1%)     32 (55.2%)  

---

    Code
      res
    Output
                                                   A: Drug X    B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one                                           
        Category 1                                 45 (65.2%)   46 (63.0%)     37 (63.8%)  
        Category 2                                 49 (71.0%)   48 (65.8%)     40 (69.0%)  
        Category 3                                 47 (68.1%)   46 (63.0%)     41 (70.7%)  
        Category 4                                 34 (49.3%)   38 (52.1%)     32 (55.2%)  

---

    Code
      res
    Output
                                                   A: Drug X    B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one                                           
        Serious AE                                 45 (65.2%)   46 (63.0%)     37 (63.8%)  
        Related AE                                 49 (71.0%)   48 (65.8%)     40 (69.0%)  
        Grade 3-5 AE                               47 (68.1%)   46 (63.0%)     41 (70.7%)  
        Grade 4/5 AE                               34 (49.3%)   38 (52.1%)     32 (55.2%)  

