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
      
      

# s_count_patients_with_flags custom variable label behaviour works

    Code
      res
    Output
      $n
      $n$SER
      [1] 164
      
      $n$REL
      [1] 164
      
      $n$CTC35
      [1] 164
      
      $n$CTC45
      [1] 164
      
      
      $count
      $count$SER
      [1] 128
      
      $count$REL
      [1] 137
      
      $count$CTC35
      [1] 134
      
      $count$CTC45
      [1] 104
      
      
      $count_fraction
      $count_fraction$SER
      [1] 128.0000000   0.7804878
      
      $count_fraction$REL
      [1] 137.0000000   0.8353659
      
      $count_fraction$CTC35
      [1] 134.0000000   0.8170732
      
      $count_fraction$CTC45
      [1] 104.0000000   0.6341463
      
      
      $n_blq
      $n_blq$SER
      [1] 0
      
      $n_blq$REL
      [1] 0
      
      $n_blq$CTC35
      [1] 0
      
      $n_blq$CTC45
      [1] 0
      
      

---

    Code
      res
    Output
      $n
      $n$`Serious AE`
      [1] 164
      
      $n$`Related AE`
      [1] 164
      
      $n$`Grade 3-5 AE`
      [1] 164
      
      $n$`Grade 4/5 AE`
      [1] 164
      
      
      $count
      $count$`Serious AE`
      [1] 128
      
      $count$`Related AE`
      [1] 137
      
      $count$`Grade 3-5 AE`
      [1] 134
      
      $count$`Grade 4/5 AE`
      [1] 104
      
      
      $count_fraction
      $count_fraction$`Serious AE`
      [1] 128.0000000   0.7804878
      
      $count_fraction$`Related AE`
      [1] 137.0000000   0.8353659
      
      $count_fraction$`Grade 3-5 AE`
      [1] 134.0000000   0.8170732
      
      $count_fraction$`Grade 4/5 AE`
      [1] 104.0000000   0.6341463
      
      
      $n_blq
      $n_blq$`Serious AE`
      [1] 0
      
      $n_blq$`Related AE`
      [1] 0
      
      $n_blq$`Grade 3-5 AE`
      [1] 0
      
      $n_blq$`Grade 4/5 AE`
      [1] 0
      
      

---

    Code
      res
    Output
      $n
      $n$`Category 1`
      [1] 164
      
      $n$`Category 2`
      [1] 164
      
      $n$`Category 3`
      [1] 164
      
      $n$`Category 4`
      [1] 164
      
      
      $count
      $count$`Category 1`
      [1] 128
      
      $count$`Category 2`
      [1] 137
      
      $count$`Category 3`
      [1] 134
      
      $count$`Category 4`
      [1] 104
      
      
      $count_fraction
      $count_fraction$`Category 1`
      [1] 128.0000000   0.7804878
      
      $count_fraction$`Category 2`
      [1] 137.0000000   0.8353659
      
      $count_fraction$`Category 3`
      [1] 134.0000000   0.8170732
      
      $count_fraction$`Category 4`
      [1] 104.0000000   0.6341463
      
      
      $n_blq
      $n_blq$`Category 1`
      [1] 0
      
      $n_blq$`Category 2`
      [1] 0
      
      $n_blq$`Category 3`
      [1] 0
      
      $n_blq$`Category 4`
      [1] 0
      
      

---

    Code
      res
    Output
      $n
      $n$`Serious AE`
      [1] 164
      
      $n$`Related AE`
      [1] 164
      
      $n$`Grade 3-5 AE`
      [1] 164
      
      $n$`Grade 4/5 AE`
      [1] 164
      
      
      $count
      $count$`Serious AE`
      [1] 128
      
      $count$`Related AE`
      [1] 137
      
      $count$`Grade 3-5 AE`
      [1] 134
      
      $count$`Grade 4/5 AE`
      [1] 104
      
      
      $count_fraction
      $count_fraction$`Serious AE`
      [1] 128.0000000   0.7804878
      
      $count_fraction$`Related AE`
      [1] 137.0000000   0.8353659
      
      $count_fraction$`Grade 3-5 AE`
      [1] 134.0000000   0.8170732
      
      $count_fraction$`Grade 4/5 AE`
      [1] 104.0000000   0.6341463
      
      
      $n_blq
      $n_blq$`Serious AE`
      [1] 0
      
      $n_blq$`Related AE`
      [1] 0
      
      $n_blq$`Grade 3-5 AE`
      [1] 0
      
      $n_blq$`Grade 4/5 AE`
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

# count_patients_with_flags custom variable label behaviour works with var_labels specified

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

# count_patients_with_flags works as expected with risk difference column

    Code
      res
    Output
                               A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                                 (N=69)       (N=73)         (N=58)                 (N=142)           
      ————————————————————————————————————————————————————————————————————————————————————————————————
      SAE                      53 (76.8%)   49 (67.1%)     39 (67.2%)          9.7 (-5.0 - 24.4)      
      SAE with fatal outcome   50 (72.5%)   47 (64.4%)     42 (72.4%)          8.1 (-7.1 - 23.3)      

---

    Code
      res
    Output
                               A: Drug X    B: Placebo   C: Combination   Risk Difference (%) (95% CI)
                                (N=202)      (N=177)        (N=162)                 (N=379)           
      ————————————————————————————————————————————————————————————————————————————————————————————————
      SAE                          53           49             39              -1.4 (-10.4 - 7.5)     
      SAE with fatal outcome       50           47             42              -1.8 (-10.6 - 7.0)     
      SAE                      53 (26.2%)   49 (27.7%)     39 (24.1%)          -1.4 (-10.4 - 7.5)     
      SAE with fatal outcome   50 (24.8%)   47 (26.6%)     42 (25.9%)          -1.8 (-10.6 - 7.0)     

