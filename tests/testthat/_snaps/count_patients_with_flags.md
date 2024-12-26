# s_count_patients_with_flags handles NA

    Code
      res
    Output
      $n
      $n$TRTEMFL
      n 
      2 
      
      
      $count
      $count$TRTEMFL
      count 
          2 
      
      
      $count_fraction
      $count_fraction$TRTEMFL
         count fraction 
             2        1 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp[[1]]
         count fraction 
             2        1 
      
      
      $fraction
      $fraction[[1]]
        num denom 
          2     2 
      
      
      $n_blq
      $n_blq[[1]]
      n_blq 
          0 
      
      

# s_count_patients_with_flags handles multiple columns

    Code
      res
    Output
      $n
      $n$TRTEMFL
      n 
      3 
      
      $n$AEOUTFL
      n 
      3 
      
      
      $count
      $count$TRTEMFL
      count 
          3 
      
      $count$AEOUTFL
      count 
          1 
      
      
      $count_fraction
      $count_fraction$TRTEMFL
         count fraction 
             3        1 
      
      $count_fraction$AEOUTFL
          count  fraction 
      1.0000000 0.3333333 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$TRTEMFL
         count fraction 
             3        1 
      
      $count_fraction_fixed_dp$AEOUTFL
          count  fraction 
      1.0000000 0.3333333 
      
      
      $fraction
      $fraction$TRTEMFL
        num denom 
          3     3 
      
      $fraction$AEOUTFL
        num denom 
          1     3 
      
      
      $n_blq
      $n_blq$TRTEMFL
      n_blq 
          0 
      
      $n_blq$AEOUTFL
      n_blq 
          0 
      
      

# s_count_patients_with_flags custom variable label behaviour works

    Code
      res
    Output
      $n
      $n$SER
        n 
      164 
      
      $n$REL
        n 
      164 
      
      $n$CTC35
        n 
      164 
      
      $n$CTC45
        n 
      164 
      
      
      $count
      $count$SER
      count 
        128 
      
      $count$REL
      count 
        137 
      
      $count$CTC35
      count 
        134 
      
      $count$CTC45
      count 
        104 
      
      
      $count_fraction
      $count_fraction$SER
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction$REL
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction$CTC35
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction$CTC45
            count    fraction 
      104.0000000   0.6341463 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$SER
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction_fixed_dp$REL
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction_fixed_dp$CTC35
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction_fixed_dp$CTC45
            count    fraction 
      104.0000000   0.6341463 
      
      
      $fraction
      $fraction$SER
        num denom 
        128   164 
      
      $fraction$REL
        num denom 
        137   164 
      
      $fraction$CTC35
        num denom 
        134   164 
      
      $fraction$CTC45
        num denom 
        104   164 
      
      
      $n_blq
      $n_blq$SER
      n_blq 
          0 
      
      $n_blq$REL
      n_blq 
          0 
      
      $n_blq$CTC35
      n_blq 
          0 
      
      $n_blq$CTC45
      n_blq 
          0 
      
      

---

    Code
      res
    Output
      $n
      $n$`Serious AE`
        n 
      164 
      
      $n$`Related AE`
        n 
      164 
      
      $n$`Grade 3-5 AE`
        n 
      164 
      
      $n$`Grade 4/5 AE`
        n 
      164 
      
      
      $count
      $count$`Serious AE`
      count 
        128 
      
      $count$`Related AE`
      count 
        137 
      
      $count$`Grade 3-5 AE`
      count 
        134 
      
      $count$`Grade 4/5 AE`
      count 
        104 
      
      
      $count_fraction
      $count_fraction$`Serious AE`
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction$`Related AE`
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction$`Grade 3-5 AE`
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction$`Grade 4/5 AE`
            count    fraction 
      104.0000000   0.6341463 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Serious AE`
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction_fixed_dp$`Related AE`
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction_fixed_dp$`Grade 3-5 AE`
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction_fixed_dp$`Grade 4/5 AE`
            count    fraction 
      104.0000000   0.6341463 
      
      
      $fraction
      $fraction$`Serious AE`
        num denom 
        128   164 
      
      $fraction$`Related AE`
        num denom 
        137   164 
      
      $fraction$`Grade 3-5 AE`
        num denom 
        134   164 
      
      $fraction$`Grade 4/5 AE`
        num denom 
        104   164 
      
      
      $n_blq
      $n_blq$`Serious AE`
      n_blq 
          0 
      
      $n_blq$`Related AE`
      n_blq 
          0 
      
      $n_blq$`Grade 3-5 AE`
      n_blq 
          0 
      
      $n_blq$`Grade 4/5 AE`
      n_blq 
          0 
      
      

---

    Code
      res
    Output
      $n
      $n$`Category 1`
        n 
      164 
      
      $n$`Category 2`
        n 
      164 
      
      $n$`Category 3`
        n 
      164 
      
      $n$`Category 4`
        n 
      164 
      
      
      $count
      $count$`Category 1`
      count 
        128 
      
      $count$`Category 2`
      count 
        137 
      
      $count$`Category 3`
      count 
        134 
      
      $count$`Category 4`
      count 
        104 
      
      
      $count_fraction
      $count_fraction$`Category 1`
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction$`Category 2`
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction$`Category 3`
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction$`Category 4`
            count    fraction 
      104.0000000   0.6341463 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Category 1`
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction_fixed_dp$`Category 2`
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction_fixed_dp$`Category 3`
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction_fixed_dp$`Category 4`
            count    fraction 
      104.0000000   0.6341463 
      
      
      $fraction
      $fraction$`Category 1`
        num denom 
        128   164 
      
      $fraction$`Category 2`
        num denom 
        137   164 
      
      $fraction$`Category 3`
        num denom 
        134   164 
      
      $fraction$`Category 4`
        num denom 
        104   164 
      
      
      $n_blq
      $n_blq$`Category 1`
      n_blq 
          0 
      
      $n_blq$`Category 2`
      n_blq 
          0 
      
      $n_blq$`Category 3`
      n_blq 
          0 
      
      $n_blq$`Category 4`
      n_blq 
          0 
      
      

---

    Code
      res
    Output
      $n
      $n$`Serious AE`
        n 
      164 
      
      $n$`Related AE`
        n 
      164 
      
      $n$`Grade 3-5 AE`
        n 
      164 
      
      $n$`Grade 4/5 AE`
        n 
      164 
      
      
      $count
      $count$`Serious AE`
      count 
        128 
      
      $count$`Related AE`
      count 
        137 
      
      $count$`Grade 3-5 AE`
      count 
        134 
      
      $count$`Grade 4/5 AE`
      count 
        104 
      
      
      $count_fraction
      $count_fraction$`Serious AE`
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction$`Related AE`
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction$`Grade 3-5 AE`
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction$`Grade 4/5 AE`
            count    fraction 
      104.0000000   0.6341463 
      
      
      $count_fraction_fixed_dp
      $count_fraction_fixed_dp$`Serious AE`
            count    fraction 
      128.0000000   0.7804878 
      
      $count_fraction_fixed_dp$`Related AE`
            count    fraction 
      137.0000000   0.8353659 
      
      $count_fraction_fixed_dp$`Grade 3-5 AE`
            count    fraction 
      134.0000000   0.8170732 
      
      $count_fraction_fixed_dp$`Grade 4/5 AE`
            count    fraction 
      104.0000000   0.6341463 
      
      
      $fraction
      $fraction$`Serious AE`
        num denom 
        128   164 
      
      $fraction$`Related AE`
        num denom 
        137   164 
      
      $fraction$`Grade 3-5 AE`
        num denom 
        134   164 
      
      $fraction$`Grade 4/5 AE`
        num denom 
        104   164 
      
      
      $n_blq
      $n_blq$`Serious AE`
      n_blq 
          0 
      
      $n_blq$`Related AE`
      n_blq 
          0 
      
      $n_blq$`Grade 3-5 AE`
      n_blq 
          0 
      
      $n_blq$`Grade 4/5 AE`
      n_blq 
          0 
      
      

# a_count_patients_with_flags works with healthy input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                              row_name formatted_cell indent_mod    row_label
      1                          n.SER            164          0   Serious AE
      2                          n.REL            164          0   Related AE
      3                        n.CTC35            164          0 Grade 3-5 AE
      4                        n.CTC45            164          0 Grade 4/5 AE
      5                      count.SER            128          0   Serious AE
      6                      count.REL            137          0   Related AE
      7                    count.CTC35            134          0 Grade 3-5 AE
      8                    count.CTC45            104          0 Grade 4/5 AE
      9             count_fraction.SER      128 (78%)          0   Serious AE
      10            count_fraction.REL    137 (83.5%)          0   Related AE
      11          count_fraction.CTC35    134 (81.7%)          0 Grade 3-5 AE
      12          count_fraction.CTC45    104 (63.4%)          0 Grade 4/5 AE
      13   count_fraction_fixed_dp.SER    128 (78.0%)          0   Serious AE
      14   count_fraction_fixed_dp.REL    137 (83.5%)          0   Related AE
      15 count_fraction_fixed_dp.CTC35    134 (81.7%)          0 Grade 3-5 AE
      16 count_fraction_fixed_dp.CTC45    104 (63.4%)          0 Grade 4/5 AE
      17                     n_blq.SER              0          0   Serious AE
      18                     n_blq.REL              0          0   Related AE
      19                   n_blq.CTC35              0          0 Grade 3-5 AE
      20                   n_blq.CTC45              0          0 Grade 4/5 AE

# a_count_patients_with_flags works with custom input.

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                    row_name formatted_cell indent_mod    row_label
      1   count_fraction.SER   128 (78.05%)          2   Serious AE
      2   count_fraction.REL   137 (83.54%)          3   Related AE
      3 count_fraction.CTC35   134 (81.71%)          0 Grade 3-5 AE
      4 count_fraction.CTC45   104 (63.41%)          0 Grade 4/5 AE

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

# count_patients_with_flags works with single indent mod value

    Code
      res
    Output
                                                                           A           B    
                                                                         (N=6)       (N=4)  
      ——————————————————————————————————————————————————————————————————————————————————————
            Total number of patients with at least one adverse event   1 (16.7%)   1 (25.0%)
            Total number of patients with fatal AEs                        0       1 (25.0%)

