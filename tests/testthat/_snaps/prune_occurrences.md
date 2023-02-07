# has_count_in_cols result performs comparisons correctly

    Code
      res
    Output
            A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————
      BRA   1 (3.7%)     4 (20%)        1 (3.2%)   

# has_count_in_any_col result performs comparisons correctly

    Code
      res
    Output
            A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————
      BRA   1 (3.7%)     4 (20%)        1 (3.2%)   

# has_fraction_in_cols result performs comparisons correctly

    Code
      res
    Output
            A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————
      BRA   1 (3.7%)     4 (20%)        1 (3.2%)   

---

    Code
      res
    Output
      [1] 121 106 129

# has_fraction_in_any_col result performs comparisons correctly

    Code
      res
    Output
            A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————
      BRA   1 (3.7%)     4 (20%)        1 (3.2%)   

# has_fractions_difference result performs comparisons correctly

    Code
      res
    Output
            A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————
      BRA   1 (3.7%)     4 (20%)        1 (3.2%)   

# has_counts_difference result performs comparisons correctly

    Code
      res
    Output
            A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————
      BRA   1 (3.7%)     4 (20%)        1 (3.2%)   

# combination of pruning functions works

    Code
      res
    Output
                A: Drug X    B: Placebo   C: Combination
      ——————————————————————————————————————————————————
      ASIAN                                             
        A       27 (22.3%)   20 (18.9%)     31 (24.0%)  
          CHN   14 (51.9%)    9 (45%)       12 (38.7%)  
        C       28 (23.1%)   19 (17.9%)     31 (24.0%)  
          RUS   4 (14.3%)    2 (10.5%)       1 (3.2%)   
      WHITE                                             
        B        7 (5.8%)     5 (4.7%)       4 (3.1%)   
          CHN   4 (57.1%)     1 (20%)        3 (75%)    

