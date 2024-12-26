# h_row_first_values works as expected

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
           A: Drug X     B: Placebo C: Combination 
                   1              4              1 

---

    Code
      res
    Output
          B: Placebo C: Combination 
                   4              1 

---

    Code
      res
    Output
           A: Drug X C: Combination 
                   1              1 

# h_row_counts works as expected

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
          B: Placebo C: Combination 
                   4              1 

# h_row_counts returns NA with empty analysis row

    Code
      res
    Output
                  A: Drug X   B: Placebo   C: Combination
      ———————————————————————————————————————————————————
      empty_row                                          

---

    Code
      res
    Output
      [1] NA NA NA

# h_row_fractions works as expected

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
          B: Placebo.p C: Combination.p 
            0.20000000       0.03225806 

# h_col_counts works as expected

    Code
      res
    Output
          B: Placebo C: Combination 
                 106            129 

