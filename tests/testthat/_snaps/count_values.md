# s_count_values works for character input without NAs

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          2 
      
      $count_fraction
          count  fraction 
      2.0000000 0.6666667 
      
      $count_fraction_fixed_dp
          count  fraction 
      2.0000000 0.6666667 
      
      $fraction
        num denom 
          2     3 
      
      $n_blq
      n_blq 
          0 
      

---

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          0 
      
      $count_fraction
         count fraction 
             0        0 
      
      $count_fraction_fixed_dp
         count fraction 
             0        0 
      
      $fraction
        num denom 
          0     3 
      
      $n_blq
      n_blq 
          0 
      

# s_count_values works for character input with NAs

    Code
      res
    Output
      $n
      n 
      5 
      
      $count
      count 
          2 
      
      $count_fraction
         count fraction 
           2.0      0.4 
      
      $count_fraction_fixed_dp
         count fraction 
           2.0      0.4 
      
      $fraction
        num denom 
          2     5 
      
      $n_blq
      n_blq 
          0 
      

---

    Code
      res
    Output
      $n
      n 
      6 
      
      $count
      count 
          1 
      
      $count_fraction
          count  fraction 
      1.0000000 0.1666667 
      
      $count_fraction_fixed_dp
          count  fraction 
      1.0000000 0.1666667 
      
      $fraction
        num denom 
          1     6 
      
      $n_blq
      n_blq 
          0 
      

# s_count_values can pass options to s_summary's logical method

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          2 
      
      $count_fraction
         count fraction 
           2.0      0.2 
      
      $count_fraction_fixed_dp
         count fraction 
           2.0      0.2 
      
      $fraction
        num denom 
          2    10 
      
      $n_blq
      n_blq 
          0 
      

# s_count_values for factor gives same result as for character

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          2 
      
      $count_fraction
         count fraction 
           2.0      0.2 
      
      $count_fraction_fixed_dp
         count fraction 
           2.0      0.2 
      
      $fraction
        num denom 
          2    10 
      
      $n_blq
      n_blq 
          0 
      

# s_count_values for factor gives the same result as for character for values not in factor level

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          0 
      
      $count_fraction
         count fraction 
             0        0 
      
      $count_fraction_fixed_dp
         count fraction 
             0        0 
      
      $fraction
        num denom 
          0     3 
      
      $n_blq
      n_blq 
          0 
      

# count_values works as expected with a single value

    Code
      res
    Output
                 all obs  
      ————————————————————
      setosa   50 (33.33%)

# count_values works as expected with multiple values and variables

    Code
      res
    Output
                all obs  
      ———————————————————
      x                  
        a, f   2 (50.00%)
      y                  
        a, f   3 (75.00%)

---

    Code
      res
    Output
              all obs  
      —————————————————
      TRUE   2 (50.00%)

# s_count_values for logical vector

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          2 
      
      $count_fraction
          count  fraction 
      2.0000000 0.6666667 
      
      $count_fraction_fixed_dp
          count  fraction 
      2.0000000 0.6666667 
      
      $fraction
        num denom 
          2     3 
      
      $n_blq
      n_blq 
          0 
      

# s_count_values for logical vector with NA

    Code
      res
    Output
      $n
      n 
      3 
      
      $count
      count 
          2 
      
      $count_fraction
          count  fraction 
      2.0000000 0.6666667 
      
      $count_fraction_fixed_dp
          count  fraction 
      2.0000000 0.6666667 
      
      $fraction
        num denom 
          2     3 
      
      $n_blq
      n_blq 
          0 
      

# count_values works with denom specified

    Code
      res
    Output
                 all obs  
      ————————————————————
      setosa   50 (33.33%)

---

    Code
      case1
    Output
           A: Drug X    B: Placebo    C: Combination
            (N=202)       (N=177)        (N=162)    
      ——————————————————————————————————————————————
      Y   80 (39.60%)   69 (38.98%)    65 (40.12%)  

---

    Code
      case2
    Output
           A: Drug X     B: Placebo    C: Combination
             (N=69)        (N=73)          (N=58)    
      ———————————————————————————————————————————————
      Y   80 (115.94%)   69 (94.52%)    65 (112.07%) 

---

    Code
      case3
    Output
           A: Drug X    B: Placebo    C: Combination
            (N=69)        (N=73)          (N=58)    
      ——————————————————————————————————————————————
      Y   80 (14.79%)   69 (12.75%)    65 (12.01%)  

---

    Code
      case4
    Output
           A: Drug X    B: Placebo    C: Combination
            (N=69)        (N=73)          (N=58)    
      ——————————————————————————————————————————————
      Y   80 (39.60%)   69 (38.98%)    65 (40.12%)  

